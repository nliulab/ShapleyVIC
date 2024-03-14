import numpy as np
from numpy.random import default_rng
import pandas as pd
from functools import partial
import multiprocessing as mp
import plotnine as pn

from . import _util
# This script is agnostic to outcome type

def get_sub_matrix(matrix, indices):
    m1 = matrix[indices, :]
    m2 = m1[:, indices]
    return m2


def draw_models_initial(coef_optim, coef_optim_var, u1, u2, m = 800, 
                            random_state = 1234):
    """ Generate m regression models centered around the optimal model

        Parameters
        ----------
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        coef_optim_var : pandas.DataFrame from statsmodel
            Variance-covariance matrix of the optimal model fitted by statsmodels, \
                usually obtain by result.cov_params()
        u1 : float
            Lower bound of a uniform distribution
        u2 : float
            Upper bound of a uniform distribution
        m : int64, optional (default: 800)
            Number of regression models to be generated around the optimal model
        random_state : int64, optional (default: 1234)
            Random seed

        Returns
        -------
        coef_mat : pandas.DataFrame
            Generated model coefficients
    """

    df_rng = default_rng(seed=random_state)
    coef_names = coef_optim.index.values
    # muln_with_mu = partial(np.random.multivariate_normal, coef_optim.values)
    muln_with_mu = partial(df_rng.multivariate_normal, coef_optim.values)
    # k_vec = np.random.uniform(u1, u2, m)
    k_vec = df_rng.uniform(u1, u2, m)

    n_cpu = max(mp.cpu_count()-2, 2) # prevent full load on all threads
    with mp.Pool(processes = n_cpu) as pool:
        coef_list = pool.map(muln_with_mu, [coef_optim_var*k for k in k_vec])
    coef_mat = pd.DataFrame(np.array(coef_list), columns=coef_names)

    return coef_mat


def mark_elig_loss(coef_df, coef_optim, loss_func, epsilon = 0.05):
    """ Mark whether newly generated models are eligible based on loss

        Parameters
        ----------
        coef_df : pandas.DataFrame
            Generated model coefficients 
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        loss_func
            Loss (or log-likelihood) function that only takes coefficients as \
                input, usually obtain from statsmodels (.loglike) before .fit()
        epsilon : float, optional (default: 0.05)
            Nearly optimal models are defined as models with logistic loss \
                less than (1+epsilon) times the minimum loss.

        Returns
        -------
        result_df : pandas.DataFrame
            Generated model coefficients with eligibility markings columns
    """

    #loss_with_xy = partial(m0.loglike)
    result_df = coef_df.copy(deep = True)
    loss_optim = loss_func(coef_optim)
    n_cpu = max(mp.cpu_count()-2, 2) # prevent full load on all threads
    with mp.Pool(processes = n_cpu) as pool:
        loss_list = pool.map(
            loss_func, 
            [coef_df.iloc[i] for i in range(len(coef_df))]
        )
    loss_vec = np.array(loss_list)
    if loss_optim < 0:
        # binary outcome
        result_df['perf_metric'] = loss_vec/loss_optim
    else:
        # continuous outcome
        result_df['perf_metric'] = loss_optim/loss_vec
    
    result_df['eligible'] = result_df['perf_metric'] < (1+epsilon)

    return result_df, -loss_optim


def eval_coef_auc(coef_vec, pred_func, x_with_constant, y):
    p = pred_func(params = coef_vec, exog = x_with_constant)
    return(_util.roc_auc_ci(y_true=y, y_score=p, positive=1)[0])


def eval_coef_prauc(coef_vec, pred_func, x_with_constant, y):
    p = pred_func(params = coef_vec, exog = x_with_constant)
    return(_util.prc_auc_ci(y_true=y, y_score=p, positive=1)[0])


def mark_elig_auc(coef_df, coef_optim, pred_func, x_with_constant, y):
    """ Mark whether newly generated models are eligible based on AUC

        Parameters
        ----------
        coef_df : pandas.DataFrame
            Generated model coefficients 
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        pred_func
            Prediction function that takes coefficients and x as \
                input, usually obtain from statsmodels (.predict)
        x : pandas.DataFrame
            Features
        y : numpy.array or pandas.Series
            Outcome

        Returns
        -------
        result_df : pandas.DataFrame
            Generated model coefficients with eligibility markings columns
    """

    #loss_with_xy = partial(m0.loglike)
    result_df = coef_df.copy(deep = True)
    p_optim = pred_func(params = coef_optim, exog = x_with_constant)
    auc_optim_vec = _util.roc_auc_ci(y_true=y, y_score=p_optim, positive=1)
    eval_with_dat = partial(
        eval_coef_auc, pred_func=pred_func, x_with_constant=x_with_constant, y=y
    )
    
    n_cpu = max(mp.cpu_count()-2, 2) # prevent full load on all threads
    with mp.Pool(processes = n_cpu) as pool:
        auc_list = pool.map(
            eval_with_dat, 
            [coef_df.iloc[i] for i in range(len(coef_df))]
        )
    auc_vec = np.array(auc_list)
    result_df['perf_metric'] = auc_vec
    result_df['eligible'] = result_df['perf_metric'] > auc_optim_vec[1]
    return result_df, auc_optim_vec


def mark_elig_prauc(coef_df, coef_optim, pred_func, x_with_constant, y):
    """ Mark whether newly generated models are eligible based on AUC

        Parameters
        ----------
        coef_df : pandas.DataFrame
            Generated model coefficients 
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        pred_func
            Prediction function that takes coefficients and x as \
                input, usually obtain from statsmodels (.predict)
        x : pandas.DataFrame
            Features
        y : numpy.array or pandas.Series
            Outcome

        Returns
        -------
        result_df : pandas.DataFrame
            Generated model coefficients with eligibility markings columns
    """

    #loss_with_xy = partial(m0.loglike)
    result_df = coef_df.copy(deep = True)
    p_optim = pred_func(params = coef_optim, exog = x_with_constant)
    auc_optim_vec = _util.prc_auc_ci(y_true=y, y_score=p_optim, positive=1)
    eval_with_dat = partial(
        eval_coef_prauc, pred_func=pred_func, x_with_constant=x_with_constant, y=y
    )
    
    n_cpu = max(mp.cpu_count()-2, 2) # prevent full load on all threads
    with mp.Pool(processes = n_cpu) as pool:
        auc_list = pool.map(
            eval_with_dat, 
            [coef_df.iloc[i] for i in range(len(coef_df))]
        )
    auc_vec = np.array(auc_list)
    result_df['perf_metric'] = auc_vec
    result_df['eligible'] = result_df['perf_metric'] > auc_optim_vec[1]
    return result_df, auc_optim_vec


def plot_perf_metric(perf_metric, eligible, x_range, select = None, 
                        plot_selected = False, x_breaks = None):
    """ Plot performance metrics of sampled models

        Parameters
        ----------
        perf_metric : numpy.array or pandas.Series
            Numeric vector of performance metrics for all sampled models
        eligible : numpy.array or pandas.Series
            Boolean vector of the same length of 'perf_metric', indicating \
                whether each sample is eligible.
        x_range : list
            Numeric vector indicating the range of eligible values for \
                performance metrics. 
            Will be indicated by dotted vertical lines in plots.
        select : list or numpy.array, optional (default: None)
            Numeric vector of indexes of 'perf_metric' to be selected
        plot_selected : bool, optional (default: False)
            Whether performance metrics of selected models should be plotted in \
                a secondary figure.
        x_breaks : list, optional (default: None)
            If selected models are to be plotted, the breaks to use in the \
                histogram

        Returns
        -------
        plot : plotnine.ggplot
            Histogram(s) of model performance made using ggplot
    """
    m = len(perf_metric)
    perf_df = pd.DataFrame(perf_metric, columns = ['perf_metric'], index = None)
    plot = pn.ggplot(perf_df, pn.aes(x = 'perf_metric')) + \
        pn.geoms.geom_histogram(breaks = np.linspace(np.min(perf_metric), np.max(perf_metric), 40)) + \
        pn.geoms.geom_vline(xintercept = x_range, linetype = 'dashed', size = 0.7) + \
        pn.labels.labs(x = "Ratio of loss to minimum loss",
            title = """Loss of {m:d} sampled models
                \n{n_elg:d} ({per_elg:.1f}%) sampled models are eligible""".format(
                    m = m, n_elg = np.sum(eligible), 
                    per_elg = np.sum(eligible)*100/m
            )
        ) + \
        pn.themes.theme_bw() + \
        pn.themes.theme(title=pn.element_text(ha = 'left'),
            axis_title_x= pn.element_text(ha = 'center'),
            axis_title_y= pn.element_text(ha = 'center'))
    if plot_selected:
        if select is None:
            print('\'select\' vector is not specified!\nUsing all models instead')
            select = [i for i in range(len(perf_df))]
        try:
            perf_select = perf_df.iloc[select]
        except:
            print('Invalid indexes detected in \'select\' vector!\nUsing all models instead')
            select = [i for i in range(len(perf_df))]
            perf_select = perf_df.iloc[select]
        plot2 = pn.ggplot(perf_select, pn.aes(x = 'perf_metric')) + \
            pn.geoms.geom_histogram(breaks = x_breaks) + \
            pn.labels.labs(x = "Ratio of loss to minimum loss",
                title = "{n_select:d} selected models".format(n_select = len(select))) + \
            pn.themes.theme_bw() + \
            pn.themes.theme(title=pn.element_text(ha = 'left'),
                axis_title_x= pn.element_text(ha = 'center'),
                axis_title_y= pn.element_text(ha = 'center'))
        return (plot,plot2)
    else:
        return plot


def find_u2_loss(u2_min, u2_max, coef_optim, coef_optim_var, loss_func, 
                    m, k, ratio_range, epsilon=0.05, random_state = 1234):
    """Recursively find a reasonable value for u2

        Parameters
        ----------
        u2_min : float
            A value below which u2 is too small to achieve target eligible ratio.
        u2_max : float
            A value above which u2 is too small to achieve target eligible ratio.
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        coef_optim_var : pandas.DataFrame from statsmodel
            Variance-covariance matrix of the optimal model fitted by statsmodels, \
                usually obtain by result.cov_params()
        loss_func
            Loss (or log-likelihood) function that only takes coefficients as \
                input
        m : int
            Number of regression models to be generated around the optimal model
        k : int
            A factor to increment the value of u2 by when searching for reasonable value of u2.
        ratio_range : list
            Range of eligible ratio. 
        criterion : string, optional (default: 'loss')
            Criterion for determining model eligibility. Currently only \
                supports 'loss'
        epsilon : float, optional (default: 0.05)
            Nearly optimal models are defined as models with logistic loss \
                less than (1+epsilon) times the minimum loss.
        random_state : int64, optional (default: 1234)
            Random seed

        Returns
        -------
        u2: float
            A reasonable value for u2.
    """
    if u2_min is None:
        u2 = u2_max / k
    if u2_max is None:
        u2 = u2_min * k
    if u2_min is not None and u2_max is not None:
        u2 = (u2_min + u2_max) / 2

    if u2 > 10:
        u1 = 0.5
    else:
        u1 = 0

    df = draw_models_initial(
        coef_optim=coef_optim, coef_optim_var=coef_optim_var, 
        m=m, u1=u1, u2=u2, random_state=random_state
    )
    df, perf_metric_optim = mark_elig_loss(
        coef_df=df, coef_optim=coef_optim, loss_func=loss_func, epsilon=epsilon
    )
    ratio = np.sum(df['eligible']) / m

    if ratio >= max(ratio_range):
        # current u2 too small
        return find_u2_loss(u2_min=u2, u2_max=u2_max, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, loss_func=loss_func, m=m, k=k, \
                ratio_range=ratio_range, epsilon=epsilon, random_state=random_state)
    elif ratio <= min(ratio_range):
        # current u2 too large
        return find_u2_loss(u2_min=u2_min, u2_max=u2, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, loss_func=loss_func, m=m, k=k, \
                ratio_range=ratio_range, epsilon=epsilon, random_state=random_state)
    else:
        # current u2 is ok
        return u2


def find_u2_auc(u2_min, u2_max, coef_optim, coef_optim_var, 
                    pred_func, x_with_constant, y,
                    m, k, ratio_range, random_state = 1234):
    """Recursively find a reasonable value for u2

        Parameters
        ----------
        u2_min : float
            A value below which u2 is too small to achieve target eligible ratio.
        u2_max : float
            A value above which u2 is too small to achieve target eligible ratio.
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        coef_optim_var : pandas.DataFrame from statsmodel
            Variance-covariance matrix of the optimal model fitted by statsmodels, \
                usually obtain by result.cov_params()
        loss_func
            Loss (or log-likelihood) function that only takes coefficients as \
                input
        m : int
            Number of regression models to be generated around the optimal model
        k : int
            A factor to increment the value of u2 by when searching for reasonable value of u2.
        ratio_range : list
            Range of eligible ratio. 
        criterion : string, optional (default: 'loss')
            Criterion for determining model eligibility. Currently only \
                supports 'loss'
        epsilon : float, optional (default: 0.05)
            Nearly optimal models are defined as models with logistic loss \
                less than (1+epsilon) times the minimum loss.
        random_state : int64, optional (default: 1234)
            Random seed

        Returns
        -------
        u2: float
            A reasonable value for u2.
    """
    if u2_min is None:
        u2 = u2_max / k
    if u2_max is None:
        u2 = u2_min * k
    if u2_min is not None and u2_max is not None:
        u2 = (u2_min + u2_max) / 2

    if u2 > 10:
        u1 = 0.5
    else:
        u1 = 0

    df = draw_models_initial(
        coef_optim=coef_optim, coef_optim_var=coef_optim_var, 
        m=m, u1=u1, u2=u2, random_state=random_state
    )
    df, perf_metric_optim = mark_elig_auc(
        coef_df=df, coef_optim=coef_optim, pred_func=pred_func, 
        x_with_constant=x_with_constant, y=y
    )
    ratio = np.sum(df['eligible']) / m

    if ratio >= max(ratio_range):
        # current u2 too small
        return find_u2_auc(u2_min=u2, u2_max=u2_max, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, pred_func=pred_func, \
                x_with_constant=x_with_constant, y=y, m=m, k=k, \
                    ratio_range=ratio_range, random_state=random_state)
    elif ratio <= min(ratio_range):
        # current u2 too large
        return find_u2_auc(u2_min=u2_min, u2_max=u2, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, pred_func=pred_func, \
                x_with_constant=x_with_constant, y=y, m=m, k=k, \
                    ratio_range=ratio_range, random_state=random_state)
    else:
        # current u2 is ok
        return u2


def find_u2_prauc(u2_min, u2_max, coef_optim, coef_optim_var, 
                    pred_func, x_with_constant, y,
                    m, k, ratio_range, random_state = 1234):
    """Recursively find a reasonable value for u2

        Parameters
        ----------
        u2_min : float
            A value below which u2 is too small to achieve target eligible ratio.
        u2_max : float
            A value above which u2 is too small to achieve target eligible ratio.
        coef_optim : pandas.Series from statsmodel
            Coefficients vector of the optimal model fitted by statsmodels, \
                usually obtain by result.params
        coef_optim_var : pandas.DataFrame from statsmodel
            Variance-covariance matrix of the optimal model fitted by statsmodels, \
                usually obtain by result.cov_params()
        loss_func
            Loss (or log-likelihood) function that only takes coefficients as \
                input
        m : int
            Number of regression models to be generated around the optimal model
        k : int
            A factor to increment the value of u2 by when searching for reasonable value of u2.
        ratio_range : list
            Range of eligible ratio. 
        criterion : string, optional (default: 'loss')
            Criterion for determining model eligibility. Currently only \
                supports 'loss'
        epsilon : float, optional (default: 0.05)
            Nearly optimal models are defined as models with logistic loss \
                less than (1+epsilon) times the minimum loss.
        random_state : int64, optional (default: 1234)
            Random seed

        Returns
        -------
        u2: float
            A reasonable value for u2.
    """
    if u2_min is None:
        u2 = u2_max / k
    if u2_max is None:
        u2 = u2_min * k
    if u2_min is not None and u2_max is not None:
        u2 = (u2_min + u2_max) / 2

    if u2 > 10:
        u1 = 0.5
    else:
        u1 = 0

    df = draw_models_initial(
        coef_optim=coef_optim, coef_optim_var=coef_optim_var, 
        m=m, u1=u1, u2=u2, random_state=random_state
    )
    df, perf_metric_optim = mark_elig_prauc(
        coef_df=df, coef_optim=coef_optim, pred_func=pred_func, 
        x_with_constant=x_with_constant, y=y
    )
    ratio = np.sum(df['eligible']) / m

    if ratio >= max(ratio_range):
        # current u2 too small
        return find_u2_prauc(u2_min=u2, u2_max=u2_max, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, pred_func=pred_func, \
                x_with_constant=x_with_constant, y=y, m=m, k=k, \
                    ratio_range=ratio_range, random_state=random_state)
    elif ratio <= min(ratio_range):
        # current u2 too large
        return find_u2_prauc(u2_min=u2_min, u2_max=u2, coef_optim=coef_optim, \
            coef_optim_var=coef_optim_var, pred_func=pred_func, \
                x_with_constant=x_with_constant, y=y, m=m, k=k, \
                    ratio_range=ratio_range, random_state=random_state)
    else:
        # current u2 is ok
        return u2
