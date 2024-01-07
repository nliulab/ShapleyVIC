import numpy as np
from numpy.random import default_rng
import pandas as pd
from statsmodels.miscmodels.ordinal_model import OrderedModel
import statsmodels.api as sm
import os

from . import _util
from ._draw_models import *


class models:
    def __init__(self, x, y, output_dir, outcome_type="binary", ordinal_link="logit", criterion="loss", epsilon = 0.05, x_names_cat=None, save_data=True):
        """ Initialise models by training optimal model

            Parameters
            ----------
            x : pandas.DataFrame
                Features
            y : numpy.array or pandas.Series
                Outcome
            output_dir : str
                Folder to save all data and outputs to
            outcome_type : str, optional (default: "binary")
                Type of outcome. Also support "continuous" and "ordinal".
            ordinal_link : string, optional (default: "logit")
                Link function for ordinal regression
            criterion : str, optional (default: 'loss')
                Criterion for defining nearly optimal, which can be 'loss' (default, any outcome type), 'auc' (binary outcome only) or 'prauc' (binary outcome only). 'loss' defines nearly optimal as loss less than (1+epsilon) times the minimum loss. 'auc' and 'prauc' define nearly optimal as having AUC or PRAUC within the 95% confidence interval of the optimal model.
            epsilon : float, optional (default: 0.05)
                If criterion='loss', nearly optimal models are defined as models with loss \
                    less than (1+epsilon) times the minimum loss.
            x_names_cat : list, optional (default: None)
                Names of categorical features, if any
            save_data : bool, optional (default: True)
                Whether to save x and y to output_dir. If False, x and y \
                must be supplied separately in subsequent R analysis.

        """
        if criterion!='loss' and outcome_type!='binary':
            raise ValueError('For non-binary outcomes, only criterion="loss" is supported.')
        
        nan_in_x = x.isnull().sum().sum()
        if nan_in_x>0:
            raise ValueError('x contains NaN, which is not allowed')
        nan_in_y = y.isnull().values.any()
        if nan_in_y:
            raise ValueError('y contains NaN, which is not allowed')
        
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)
            
        x_dm, x_groups = _util.model_matrix(x=x, x_names_cat=x_names_cat)
        if outcome_type == "binary":
            x_with_constant = sm.add_constant(x_dm)
            m0 = sm.GLM(y, x_with_constant, family=sm.families.Binomial())
            m = m0.fit()
        elif outcome_type == "continuous":
            x_with_constant = sm.add_constant(x_dm)
            m0 = sm.OLS(y, x_with_constant)
            m = m0.fit()
        elif outcome_type == "ordinal":
            m0 = OrderedModel(y, x_dm, distr=ordinal_link)
            m = m0.fit(method='bfgs')
        else:
            raise ValueError('outcome type not yet supported')
        
        self.model_prefit = m0
        self.model_optim = m
        self.outcome_type = outcome_type
        self.x_groups = x_groups
        self.x_names_cat = x_names_cat
        self.x = x
        self.y = y
        self.output_dir = output_dir
        self.criterion = criterion
        self.epsilon = epsilon
        
        if save_data:
            x.to_csv(os.path.join(output_dir, 'x.csv'))
            y.to_csv(os.path.join(output_dir, 'y.csv'))
        
        return None


    # Following functions are agnostic to outcome types
    def draw_models(self, u1, u2, m = 800, n_final = 350, random_state = 1234):
        """Generate nearly optimal logistic regression models from Rashomon set

            Parameters
            ----------
            u1 : float
                Lower bound of a uniform distribution. Can initialise using \
                    ShapleyVIC.model.init_hyper_params
            u2 : float
                Upper bound of a uniform distribution. Can initialise using \
                    ShapleyVIC.model.init_hyper_params
            m : int64, optional (default: 800)
                Number of regression models to be generated around optimal model
            criterion : str, optional (default: 'loss')
                Criterion for defining nearly optimal, which can be 'loss' (default, any outcome type), 'auc' (binary outcome only) or 'prauc' (binary outcome only). 'loss' defines nearly optimal as loss less than (1+epsilon) times the minimum loss. 'auc' and 'prauc' define nearly optimal as having AUC or PRAUC within the 95% confidence interval based on the optimal model.
            n_final: int64, optional (default: 350)
                Final number of nearly optimal models to select
            random_state : int64, optional (default: 1234)
                Random seed

        """

        if self.criterion=="loss":
            print(f"Nearly optimal defined based on {self.criterion} with epsilon={self.epsilon}.\n")
        else:
            print(f"Nearly optimal defined based on {self.criterion}.\n")

        coef_gen = draw_models_initial(
            coef_optim=self.model_optim.params, 
            coef_optim_var=self.model_optim.cov_params(), 
            m=m, u1=u1, u2=u2, random_state=random_state
        )
        if self.criterion == 'loss':
            coef_elg, perf_metric_optim = mark_elig_loss(
                coef_df=coef_gen, coef_optim=self.model_optim.params,
                loss_func=self.model_prefit.loglike, epsilon=self.epsilon
            )
        elif self.criterion == 'auc':
            x_dm, x_groups = _util.model_matrix(
                x=self.x, x_names_cat=self.x_names_cat
            )
            x_with_constant = sm.add_constant(x_dm)
            coef_elg, perf_metric_optim = mark_elig_auc(
                coef_df=coef_gen, coef_optim=self.model_optim.params, 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y
            )
        elif self.criterion == 'prauc':
            x_dm, x_groups = _util.model_matrix(
                x=self.x, x_names_cat=self.x_names_cat
            )
            x_with_constant = sm.add_constant(x_dm)
            coef_elg, perf_metric_optim = mark_elig_prauc(
                coef_df=coef_gen, coef_optim=self.model_optim.params, 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y
            )
        else:
            raise ValueError('Other criterion not yet supported')
        
        if n_final is not None:
            if np.sum(coef_elg['eligible']) <= n_final:
                print('Not enough sampled models are eligible. \
                    All eligible are selected.')
                select = np.where(coef_elg['eligible'] == True)[0]
            else:
                df_rng = default_rng(seed=random_state)
                select = np.where(coef_elg['eligible'] == True)[0]
                select = df_rng.choice(select, n_final, replace=False)
            if self.criterion == 'loss':
                n_points = int(self.epsilon/0.005)+1
                x_breaks = np.linspace(1,1+self.epsilon, n_points)
                plot = plot_perf_metric(
                    perf_metric=coef_elg['perf_metric'], 
                    eligible=coef_elg['eligible'],
                    x_range=[1, 1+self.epsilon], select=select,
                    plot_selected=True, x_breaks=x_breaks
                )
            elif self.criterion == 'auc' or self.criterion == 'prauc':
                n_points = 11
                perf_metric_upper = np.max([np.max(coef_elg['perf_metric']), 
                                            perf_metric_optim[2]])
                x_breaks = np.linspace(
                    perf_metric_optim[1],perf_metric_upper, n_points
                )
                plot = plot_perf_metric(
                    perf_metric=coef_elg['perf_metric'], 
                    eligible=coef_elg['eligible'],
                    x_range=[perf_metric_optim[1], perf_metric_optim[2]], 
                    select=select, plot_selected=True, x_breaks=x_breaks
                )
            else:
                raise ValueError('Other criterion not yet supported')
            select_array = np.zeros(len(coef_elg), dtype=bool)
            select_array[select] = True
            coef_elg['selected'] = select_array
            coef_elg = coef_elg.loc[coef_elg['selected']==True]
            coef_elg = coef_elg.drop(columns=['eligible','selected'])
            coef_elg.reset_index(drop = True, inplace=True)
        else:
            plot = plot_perf_metric(perf_metric=coef_elg['perf_metric'], 
                                    eligible=coef_elg['eligible'],
                                    x_range=[1, 1+self.epsilon])
        
        self.models_near_optim = coef_elg
        self.perf_metric_optim = perf_metric_optim
        self.models_plot = plot
        coef_elg.to_csv(os.path.join(self.output_dir, 'models_near_optim.csv'))
        return None


    def init_hyper_params(self, m = 400, ratio_range=[0.725, 0.775], 
                            epsilon = 0.05):
        """Suggest initial values for hyper-parameters, u1 and u2, \
            to achieve reasonable proportion of eligible models

            Parameters
            ----------
            m : int64, optional (default: 400)
                Number of models to draw for determining u1 and u2 values
            ratio_range : list, optional (default: [0.725, 0.775])
                Range of desirable proportional of eligible models in all m \
                    sampled models
            epsilon : float, optional (default: 0.05)
                Nearly optimal models are defined as models with loss \
                    less than (1+epsilon) times the minimum loss.
            
            Returns
            -------
            (u1, u2) Returns a tuple of reasonable values for u1 and u2.
        """
        random_state = 1234

        k = 10
        
        if self.criterion=="loss":
            print(f"Nearly optimal defined based on {self.criterion} with epsilon={self.epsilon}.\n")
        else:
            print(f"Nearly optimal defined based on {self.criterion}.\n")
        
        df = draw_models_initial(
            coef_optim=self.model_optim.params, 
            coef_optim_var=self.model_optim.cov_params(), 
            m=m, u1=0, u2=1, random_state=random_state
        )
        
        if self.criterion == 'loss':
            df, perf_metric_optim = mark_elig_loss(
                coef_df=df, coef_optim=self.model_optim.params,
                loss_func=self.model_prefit.loglike, epsilon=epsilon
            )
        elif self.criterion == 'auc':
            x_dm, x_groups = _util.model_matrix(
                x=self.x, x_names_cat=self.x_names_cat
            )
            x_with_constant = sm.add_constant(x_dm)
            df, perf_metric_optim = mark_elig_auc(
                coef_df=df, coef_optim=self.model_optim.params, 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y
            )
        elif self.criterion == 'prauc':
            x_dm, x_groups = _util.model_matrix(
                x=self.x, x_names_cat=self.x_names_cat
            )
            x_with_constant = sm.add_constant(x_dm)
            df, perf_metric_optim = mark_elig_prauc(
                coef_df=df, coef_optim=self.model_optim.params, 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y
            )
        else:
            raise ValueError('Other criterion not yet supported')

        ratio = np.sum(df['eligible']) / m

        if ratio < min(ratio_range):
            u2_max = 1
            u2_min = None
        else:
            u2_max = None
            u2_min = 1
        
        if self.criterion == 'loss':
            u2 = find_u2_loss(
                u2_min=u2_min, u2_max=u2_max, 
                coef_optim=self.model_optim.params, 
                coef_optim_var=self.model_optim.cov_params(), 
                loss_func=self.model_prefit.loglike, 
                m=m, k=k, ratio_range=ratio_range, epsilon=epsilon,
                random_state=random_state
            )
        elif self.criterion == 'auc':
            u2 = find_u2_auc(
                u2_min=u2_min, u2_max=u2_max, 
                coef_optim=self.model_optim.params, 
                coef_optim_var=self.model_optim.cov_params(), 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y, 
                m=m, k=k, ratio_range=ratio_range,
                random_state=random_state
            )
        elif self.criterion == 'prauc':
            u2 = find_u2_prauc(
                u2_min=u2_min, u2_max=u2_max, 
                coef_optim=self.model_optim.params, 
                coef_optim_var=self.model_optim.cov_params(), 
                pred_func=self.model_optim.model.predict, 
                x_with_constant=x_with_constant, y=self.y, 
                m=m, k=k, ratio_range=ratio_range,
                random_state=random_state
            )
        else:
            raise ValueError('Other criterion not yet supported')
        
        if u2 > 10:
            u1 = 0.5
        else:
            u1 = 0

        return (u1, u2)
