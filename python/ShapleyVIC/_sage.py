import numpy as np
import pandas as pd
import sage
import warnings


def compute_sage_values(model, coef_vec, x_expl_dm, y_expl, outcome_type, 
                            x_groups=None, var_names=None, 
                            model_id=None, perf_metric=None, 
                            output_file=None, threshold=0.05):
    """Compute SAGE values for a regression model

        Parameters
        ----------
        model : sklearn.linear_model.LogisticRegression or \
            statsmodels.genmod.generalized_linear_model.GLM
            Optimal model to use as a skeleton. sklearn for binary outcome \
                (faster than statsmodels)
        coef_vec : numpy.array 
            Coefficients of the regression model
        x_expl_dm : pandas.DataFrame
            Features in explanation data after one-hot encoding
        y_expl : numpy.array or pandas.Series
            Outcome in explanation data
        outcome_type : string
            Type of outcome ('biinary' or 'ordinal')
        x_groups : list, optional (default: None)
            A list of integers indicating which columns belong to the same \
                categorical feature, if any
        var_names : list, optional (default: None)
            Features names
        model_id : int64, optional (default: None)
            Model ID to include in output Dataframe.
        perf_metric : float, optional (default: None)
            Model performance to include in output Dataframe.
        output_file : str, optional (default: None)
            csv file name to write output to.
        threshold : float, optional (default: 0.05)
            Threshold parameter for convergence. 
        
        Returns
        -------
        Returns a Dataframe with model ID, variable names, SAGE values and SD, \
            model performance.
    """
    if outcome_type == "binary":
        # use sklearn model
        model.intercept_ = np.array([coef_vec[0]])
        model.coef_ = np.array([coef_vec[1:]])
        f = lambda x: model.predict_proba(x)
        loss_type = 'cross entropy'
    elif outcome_type == "continuous":
        # use sklearn model, but no longer has predict_proba
        model.intercept_ = np.array([coef_vec[0]])
        model.coef_ = np.array([coef_vec[1:]])
        f = lambda x: model.predict(x)
        loss_type = 'mse'
    elif outcome_type == "ordinal":
        # ordinal, for now use statsmodel
        f = lambda x: model.model.predict(params=coef_vec, exog=x)
        loss_type = 'cross entropy'
    else:
        print("Other outcome_type not yet supported.\n")
        return None

    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("ignore")
        if x_groups is None:
            sage_imp = sage.MarginalImputer(f, x_expl_dm)
        else:
            sage_imp = sage.GroupedMarginalImputer(f, x_expl_dm, groups=x_groups)
    
    sage_est = sage.PermutationEstimator(sage_imp, loss_type)
    sage_val = sage_est(x_expl_dm.values, y_expl.values, bar=False, thresh=threshold)

    if var_names is None:
        var_names = ''
    if model_id is None:
        model_id = ''
    if perf_metric is None:
        perf_metric = ''
    sage_df = pd.DataFrame({
        'model_id':model_id, 
        'var_names':var_names,
        'sage_value_unadjusted':sage_val.values, 
        'sage_sd':sage_val.std, 
        'perf_metric':perf_metric
    })
    
    if output_file is not None:
        sage_df.to_csv(output_file)
    return sage_df
