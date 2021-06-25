import sage

def compute_sage_value_logit(model, X_test, Y_test, verbose = True):
    imputer = sage.MarginalImputer(model, X_test)
    estimator = sage.PermutationEstimator(imputer, 'cross entropy')
    sage_values = estimator(X_test, Y_test, verbose = verbose)
    return sage_values

