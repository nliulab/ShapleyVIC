import pandas as pd
import numpy as np
from sklearn.metrics import roc_auc_score, precision_recall_curve, auc
from math import sqrt, log, exp
from functools import partial
import multiprocessing as mp


def model_matrix(x, x_names_cat=None):
    """ Apply one-hot encoding given names of categorical features

        Parameters
        ----------
        x : pandas.DataFrame
            Named DataFrame of Features
        x_names_cat : list, optional (default: None)
            Names of categorical features, if any

        Returns
        -------
        x_dm : pandas.DataFrame
            Features after one-hot encoding
        x_group : list
            A list of integers indicating which columns belong to the same \
                categorical feature
    """
    n = x.shape[1]
    if x_names_cat is None:
        # All are continuous. No need to convert
        # x_group = [[i] for i in range(n)]
        x_group = None
        x_dm = x
    else:
        x_dm_ls = []
        x_group = []
        j = 0
        x_names = x.columns.values
        for i in range(n):
            if x_names[i] in x_names_cat:
                # x_i is categorical
                xi_dm = pd.get_dummies(x[x_names[i]], prefix=x_names[i], 
                    drop_first=True, dtype=int)
                j_new = j + xi_dm.shape[1]
            else:
                xi_dm = pd.DataFrame(x[x_names[i]])
                j_new = j + 1
            x_group.append(list(range(j, j_new)))
            x_dm_ls.append(xi_dm)
            j = j_new
        x_dm = pd.concat(x_dm_ls, axis=1)
    return x_dm, x_group


def roc_auc_ci(y_true, y_score, positive=1):
    AUC = roc_auc_score(y_true, y_score)
    N1 = sum(y_true == positive)
    N2 = sum(y_true != positive)
    Q1 = AUC / (2 - AUC)
    Q2 = 2*AUC**2 / (1 + AUC)
    SE_AUC = sqrt((AUC*(1 - AUC) + (N1 - 1)*(Q1 - AUC**2) + (N2 - 1)*(Q2 - AUC**2)) / (N1*N2))
    lower = AUC - 1.96*SE_AUC
    upper = AUC + 1.96*SE_AUC
    if lower < 0:
        lower = 0
    if upper > 1:
        upper = 1
    return([AUC, lower, upper])
    # return (AUC, lower, upper)


def ci_logit_interval(theta, n):
    # https://pages.cs.wisc.edu/~boyd/aucpr_final.pdf
    eta = log(theta / (1 - theta))
    tau = 1 / sqrt(n * theta * (1 - theta))
    phi_alpha = 1.96
    eta_lower = exp(eta - phi_alpha * tau)
    eta_upper = exp(eta + phi_alpha * tau)
    lower = eta_lower / (1 + eta_lower)
    upper = eta_upper / (1 + eta_upper)
    return([theta, lower, upper])


def prc_auc_ci(y_true, y_score, positive=1):
    precision, recall, thresholds = precision_recall_curve(y_true, y_score)
    prauc = auc(recall, precision)
    return(ci_logit_interval(prauc, len(y_true)))