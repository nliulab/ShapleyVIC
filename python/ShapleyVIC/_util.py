import pandas as pd


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
                    drop_first=True)
                j_new = j + xi_dm.shape[1]
            else:
                xi_dm = pd.DataFrame(x[x_names[i]])
                j_new = j + 1
            x_group.append(list(range(j, j_new)))
            x_dm_ls.append(xi_dm)
            j = j_new
        x_dm = pd.concat(x_dm_ls, axis=1)
    return x_dm, x_group
