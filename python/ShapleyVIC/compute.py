import pandas as pd
import os
import sys
import time
import multiprocessing as mp
from tqdm import tqdm
from tqdm_joblib import tqdm_joblib
from joblib import Parallel, delayed
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression

from . import _sage
from . import _util


def compute_shapley_vic(model_obj, x_expl, y_expl, n_cores, threshold=0.05):
    """Compute ShapleyVIC values for all nearly optimal models

        Parameters
        ---------- 
        model_obj : ShapleyVIC.model.models
            Object created using ShapleyVIC.model.models
        x_expl : pandas.DataFrame
            Features in explanation data
        y_expl : numpy.array or pandas.Series
            Outcome in explanation data
        n_cores : int
            Number of cores to use. At most maximum number of cores minus 1
        threshold : float, optional (default: 0.05)
            Threshold parameter for convergence. 
        
        Returns
        -------
        Returns None.
    """
    nan_in_x = x_expl.isnull().sum().sum()
    if nan_in_x>0:
        raise ValueError('x_expl contains NaN, which is not allowed')
    nan_in_y = y_expl.isnull().values.any()
    if nan_in_y:
        raise ValueError('y_expl contains NaN, which is not allowed')

    # Setting seed did not work
    # np.random.seed(random_state)
    # rng = np.random.RandomState(random_state)
    output_dir = model_obj.output_dir
    # Make output hierarchy:
    output_dir_svic = os.path.join(output_dir, 'svic')
    if not os.path.exists(output_dir_svic):
        os.makedirs(output_dir_svic)

    n_cores_max = mp.cpu_count()-1 # prevent full load on all threads
    if n_cores > n_cores_max:
        n_cores = n_cores_max
        print(f"Too many cores selected. Reduced to {n_cores}.\n")
    else:
        print(f"Using {n_cores} cores in parallel computing.\n")
    
    if model_obj.outcome_type == "binary":
        x_dm, x_groups = _util.model_matrix(x=model_obj.x, 
            x_names_cat=model_obj.x_names_cat)
        model = LogisticRegression(random_state=0, solver="liblinear").fit(
            x_dm.values, model_obj.y.values
        )
    elif model_obj.outcome_type == "continuous":
        x_dm, x_groups = _util.model_matrix(x=model_obj.x, 
            x_names_cat=model_obj.x_names_cat)
        model = LinearRegression().fit(
            x_dm.values, model_obj.y.values
        )
    elif model_obj.outcome_type == "ordinal":
        model = model_obj.model_optim
    else:
        #model = model_obj.model_optim
        print(f"Other outcome type not yet supported.\n")
        return None

    x_expl_dm, x_groups = _util.model_matrix(
        x=x_expl, x_names_cat=model_obj.x_names_cat
    )

    start_time = time.perf_counter()
    models_list = model_obj.models_near_optim.values.tolist()
    with tqdm_joblib(tqdm(desc="ShapleyVIC", total=len(models_list))) as progress_bar:
        df_svic = Parallel(n_jobs=n_cores, verbose=1)(
            delayed(_sage.compute_sage_values)(
                model=model, coef_vec=models_list[i][:-1], 
                x_expl_dm=x_expl_dm, x_groups=x_groups, 
                y_expl=y_expl, outcome_type=model_obj.outcome_type, 
                var_names=x_expl.columns.values, 
                model_id=i, perf_metric=models_list[i][-1], 
                output_file=os.path.join(output_dir_svic, f'shapley_vic_{i}.csv'), 
                threshold=threshold
            ) 
            for i in range(len(models_list))
        )
        sys.stdout.flush()
    # Initial result is list. Combine to DataFrame:
    df_svic = pd.concat(df_svic)
    finish_time = time.perf_counter()
    diff_time = finish_time-start_time
    diff_min = diff_time / 60
    if diff_min < 60:
        print(f"Program finished in {diff_min} minutes")
    else:
        print(f"Program finished in {diff_min/60} hours")
    
    df_svic.to_csv(os.path.join(output_dir, 'df_svic.csv'))
    return None
