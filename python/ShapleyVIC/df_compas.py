import pkg_resources
import pandas as pd


def load_data():
    """Return a dataframe on 2-year recidivism.
    
    Contains the 2-year recidivism outcome and 6 binary variables of 7214 
    subjects and an additional train-test indicator, created from raw data
    (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas.csv) and code
    (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas-logistics.R) shared
    by Dong and Rudin (2020). Columns are:
        y              non-null int64. 2-year recidivism (the binary outcome, 1=event and 0=non-event).
        age            non-null int64. Age dichotomised at 20 (1/0).
        race           non-null int64. Race, African American or otherwise (1/0).
        prior          non-null int64. Presence of prior criminal history (1/0).
        gender         non-null int64. Gender (1/0).
        juvenilecrime  non-null int64. Presence of juvenile criminal history (1/0).
        currentcharge  non-null int64. Current charge (1/0).
        train_test     non-null object. Whether the observation belonged to the training or test set ("train"/"test").

    """
    # This is a stream-like object. If you want the actual info, call
    # stream.read()
    stream = pkg_resources.resource_stream(__name__, 'data/df_compas.csv')
    return pd.read_csv(stream)

