import pkg_resources
import pandas as pd

def load_data():
    # url = "https://stats.idre.ucla.edu/stat/data/ologit.dta"
    # data_student = pd.read_stata(url)
    stream = pkg_resources.resource_stream(__name__, 'data/ologit.dta')
    data_student = pd.read_stata(stream)
    data_student['apply'].replace(
        {'very likely':2, 'somewhat likely':1, 'unlikely':0}, 
        inplace=True
    )
    return data_student
