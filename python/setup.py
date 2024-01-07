from setuptools import setup, find_packages

setup(
    name="ShapleyVIC",
    version="1.2.0",
    author="Yilin Ning, Chenglin Niu, Mingxuan Liu",
    author_email="yilin.ning@duke-nus.edu.sg",
    description="ShapleyVIC: Shapley Variable Importance Cloud for Interpretable Machine Learning, with survival",
    packages=['ShapleyVIC'],
    install_requires=[ 
        'sage-importance @ git+https://github.com/nyilin/sage.git',
        'pandas', 'numpy', 
        'plotnine', 
        'statsmodels', 'scikit-learn', 
        'tqdm', 'tqdm_joblib', 'joblib'
    ],
    classifiers = [
        'Programming Language :: Python :: 3',
        'License :: OSI Approved :: MIT License',
    ],
    python_requires='>=3.6',
    include_package_data=True,
    package_data={'': ['data/*.csv', 'data/*.dta']},
)
