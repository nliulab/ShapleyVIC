import pandas
from sklearn.model_selection import train_test_split
train = pandas.read_csv('~/df.csv')
# df_train = pandas.DataFrame(train)
# df_train.to_csv("~/train_py.csv")
# df_test = pandas.DataFrame(test)
# df_test.to_csv("~/test_py.csv")
Y_train = train[:, 0].copy().astype(int)
Y_test = test[:, 0].copy().astype(int)
train = train[:, :-1].copy()
test = test[:, :-1].copy()


import sage
from sklearn.linear_model import LogisticRegression

model = LogisticRegression(random_state=0).fit(train, Y_train)
# Y_pred = model.predict(test)
# print(Y_pred)
# print(test)
# model.predict_proba(test)
# pandas.crosstab(Y_pred, Y_test)

# Setup and calculate
# imputer = sage.MarginalImputer(model, test)
# estimator = sage.PermutationEstimator(imputer, 'cross entropy')
# sage_values = estimator(test, Y_test, verbose = True)
# sage_values.values
# sage_values.std

####

from shapreg import removal, games, shapley
from shapreg.utils import crossentropyloss
from shapreg import stochastic_games
model_lam = lambda x: model.predict_proba(x)
marginal_extension = removal.MarginalExtension(train, model_lam)
game = stochastic_games.DatasetLossGame(marginal_extension, test, Y_test, crossentropyloss)
expl = shapley.ShapleyRegression(game)
sage_df = pandas.DataFrame({'sage_value':expl.values, 'sage_sd':expl.std})
sage_df.to_csv('~/sage_df.csv')

# coef = [11.22768236,  0.25843793,  0.18604274, -0.35851358,  0.15133452, -0.36411981]
# expl = compute_sage_value_logit_reg(-5.45936444, coef, train, Y_train,test, Y_test)
        
