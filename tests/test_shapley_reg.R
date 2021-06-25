library(dplyr)
library(reticulate)
sklearn_linear <- import("sklearn.linear_model")
source_python("inst/compute_sage_value_reg.py")
source_python("inst/compute_sage_value.py")
source("R/compute_shaply_vic.R")
load("../../Analysis/data/testdf6_mimic_20000.Rdata")
X <- testdf6_mimic_20000 %>% select(-label) %>% as.data.frame()
y <- as.numeric(as.character(testdf6_mimic_20000$label))
# SAGE suggest test size<=1024
set.seed(1234)
i_test <- sample(1:nrow(testdf6_mimic_20000), size = 3000, replace = FALSE)
X_train <- as.matrix(X[-i_test, ])
y_train <- matrix(y[-i_test], ncol = 1)
X_test <- as.matrix(X[i_test, ])
y_test <- matrix(y[i_test], ncol = 1)

m_optim_r <- glm((y_train == 1) ~ X_train, family = "binomial")
coef_optim <- coef(m_optim_r)
se_optim <- sqrt(diag(vcov(m_optim_r)))
# Only use the Python glm as a skeleton. Use the R glm estimates
m_optim <- sklearn_linear$LogisticRegression(solver = 'liblinear')$fit(
  X_train, as.vector(y_train)
)
set.seed(1234)
mr_optim1 <- compute_mr_model(model_py = m_optim, coef_vec = coef_optim,
                              X = X_test, y = y_test)
set.seed(1234)
mr_optim2 <- compute_mr_model_reg(model_py = m_optim, coef_vec = coef_optim,
                                  X_train = X_train, X = X_test, y = y_test)

# -----

library(dplyr)
library(reticulate)
sklearn_linear <- import("sklearn.linear_model")
source_python("inst/compute_sage_value_reg.py")
source_python("inst/compute_sage_value.py")
source("R/compute_shaply_vic.R")
df <- read.csv("../../Analysis/data/compas.csv") %>%
  select(Age.18.20, Age..45, Gender.Male, Race.African.American, Race.Caucasian,
         Race.Asian, Race.Hispanic, Race.Native.American, Race.Other, Juvenile.Felonies.0,
         Juvenile.Felonies.1.3, Juvenile.Felonies.3, Juvenile.Crimes.0, Juvenile.Crimes.1.3,
         Juvenile.Crimes.3, Juvenile.Crimes.5, Prior.Crimes.0, Prior.Crimes.1.3,
         Prior.Crimes.3, Prior.Crimes.5, Current.Charge.Degree.Misdemeanor,
         Recidivate.Within.Two.Years)
df <- data.frame(y = ifelse(df$Recidivate.Within.Two.Years == 1, 1, 0),
                 age = df$Age.18.20,
                 race = df$Race.African.American,
                 prior = df$Prior.Crimes.0,
                 gender = df$Gender.Male,
                 juvenilecrime = df$Juvenile.Crimes.0,
                 currentcharge = df$Current.Charge.Degree.Misdemeanor)
# write.csv(df, file = "~/df.csv", row.names = FALSE)
X <- df[, -1]
y <- df$y
set.seed(1234)
# SAGE suggest test size<=1024
i_test <- sample(1:nrow(df), size = round(nrow(df) * 0.1), replace = FALSE)
X_train <- as.matrix(X[-i_test, ])
y_train <- matrix(y[-i_test], ncol = 1)
X_test <- as.matrix(X[i_test, ])
y_test <- matrix(y[i_test], ncol = 1)
# write.csv(df[-i_test, ], file = "../../Analysis/data/compas_train.csv", row.names = FALSE)
# write.csv(df[i_test, ], file = "../../Analysis/data/compas_test.csv", row.names = FALSE)

m_optim_r <- glm((y_train == 1) ~ X_train, family = "binomial")
coef_optim <- coef(m_optim_r)
# Only use the Python glm as a skeleton. Use the R glm estimates
m_optim <- sklearn_linear$LogisticRegression(solver = 'liblinear')$fit(
  X_train, as.vector(y_train)
)
set.seed(1234)
mr_optim1 <- compute_mr_model(model_py = m_optim, coef_vec = coef_optim,
                              X = X_test, y = y_test)

set.seed(1234)
mr_optim2 <- compute_mr_model_reg(
  coef_int = c(coef_optim[1]), coef = coef_optim[-1],
  X_train = X_train, y_train = as.vector(y_train), X = X_test, y = y_test
)

# return_vec(coef_optim)
game <- return_model(X_train, as.vector(y_train),
                     # X_test[1:20, ], matrix(y_test[1:20, ], ncol = 1),
                     X_train, y_train,
                     coef_optim[1], coef_optim[-1])
game$extension
