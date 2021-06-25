library(reticulate)
library(dplyr)
# df <- read.csv("../../Analysis/data/compas.csv") %>%
df <- read.csv("data/compas.csv") %>%
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
write.csv(df[-i_test, ], file = "~/py_compas_train.csv", row.names = FALSE)
write.csv(df[i_test, ], file = "~/py_compas_test.csv", row.names = FALSE)
m_optim_r <- glm((y_train == 1) ~ X_train, family = "binomial")
coef_optim <- coef(m_optim_r)
write.csv(rbind(coef_optim, coef_optim), file = "~/py_compas_coef_optim.csv", row.names = FALSE)
# Only use the Python glm as a skeleton. Use the R glm estimates
# m_optim$intercept_ <- coef_optim[1]
# m_optim$coef_ <- matrix(coef_optim[-1], nrow = 1)
# set.seed(1234)
# mr_optim <- compute_mr_model(model_py = m_optim, X = X_test, y = y_test,
#                              check_convergence = TRUE)

# coef_df <- readRDS("../../Analysis/data/compas/coef_df_0609.RDS")
coef_df <- readRDS("data/compas/coef_df_0609.RDS")
rashomon <- coef_df %>% filter(select == 1) %>%
  select(-id, -loss_ratio, -loss_in_rashomon, -select)
write.csv(rashomon, file = "~/py_compas_rashomon.csv", row.names = FALSE)
# write.csv(rashomon, file = "~/py_compas/rashomon.csv", row.names = FALSE)
mr_folder <- "../../shapley_vic_compas_2021-06-17"
if (!dir.exists(mr_folder)) dir.create(mr_folder)
set.seed(2018)
# model class reliance for all models in the rashomon set
py_code <- readLines("tests/py_compas.py")
rows <- 1:nrow(rashomon)
lapply(rows, function(i) {
  csv_name <- file.path(mr_folder, paste0("compas_shapley_reg_", i, ".csv"))
  py_name <- paste0("~/py_compas/py_script_", i, ".py")
  sink(py_name)
  cat("i =", i, "\n")
  cat("csv_name = '", csv_name, "'\n", sep = "")
  lapply(py_code, function(row) cat(row, "\n"))
  sink()
  source_python(py_name)
})
