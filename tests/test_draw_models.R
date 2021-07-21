data("df_compas")
X <- df_compas[, -1]
y <- df_compas$y
set.seed(1234)
i_test <- sample(1:nrow(df_compas), size = round(nrow(df_compas) * 0.1),
                 replace = FALSE)
X_train <- as.matrix(X[-i_test, ])
y_train <- matrix(y[-i_test], ncol = 1)
X_test <- as.matrix(X[i_test, ])
y_test <- matrix(y[i_test], ncol = 1)
m_optim_r <- glm(y ~ ., data = df_compas[-i_test, ], family = "binomial")
coef_df <- ShapleyVIC::draw_models(coef_optim = coef(m_optim_r),
                                   coef_optim_var = vcov(m_optim_r),
                                   design_mat = cbind(1, X_train), y = y_train,
                                   M = 800, u1 = 0.5, u2 = 80, epsilon = 0.05,
                                   n_final = 350)
saveRDS(coef_df, file = "~/coef_df_0609.RDS")

compute_loss_logit <- function(beta, x_df, y) {
  if (!all(y %in% c(-1, 1))) {
    y <- ifelse(y == 1, 1, -1)
  }
  X <- model.matrix(~ ., data = x_df)
  sum(log(1 + exp(-y * X %*% beta)))
}
compute_mr_logit = function(vname, beta, x_df, y, n_iters = 50){
  loss0 <- compute_loss_logit(beta = beta, x_df = x_df, y = y)
  if (all(x_df[, vname] %in% c(0, 1))) {
    # Variable of interest is binary: there is a closed form for shuffled loss
    p = sum(x_df[, vname] == 1)/nrow(x_df)
    # replaced by 1 with prob p
    x_df[, vname] <- 1
    loss = compute_loss_logit(beta = beta, x_df = x_df, y = y) * p
    x_df[, vname] <- 0
    loss = loss + compute_loss_logit(beta = beta, x_df = x_df, y = y) * (1 - p)
    # this is the loss after shuffle the obs
    mr = loss / loss0
  } else {
    # Variable of interest is numeric/categorical: need to randomly permute the
    # variable to compute empirical loss
    x <- x_df[, vname]
    loss_vec <- unlist(lapply(1:n_iters, function(iter) {
      # Randomly permute the variable of interest:
      x_df[, vname] <- sample(x, size = length(x), replace = FALSE)
      # Compute the loss using this permuted variable:
      compute_loss_logit(beta = beta, x_df = x_df, y = y)
    }))
    mr <- mean(loss_vec) / loss0
  }
  return(mr)
}
compute_mr_full <- function(vlist, beta, x_df, y){
  unlist(lapply(vlist, function(vname) {
    compute_mr_logit(vname = vname, beta = beta, x_df = x_df, y = y)
  }))
}
# parameter
epsilon = 0.05
set.seed(2018)

vlist <- names(X)
library(dplyr)
coef_df_final <- coef_df %>% filter(select == 1) %>%
  select(-id, -loss_ratio, -loss_in_rashomon, -select)
mcr_vic <- as.data.frame(do.call("rbind", lapply(1:nrow(coef_df_final), function(i) {
  compute_mr_full(vlist = vlist, beta = as.double(coef_df_final[i, ]),
                  x_df = as.data.frame(X_test), y = y_test)
})))
names(mcr_vic) <- colnames(X_train)
apply(mcr_vic, 2, summary)
saveRDS(mcr_vic, file = "~/mcr_vic_compas_0609.RDS")

rash_loss <- t(apply(coef_df_final, 1, function(coef_vec) {
  ShapleyVIC:::compute_loss_bin(coef_vec = coef_vec,
                                design_mat = cbind(1, X_train),
                                y = y_train)
}))
coef_df_final_long <- do.call("rbind", lapply(1:nrow(coef_df_final), function(i) {
  data.frame(id = i, Variable = colnames(coef_df_final)[-1],
             coef = as.numeric(coef_df_final[i, -1]))
}))
df_imp_vic <- data.frame(
  id = rep(1:nrow(mcr_vic), rep = ncol(mcr_vic)),
  Variable = rep(names(X), each = nrow(mcr_vic)),
  loss = rep(as.numeric(rash_loss), rep = ncol(mcr_vic))
) %>%
  left_join(coef_df_final_long) %>%
  mutate(val = unlist(c(mcr_vic)) - 1, sd = NA, val_lower = NA, val_upper = NA,
         pval = NA, sig = FALSE, Method = "VIC")
df_imp_vic_bar <- ShapleyVIC::summarise_shapley_vic(val = df_imp_vic$val, val_sd = NULL,
                                                    var_names = df_imp_vic$Variable)
loss_optim <- ShapleyVIC:::compute_loss_bin(coef_vec = coef(m_optim_r),
                               design_mat = cbind(1, X_train), y = y_train)
df_imp_vic %>%
  mutate(
    `% exceeding minimum loss` = cut(
      x = loss,
      breaks = c(-Inf, loss_optim * (1 + (1:4) / 100), Inf), right = FALSE,
      labels = paste0("<", 1:5, "%")
    ),
    Variable_ordered = factor(Variable, levels = levels(df_imp_vic_bar$Variable_ordered))
  ) %>%
  ggplot(aes(x = Variable_ordered, y = val + 1, color = `% exceeding minimum loss`)) +
  geom_hline(yintercept = 1) +
  ggbeeswarm::geom_quasirandom(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(n = 6, name = "Blues")[-1])) +
  theme_bw() +
  coord_flip() +
  labs(x = "", y = "Model reliance", title = "(b) VIC")

ggplot(data = data.frame(x = unique(df_imp_vic$loss) / loss_optim), aes(x = x)) +
  geom_histogram(breaks = seq(from = 1, to = 1.05, by = 0.005)) +
  labs(x = "Ratio of loss to minimum loss", y = "Count",
       title = "350 nearly optimal models selected for COMPAS study") +
  theme_bw()

# -----

library(dplyr)
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

coef_df <- ShapleyVIC::draw_models(coef_optim = coef(m_optim_r),
                                   coef_optim_var = vcov(m_optim_r),
                                   design_mat = cbind(1, X_train), y = y_train,
                                   M = 800, u1 = 0.5, u2 = 20, epsilon = 0.05,
                                   n_final = 350)
saveRDS(coef_df, file = "~/coef_df_mimic_0609.RDS")

coef_df <- ShapleyVIC::draw_models(coef_optim = coef(m_optim_r),
                                   coef_optim_var = vcov(m_optim_r),
                                   design_mat = cbind(1, X_train), y = y_train,
                                   M = 800, u1 = 0.5, u2 = 20, epsilon = 0.05,
                                   n_final = 400)
saveRDS(coef_df, file = "~/coef_df_mimic_0628.RDS")

compute_loss_logit <- function(beta, x_df, y) {
  if (!all(y %in% c(-1, 1))) {
    y <- ifelse(y == 1, 1, -1)
  }
  X <- model.matrix(~ ., data = x_df)
  sum(log(1 + exp(-y * X %*% beta)))
}
compute_mr_logit = function(vname, beta, x_df, y, n_iters = 50){
  loss0 <- compute_loss_logit(beta = beta, x_df = x_df, y = y)
  if (all(x_df[, vname] %in% c(0, 1))) {
    # Variable of interest is binary: there is a closed form for shuffled loss
    p = sum(x_df[, vname] == 1)/nrow(x_df)
    # replaced by 1 with prob p
    x_df[, vname] <- 1
    loss = compute_loss_logit(beta = beta, x_df = x_df, y = y) * p
    x_df[, vname] <- 0
    loss = loss + compute_loss_logit(beta = beta, x_df = x_df, y = y) * (1 - p)
    # this is the loss after shuffle the obs
    mr = loss / loss0
  } else {
    # Variable of interest is numeric/categorical: need to randomly permute the
    # variable to compute empirical loss
    x <- x_df[, vname]
    loss_vec <- unlist(lapply(1:n_iters, function(iter) {
      # Randomly permute the variable of interest:
      x_df[, vname] <- sample(x, size = length(x), replace = FALSE)
      # Compute the loss using this permuted variable:
      compute_loss_logit(beta = beta, x_df = x_df, y = y)
    }))
    mr <- mean(loss_vec) / loss0
  }
  return(mr)
}
compute_mr_full <- function(vlist, beta, x_df, y){
  unlist(lapply(vlist, function(vname) {
    compute_mr_logit(vname = vname, beta = beta, x_df = x_df, y = y)
  }))
}
# parameter
epsilon = 0.05
set.seed(2018)

vlist <- names(X)
library(dplyr)
coef_df_final <- coef_df %>% filter(select == 1) %>%
  select(-id, -loss_ratio, -loss_in_rashomon, -select)
mcr_vic <- as.data.frame(do.call("rbind", lapply(1:nrow(coef_df_final), function(i) {
  compute_mr_full(vlist = vlist, beta = as.double(coef_df_final[i, ]),
                  x_df = as.data.frame(X_test), y = y_test)
})))
names(mcr_vic) <- colnames(X_train)
apply(mcr_vic, 2, summary)
saveRDS(mcr_vic, file = "~/mcr_vic_mimic_0609.RDS")

rash_loss <- t(apply(coef_df_final, 1, function(coef_vec) {
  ShapleyVIC:::compute_loss_bin(coef_vec = coef_vec,
                                design_mat = cbind(1, X_train),
                                y = y_train)
}))
coef_df_final_long <- do.call("rbind", lapply(1:nrow(coef_df_final), function(i) {
  data.frame(id = i, Variable = colnames(coef_df_final)[-1],
             coef = as.numeric(coef_df_final[i, -1]))
}))
df_imp_vic <- data.frame(
  id = rep(1:nrow(mcr_vic), rep = ncol(mcr_vic)),
  Variable = rep(names(X), each = nrow(mcr_vic)),
  loss = rep(as.numeric(rash_loss), rep = ncol(mcr_vic))
) %>%
  left_join(coef_df_final_long) %>%
  mutate(val = unlist(c(mcr_vic)) - 1, sd = NA, val_lower = NA, val_upper = NA,
         pval = NA, sig = FALSE, Method = "VIC")
df_imp_vic_bar <- ShapleyVIC::summarise_shapley_vic(val = df_imp_vic$val, val_sd = NULL,
                                                    var_names = df_imp_vic$Variable)
loss_optim <- ShapleyVIC:::compute_loss_bin(coef_vec = coef(m_optim_r),
                                            design_mat = cbind(1, X_train), y = y_train)
library(ggplot2)
df_imp_vic %>%
  mutate(
    `% exceeding minimum loss` = cut(
      x = loss,
      breaks = c(-Inf, loss_optim * (1 + (1:4) / 100), Inf), right = FALSE,
      labels = paste0("<", 1:5, "%")
    ),
    Variable_ordered = factor(Variable, levels = levels(df_imp_vic_bar$Variable_ordered))
  ) %>%
  ggplot(aes(x = Variable_ordered, y = val + 1, color = `% exceeding minimum loss`)) +
  geom_hline(yintercept = 1) +
  ggbeeswarm::geom_quasirandom(size = 0.5, alpha = 0.5) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(n = 6, name = "Blues")[-1])) +
  theme_bw() +
  coord_flip() +
  labs(x = "", y = "Model reliance", title = "(b) VIC")

ggplot(data = data.frame(x = unique(df_imp_vic$loss) / loss_optim), aes(x = x)) +
  geom_histogram(breaks = seq(from = 1, to = 1.05, by = 0.005)) +
  labs(x = "Ratio of loss to minimum loss", y = "Count",
       title = "350 nearly optimal models selected for COMPAS study") +
  theme_bw()
