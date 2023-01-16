d <- "/Users/yilinning/Library/CloudStorage/OneDrive-NationalUniversityofSingapore/Staff_Yilin/[Archive] Published work/1 ShapleyVIC-readmission/Analysis/shapley_vic_output"
f_vec <- dir(d)
df_svic <- do.call("rbind", lapply(f_vec, function(f) {
  read.csv(file.path(d, f))
}))
head(df_svic)
names(df_svic)[2] <- "var_names"
names(df_svic)[3] <- "sage_value_unadjusted"
write.csv(df_svic, file = "score_output/df_svic.csv", row.names = FALSE)

models <- df_svic
models$shapley_vic_val <- models$sage_value_unadjusted
models$var_names_raw <- models$var_names
x_names_display <- c(
  "Age", "Gender", "Race", "ED LOS", "ED triage", 
  "ED boarding time", "Consultation waiting time", "No. ED visit", 
  "Day of week", "Inpatient LOS", "Ventilation", "Resuscitation", 
  "No. surgery", "No. ICU stay", 
  "No. HD stay", "Pulse", "Respiration", "SpO2", 
  "DBP", "SBP", "Bicarbonate", "Creatinine", 
  "Potasium", "Sodium", "MI", "CHF", "PVD", "Stroke", 
  "Dementia", "Pulmonary", "Rheumatic", "PUD", "Mild liver disease", 
  "Diabetes", "Diabetes with complications", "Paralysis", "Renal", "Cancer", 
  "Severe liver disease", "Metastatic cancer", "Admission type"
)
x_names <- setdiff(names(dat), "label")
models$var_names <- x_names_display[match(x = models$var_names, table = x_names)]
overall_importance <- ShapleyVICOld::summarise_shapley_vic(
  val = models$shapley_vic_val, val_sd = models$sage_sd, 
  var_names = models$var_names
)
overall_importance$significant <- overall_importance$val_lower > 0
overall_importance <- overall_importance[sort.list(-overall_importance$val), ]
model_object <- list(models = models, overall_importance = overall_importance)
class(model_object) <- "ShapleyVIC"
plot(model_object)
saveRDS(model_object, file = "score_output/model_object.RDS")

model_object <- readRDS("score_output/model_object.RDS")
library(ShapleyVIC)
rank_variables(x = model_object, summarise = TRUE, as_vector = TRUE)
rank_variables(x = model_object, summarise = TRUE, filter = FALSE)
rank_variables(x = model_object, summarise = FALSE, filter = FALSE)
