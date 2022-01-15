# SETUP
library(tidyverse)
library(h2o)
library(here)
source(here("src", "data", "extract_aml_cvpreds.R"))
source(here("src","data", "extract_best_non_ensemble.R"))

# We used a computer with 32 GB RAM and reserved 25GB RAM for the task. 
h2o.init(max_mem_size = "25G") 
h2o.removeAll()

# Import of input data
indata <- read_csv(here("data","processed","input.csv")) %>%
  mutate(
    Year  = as_factor(Year),
    Month = as_factor(Month),
    Day   = as_factor(Day),
    Dno   = as_factor(Dno),
    Block = as_factor(Block),
    WL    = as_factor(WL),
    CC    = as_factor(CC),
    Crop  = as_factor(Crop),
  )

# Filtering of input data
indata <- indata %>%
  filter(Crop == "Barley", # Wheat in 2018 and 2019
         Dno == 91, # June 30.
         !(Year %in% c(2010, 2011,2012, 2013) & CC == "CC+")) # No CC+ treatment in those years

# Preparing data for H2O
indata <- indata %>%
  as.h2o()

# Defining specific model arguments
# Defining independent variables for the procedure
x <- c("Int", "RVImax","P6","ET6")
args_unique <- list(x)

# Defining common model arguments
args_in_common <- list(y = "Yield",             # Defining dependent variable for the procedure
                       training_frame = indata, # 
                       seed = 1,                # To increase reproducibility of the results
                       fold_column = Block,     # Block is folding variable. Number of foldings is set to the number of blocks, i.e. 4
                       keep_cross_validation_predictions = TRUE,
                       max_runtime_secs = 0     # no time limitation
)

# Defining all model arguments (common and specific)
model_arguments <- tibble(features = args_unique) %>%
  mutate(args = map(features, ~ c(list(x=.x), args_in_common)))

# Running models
glms <- model_arguments %>%
  mutate(
    models        = invoke_map(h2o.glm, args),
    glm_cvpreds = map(models, ~ as.vector(h2o.getFrame(.x@model$cross_validation_holdout_predictions_frame_id$name))),
  )
amls <- model_arguments %>%
  mutate(
    leaderboards  = invoke_map(h2o.automl, args),
    models        = map(leaderboards,~ .x@leader),
    models_bne    = map(leaderboards,~ extract_best_non_ensemble(.x)),
    aml_cvpreds = map(leaderboards, ~ extract_aml_cvpreds(.x)),
  )
# Information about the trained models are stored in the h2o.glm and h2o.automl objects.
# These objects can be inspected. For instance:
# The commands glms$models[[1]], and amls$models[[1]] prints information about the trained glm, aml models, respectively
# This is necessary if you want to calculate standard deviation for MAE based on the 4 different foldings (developed models).

# Extracting cross validated MAE and R2
glms <- glms %>%
  mutate(mae_xval    = map_dbl(models, h2o.mae,xval=TRUE),
         r2_xval     = map_dbl(models, h2o.r2 ,xval=TRUE),)

amls <- amls %>%
  mutate(mae_xval    = map_dbl(models, h2o.mae,xval=TRUE),
         r2_xval     = map_dbl(models, h2o.r2 ,xval=TRUE),)

# Combining crossvalidated predictions with indata and writing outdata.
glm_cvpreds = as.h2o(glms$glm_cvpreds)
aml_cvpreds = as.h2o(amls$aml_cvpreds)
names(glm_cvpreds)[1] = c("glm_predict")
names(aml_cvpreds)[1] = c("aml_predict")
outdata04 = h2o.cbind(indata, glm_cvpreds, aml_cvpreds) %>%
  as.data.frame()
outdata04 %>%
  write_csv(here("data","processed","Outdata04.csv"))

### All done, shutdown H20
h2o.shutdown(prompt=FALSE)
