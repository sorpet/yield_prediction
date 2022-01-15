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

Dnos <- 71:91 # All days between June 10. and June 30.

# Filtering of input data
indata <- indata %>%
  filter(Crop == "Barley", # Wheat in 2018 and 2019
         !(Year %in% c(2010, 2011,2012, 2013) & CC == "CC+"),
         Dno %in% Dnos) # No CC+ treatment in those years


# Preparing data for H2O
indata <- indata %>%
  as.h2o()


# Defining specific model arguments
# Defining independent training frames
args_unique <- as.data.frame(indata) %>%
  group_by(Dno) %>%
  group_split() %>%
  map(~ as.h2o(.x))

# Defining common model arguments
args_in_common <- list(y = "Yield",             # Defining dependent variable for the procedure
                       x = c("Int", "RVImax","P30","ET30"),
                       seed = 1,                # To increase reproducibility of the results
                       exclude_algos = c("GLM"),
                       fold_column = "Year",      # Year is folding variable. Number of foldings is set to the number of years, i.e. 9
                       keep_cross_validation_predictions = TRUE,
                       max_runtime_secs = 0     # no time limitation
)

model_arguments <- tibble(training_frames = args_unique) %>%
  mutate(args = map(training_frames, ~ c(list(training_frame=.x), args_in_common)))
model_arguments 

# Running 21 models, this will take time/days depending on your equipment.
amls <- model_arguments %>%
  mutate(
    leaderboards  = invoke_map(h2o.automl, args),
    models        = map(leaderboards,~ .x@leader),
    models_bne    = map(leaderboards,~ extract_best_non_ensemble(.x)),
    aml_cvpreds   = map(leaderboards, ~ extract_aml_cvpreds(.x)),
  )
# Information about the trained model are stored in the h2o.automl objects.
# These objects can be inspected. For instance:
# The command amls$models[[1]] prints information about the first of the 21 trained aml models i.e. June 10.
# This is necessary if you want to calculate standard deviation for MAE based on the 9 different foldings (developed models).

# Extracting cross validated MAE and R2
amls <- amls %>%
  mutate(mae_xval    = map_dbl(models, h2o.mae,xval=TRUE),
         r2_xval     = map_dbl(models, h2o.r2 ,xval=TRUE),)

### All done, shutdown H20
h2o.shutdown(prompt=FALSE)