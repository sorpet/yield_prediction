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



Years <- indata %>% distinct(Year) %>% .$Year

# Increasing sequence of years starting with 2010
two   <- v[c(1, 2)] # 2010 and 2011, etc.
three <- v[c(1, 2, 3)]
four  <- v[c(1, 2, 3, 4)]
five  <- v[c(1, 2, 3, 4, 5)]
six   <- v[c(1, 2, 3, 4, 5, 6)]
seven <- v[c(1, 2, 3, 4, 5, 6, 7)]
eigth <- v[c(1, 2, 3, 4, 5, 6, 7, 8)]
nine <- v[c(1, 2, 3, 4, 5, 6, 7, 8, 9)]

# Decreasing sequence of years starting with 2020. Uncomment these for before running models with decreasing year sequence
#two   <- v[c(9, 8)]
#three <- v[c(9, 8, 7)]
#four  <- v[c(9, 8, 7, 6)]
#five  <- v[c(9, 8, 7, 6, 5)]
#six   <- v[c(9, 8, 7, 6, 5, 4)]
#seven <- v[c(9, 8, 7, 6, 5, 4, 3)]
#eigth <- v[c(9, 8, 7, 6, 5, 4, 3, 2)]
#nine <-  v[c(9, 8, 7, 6, 5, 4, 3, 2, 1)] 

comb_list <- list(two,three,four,five,six,seven,eigth,nine)

# Defining specific model arguments
# Defining independant training frames
args_unique <- tibble(cons_years=comb_list) %>%
  mutate(training_frames = map(cons_years, ~ indata %>% filter(Year %in% .x)),
         training_frames = map(training_frames, ~ as.h2o(.x)),
  )

# Defining common model arguments
args_in_common <- list(y = "Yield",             # Defining dependent variable for the procedure
                       x = c("Int", "RVImax","P6","ET6"),
                       exclude_algos = c("GLM"),
                       seed = 1,                # To increase reproducibility of the results
                       fold_column = "Year",    # Year is folding variable. Number of foldings is set to the number of years, i.e. 9
                       keep_cross_validation_predictions = TRUE,
                       max_runtime_secs = 0     # no time limitation
)

# Defining all model arguments (common and specific)
len = dim(args_unique)[1]
args <- rep(list(args_in_common),len)
model_arguments <- tibble(args = args) %>%
  mutate(args = map2(args,args_unique$training_frames, ~ c(.x,list("training_frame"=.y))))

# Running models
amls <- model_arguments %>%
  mutate(
    leaderboards  = invoke_map(h2o.automl, args),
    models        = map(leaderboards,~ .x@leader),
    models_bne    = map(leaderboards,~ extract_best_non_ensemble(.x)),
    aml_cvpreds = map(leaderboards, ~ extract_aml_cvpreds(.x)),
  )
# Information about the trained model are stored in the h2o.automl objects.
# These objects can be inspected. For instance:
# The command amls$models[[1]] prints information about the first of the 8 trained aml models i.e. sequences of years.
# This is necessary if you want to calculate standard deviation for MAE based on the variable number of foldings.

# Extracting cross validated MAE and R2
amls <- amls %>%
  mutate(mae_xval    = map_dbl(models, h2o.mae,xval=TRUE),
         r2_xval     = map_dbl(models, h2o.r2 ,xval=TRUE),)

### All done, shutdown H20
h2o.shutdown(prompt=FALSE)
