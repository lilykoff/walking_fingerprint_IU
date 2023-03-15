
library(tidyverse)
library(readr)

# steps to replicate analysis
# run data processing file to get raw processed accelerometry data
# load all functions in functions.R file 
# read in 
df_all <- read.csv("df_all.csv")[,-1]

# vector of subjects
subjects <- unique(df_all $ID2)
# vector of time lags 
time_lags <- seq(15, 90, 15)
# grid cell size 
gcell_size <- 0.25
# location - could use other body parts instead 
location <- "lw"

all_data <- map_dfr(.x = subjects, .f = get_grid_data,
                    time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)
# save file to save time in the futur
# write.csv(all_data, "all_data.csv")

all_data <- read_csv("all_data.csv")[,-1]
# get 200 seconds for training, 180 seconds for testing 
data_train <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J <= 200) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
data_test <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J > 200 & J <= 380) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()

# get predicted values from logistic regression 
all_predictions <- map_dfr_progress(.x = subjects, n_predictors=Inf, train = data_train,
                           test = data_test,threshold = 0.001,
                           .f = fit_model)

# plot and summarie predictions 
plot_preds(all_predictions)

summarize_preds(all_predictions)

