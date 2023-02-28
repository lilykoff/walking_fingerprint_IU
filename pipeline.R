
library(tidyverse)
df_all <- read.csv("df_all.csv")[,-1]

subjects <- unique(df_all$ID2)
time_lags <- seq(15, 90, 15)
gcell_size <- 0.25
location <- "lw"
all_data <- map_dfr(.x = subjects, .f = get_grid_data,
                    time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)

# write.csv(all_data, "all_data.csv")
all_data <- read_csv("all_data.csv")[,-1]
# split into training and testing based on percent 
training_pct <- 0.75 
threshold <- 0.001
n_predictors=Inf
data_train <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J <= maxJ*training_pct) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
data_test <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J > maxJ*training_pct) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()
subjects <- unique(all_data$ID)
all_predictions <- map_dfr(.x = subjects, n_predictors=n_predictors, train = data_train,
                           test = data_test,threshold = threshold,
                           .f = fit_model)

# print summary 
summarize_preds(all_predictions = all_predictions)
# store plot 
g <- plot_preds(all_predictions = all_predictions)

