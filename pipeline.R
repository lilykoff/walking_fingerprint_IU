
library(tidyverse)
library(readr)
df_all <- read_csv("df_all.csv", col_types = cols(...1 = col_skip())) 



subjects <- unique(df_all$ID2)
time_lags <- seq(15, 90, 15)
gcell_size <- 0.25
location <- "lw"
all_data <- map_dfr_progress(.x = subjects, .f = get_grid_data,
                    time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)

data_train <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J <= 200) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
data_test <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J > 200 & J <= 380) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()


all_predictions <- map_dfr_progress(.x = subjects, n_predictors=Inf, train = data_train,
                           test = data_test,threshold = 0.001,
                           .f = fit_model)
plot_preds(all_predictions)

write.csv(all_data, "all_data.csv")



# stop here 
gcell_size <- 0.15
location <- "lw"
all_data_gcell_0.15 <- map_dfr(.x = subjects, .f = get_grid_data,
                    time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)
write.csv(all_data_gcell_0.15, "all_data_gc_0.15.csv")
location <- "la"
all_data_gcell_0.15_la <- map_dfr(.x = subjects, .f = get_grid_data,
                               time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)


write.csv(all_data_gcell_0.15_la, "all_data_gc_0.15_la.csv")

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

data_train <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J <= 200) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
data_test <- all_data %>% group_by(ID) %>% 
  mutate( maxJ = max(J)) %>% filter(J > 200) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()


all_predictions <- map_dfr(.x = subjects, n_predictors=n_predictors, train = data_train,
                           test = data_test,threshold = threshold,
                           .f = fit_model)
all_models <- map(.x = subjects, n_predictors=n_predictors, train = data_train,
                           test = data_test,threshold = threshold,
                           .f = extract_models)


# print summary 
summarize_preds(all_predictions = all_predictions)
# store plot 
g <- plot_preds(all_predictions = all_predictions)
g

training_pct <- 0.60
all_predictions_30 <- map_dfr(.x = subjects, n_predictors=Inf, train = data_train,
                           test = data_test,threshold = threshold,
                           .f = fit_model)
summarize_preds(all_predictions_30)
