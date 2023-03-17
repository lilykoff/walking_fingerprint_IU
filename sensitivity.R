library(rje)
library(tidyverse)
library(viridis)
library(gt)
library(moments)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

df_all <- read.csv("df_all.csv")[,-1]

subjects <- unique(df_all$ID2)
time_lags <- seq(15, 90, 15)
gcell_size <- 0.25
location <- "lw"
#all_data <- map_dfr_progress(.x = subjects, .f = get_grid_data, time_lags = time_lags, gcell_size = gcell_size, location = location, data = df_all)
options(dplyr.summarise.inform = FALSE)
all_data <- read_csv("all_data.csv")[,-1]


map_dfr_progress_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr_progress(.x, f, ..., .id = .id)
}

get_grid_data  <- function(subject, time_lags, gcell_size, location, data){
  df <- data %>% dplyr::select(ID2, paste("signal_", location, sep = ""), J) %>% 
    rename(signal = paste("signal_", location, sep = ""),
           ID = ID2)
  # max_signal <- round(max(df$signal), 0) 
  # to replicate our data we set to 3 
  max_signal <- 3
  tmp <- df %>% filter(ID == subject) # filter to subject 
  n_seconds <- max(tmp$J) # number of total seconds for that subject 
  seconds <- rep(seq(1, n_seconds, 1), each=length(time_lags)) # vector of seconds and lags so that we can iterate over both
  lags <- rep(time_lags, n_seconds) # vector of lags 
  get_grid_data_onesubj <- function(second, lag){
    # function that gets number of points in each "grid cell" for a given second and given lag 
    tmp %>% filter(J==second) %>% dplyr::select(signal) %>% mutate(
      lag_signal = lag(signal, n = lag)  # for each second, calculate signal and lagged signal 
    ) %>% mutate(cut_sig = cut(signal, breaks = seq(0, max_signal, by = gcell_size), include.lowest = T),
                 cut_lagsig = cut(lag_signal, breaks = seq(0, max_signal, by = gcell_size), include.lowest = T)) %>% 
      drop_na() %>% # count # points in each "grid cell" 
      count(cut_sig, cut_lagsig, .drop=FALSE) %>% mutate(lag_hz = lag, ID = subject, J = second, cell = paste(cut_sig, cut_lagsig, lag_hz)) %>%
      dplyr::select(n, ID, J, cell)
  }
  # apply above function over all seconds and lags, return df
  map2_dfr(.x = seconds, .y = lags, 
           .f = get_grid_data_onesubj) %>% pivot_wider(id_cols = c(ID, J), names_from = cell, values_from = n) # apply across all seconds and lags 
}


fit_model <- function(subject, n_predictors, train, test, threshold){
  # first filter to grids above threshold 
  grids <- train %>% filter(ID==subject) %>% pivot_longer(cols = -ID) %>% 
    mutate(lag = sub("^.+] ", "", name)) %>% group_by(name, lag) %>% 
    summarize(value = sum(value)) %>% group_by(lag) %>% mutate(
      group_sum = sum(value)
    ) %>% ungroup() %>% mutate(
      pct = value/group_sum
    ) %>% filter(pct >= threshold) %>% dplyr::select(name)  %>% unlist() %>% unname()
  # make outcome binary 
  train$class <- ifelse(train$ID==subject, 1, 0)
  test$class <- ifelse(test$ID==subject, 1, 0)
  # with the training data, fit a logistic regression with each predictor individually and get p-value 
  # select __ lowest p values based on n_predictors input 
  # if we want to use all predictors, then we set n_predictors=Inf
  if(!is.infinite(n_predictors)){
    imp_grids <- train %>% dplyr::select(all_of(grids)) %>%
      map(~glm(train$class ~.x, data = train, family = binomial(link="logit"))) %>%
      map(summary.glm) %>% map(c("coefficients")) %>% map_dbl(8) %>% as.data.frame() %>% rename(pval = ".") %>% slice_min(pval, n=n_predictors) %>% rownames()
  }
  else{
    imp_grids <- grids
  }
  # select important grids from train and test data 
  tmp <- train %>% dplyr::select(c(class, all_of(imp_grids))) 
  tmp_test <- test %>% dplyr::select(all_of(imp_grids))
  # fit logistic regression 
  mod <- glm(class ~ ., data = tmp, family=binomial(link="logit"))
  # extract predictors
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  # return data frame with predictions, model, and subject 
  return(cbind(pred, rep(subject, length(pred)), test$ID) %>% data.frame() %>% rename(model = V2, true_subject = V3))
}

extract_models <- function(subject, n_predictors, train, test, threshold){
  # first filter to grids above threshold 
  grids <- train %>% filter(ID==subject) %>% pivot_longer(cols = -ID) %>% 
    mutate(lag = sub("^.+] ", "", name)) %>% group_by(name, lag) %>% 
    summarize(value = sum(value)) %>% group_by(lag) %>% mutate(
      group_sum = sum(value)
    ) %>% ungroup() %>% mutate(
      pct = value/group_sum
    ) %>% filter(pct >= threshold) %>% dplyr::select(name)  %>% unlist() %>% unname()
  # make outcome binary 
  train$class <- ifelse(train$ID==subject, 1, 0)
  test$class <- ifelse(test$ID==subject, 1, 0)
  # with the training data, fit a logistic regression with each predictor individually and get p-value 
  # select __ lowest p values based on n_predictors input 
  # if we want to use all predictors, then we set n_predictors=Inf
  if(!is.infinite(n_predictors)){
    imp_grids <- train %>% dplyr::select(all_of(grids)) %>%
      map(~glm(train$class ~.x, data = train, family = binomial(link="logit"))) %>%
      map(summary.glm) %>% map(c("coefficients")) %>% map_dbl(8) %>% as.data.frame() %>% rename(pval = ".") %>% slice_min(pval, n=n_predictors) %>% rownames()
  }
  else{
    imp_grids <- grids
  }
  # select important grids from train and test data 
  tmp <- train %>% dplyr::select(c(class, all_of(imp_grids))) 
  tmp_test <- test %>% dplyr::select(all_of(imp_grids))
  # fit logistic regression 
  mod <- glm(class ~ ., data = tmp, family=binomial(link="logit"))
  # extract predictors
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  # return data frame with predictions, model, and subject 
  return(mod)
}

summarize_preds <- function(all_predictions){
  # group by the true subject and the model
  # for example, predictions from model k, row j is prob(row of j data is from subject k)
  # get the mean predicted probabilities and see if highest predicted prob is "correct" 
  df <- all_predictions %>% group_by(true_subject, model) %>% summarize(mean_pred = mean(pred)) %>% 
    group_by(true_subject) %>% summarize(
      maxprob = max(mean_pred),
      prediction = model[mean_pred==maxprob], # which subject has highest predicted probability 
      probsubj = mean_pred[true_subject==model] # which probability comes from the "true" model 
    ) %>% mutate(
      correct = ifelse(as.numeric(prediction)==true_subject, 1, 0)
    ) 
  df %>% filter(correct==0) %>% dplyr::select(-correct) %>% gt() %>% 
    tab_header(title = "Incorrect Subjects Summary",
               subtitle = paste("Correct prediction rate: ", round(sum(df$correct)/nrow(df), 2))) %>%
    cols_label(true_subject = "Subject",
               maxprob = "Highest Predicted Probability",
               prediction = "Predicted Subject",
               probsubj = "True Subject Predicted Probability") %>% fmt_number(columns = c(2,4), decimals=2)
}
# function to plot results 
plot_preds <- function(all_predictions, title=NULL){
  subj <- unique(all_predictions$true_subject)
  vlines <- subj
  col1 <-"firebrick3"; col2 <- "springgreen3"
  df <- all_predictions %>% group_by(true_subject, model) %>% summarize(mean_pred = mean(pred))  %>% group_by(true_subject) %>% summarize(
    maxprob = max(mean_pred),
    prediction = model[mean_pred==maxprob],
    probsubj = mean_pred[true_subject==model]
  ) %>% mutate(
    correct = ifelse(as.numeric(prediction)==true_subject, 1, 0)
  ) 
  num_wrong <- nrow(df[df$correct!=1,])
  g <-  all_predictions %>% group_by(true_subject, model) %>% summarize(mean_pred = mean(pred)) %>% mutate(
    correct = ifelse(true_subject==model, "Correct", "Incorrect"))%>% ggplot(aes(x = true_subject, y= mean_pred, col = as.factor(correct)))+theme_minimal()+
    labs(x = "Subject ID", y= "Avg. Predicted Probability", subtitle = paste(num_wrong, "Subjects Incorrect"))+ 
    scale_color_manual(name = "", values=c("Correct"=col2, "Incorrect" = col1))+
    geom_vline(xintercept=vlines, col = "lightgrey", alpha=.2)+scale_x_continuous(breaks=subj)+
    geom_jitter(size=3, alpha=.8, width = .1)+theme(legend.position="bottom")+scale_y_continuous(limits=c(0,1))
  if(is.null(title)){g}
  else{g + ggtitle(paste(title))}
}

# function to run all of the above in one line 
pipeline <- function(data, location, training_pct, time_lags, gcell_size, threshold, n_predictors){
  # get vector of subjects 
  subjects <- data %>% dplyr::select(ID2) %>% distinct() %>% unlist()
  # get grid data (predictors)  
  data_all <- map_dfr_progress(subjects, get_grid_data,
                               time_lags = time_lags, gcell_size = gcell_size, location=location, session=sessions, data=data)
  # get training and testing data based on pct supplied 
  data_train <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J <= maxJ*training_pct) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
  data_test <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J > maxJ*training_pct) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()
  # fit models 
  all_predictions <- map_dfr_progress(.x = subjects, n_predictors=n_predictors, train = data_train,
                                      test = data_test,threshold = threshold,
                                      .f = fit_model)
  # print summary 
  summarize_preds(all_predictions = all_predictions)
  # store plot 
  g <- plot_preds(all_predictions = all_predictions)
  # save predictions 
  return(list(predictions = all_predictions, plot = g, data = data_all))
}

get_roc <- function(all_predictions, individual = F){
  test_preds_wide <- all_predictions %>% group_by(model) %>% dplyr::select(-true_subject) %>% group_split() %>% bind_cols() %>% dplyr::select(-starts_with("model"))
  colnames(test_preds_wide) <- unique(all_predictions$true_subject) # rename columns to match models 
  true <- all_predictions$true_subject[1:nrow(test_preds_wide)] # get true subjects 
  multiroc <- pROC::multiclass.roc(true, as.matrix(test_preds_wide))
  if(individual == F){
    return(round(multiroc$auc, 2))
    print(round(multiroc$auc, 2))
  }
  else{
    split_df <- all_predictions %>%  mutate(class =ifelse(model==true_subject, 1, 0)) %>% group_by(model) %>% 
      group_split()
    roc_objects <- map(.x = split_df, response = class, predictor = pred, .f = pROC::roc)
    g.list <- pROC::ggroc(roc_objects)
    g <- g.list+scale_color_viridis_d(name = "Subject")+labs(x = "Specificity", y = "Sensitivity", title = "AUC Curves",
                                                             subtitle = paste("Multiple AUC:", round(multiroc$auc, 2)))+theme_minimal() 
    aucs <- map_dbl(.x = roc_objects, .f = pROC::auc) 
    print(g)
    return(list(multiroc = multiroc$auc, aucs = aucs, plot = g))
  }
}


create_moments <- function(subject, data, location) {
  loc <- paste0("signal_", location)
  data %>% filter(ID2 == subject) %>% dplyr::select(all_of(loc), J, ID2) %>% rename(signal = loc, ID = ID2) %>% group_by(J, ID) %>% summarize(
    SD = sd(signal),
    mean = mean(signal),
    m2 = moment(signal, order=2, central=T),
    m3 = moment(signal, order=3, central=T),
    m4 = moment(signal, order=4, central=T))
}

fit_randomforest <- function(subject, train, test, threshold){
  grids <- train %>% filter(ID==subject) %>% pivot_longer(cols = -ID) %>% 
    mutate(lag = sub("^.+] ", "", name)) %>% group_by(name, lag) %>% 
    summarize(value = sum(value)) %>% group_by(lag) %>% mutate(
      group_sum = sum(value)
    ) %>% ungroup() %>% mutate(
      pct = value/group_sum
    ) %>% filter(pct >= threshold) %>% dplyr::select(name)  %>% unlist() %>% unname()
  # make outcome binary 
  train$class <- ifelse(train$ID==subject, 1, 0)
  test$class <- ifelse(test$ID==subject, 1, 0)
  tmp <- train %>% dplyr::select(class, all_of(grids)) %>% janitor::clean_names()
  tmp_test <- test %>% dplyr::select(all_of(grids)) %>% janitor::clean_names()
  # hack to fix R being dumb 
  mod <- randomForest::randomForest(as.factor(class)~., data = tmp)
  pred <- predict(mod, tmp_test, type = "prob")[,2]
  return(cbind(pred, rep(subject, length(pred)), test$ID) %>% data.frame() %>% rename(model = V2, true_subject = V3))
}

fit_model_moments <- function(train, test, subject) {
  training_tmp <- train %>% mutate(class = ifelse(ID==subject, 1, 0)) %>% dplyr::select(-ID) 
  testing_tmp <- test %>% dplyr::select(-ID)
  model <- glm(class ~., data = training_tmp, family = binomial(link="logit"))
  pred <- predict(model, testing_tmp, type= "response")
  return(cbind(pred, rep(subject, length(pred)), test$ID) %>% data.frame() %>% rename(model = V2, true_subject = V3))
}


### ORIGINAL METHOD 
threshold <- 0.001
n_predictors=Inf

# training data: first 200 seconds
# testing data: next 180 seconds 
data_train <- all_data %>% group_by(ID) %>% filter(J <= 200) %>% dplyr::select(-J) %>% ungroup()
data_test <- all_data %>% group_by(ID) %>% filter(J > 200 & J<=380) %>%  dplyr::select(-J) %>% ungroup()

all_predictions <- map_dfr_progress(.x = subjects, n_predictors=n_predictors, train = data_train,
                                    test = data_test,threshold = threshold,
                                    .f = fit_model)

summarize_preds(all_predictions)

# using only 20 predictors 
all_predictions_20 <- map_dfr_progress(.x = subjects, n_predictors=20, train = data_train,
                                    test = data_test,threshold = threshold,
                                    .f = fit_model)

summarize_preds(all_predictions_20)
plot_preds(all_predictions_20, title = "20 Predictors")

# visualization
plot_preds(all_predictions, title = "Original Method")

# random forest method 
set.seed(123)
all_predictions_rf <- map_dfr_progress_progress(.x = subjects, train = data_train,
                                                test = data_test,threshold = threshold,
                                                .f = fit_randomforest)

# visualization
plot_preds(all_predictions_rf, title = "Random Forest")


## using top 30 predictors 

all_predictions_30 <- map_dfr_progress(.x = subjects, n_predictors=30, train = data_train,
                                       test = data_test,threshold = threshold,
                                       .f = fit_model)

summarize_preds(all_predictions_30)

# visualization
plot_preds(all_predictions_30, title = "30 Predictors")


moments_dat <- map_dfr_progress(.x = subjects, .f = create_moments, location = "lw", data = df_all)

# split into training and testing
train_meansd <-moments_dat %>% filter(J <= 200) %>% ungroup() %>% dplyr::select(mean, SD, ID)
test_meansd <- moments_dat %>% filter(J > 200 & J <= 380) %>% ungroup() %>% dplyr::select(mean, SD, ID)
all_predictions_msd <- map_dfr_progress(.x = subjects, train = train_meansd,
                                        test = test_meansd,
                                        .f = fit_model_moments)


# visualization
plot_preds(all_predictions_msd, title = "Mean + SD")

train_fourmom <- moments_dat %>% filter(J<= 200) %>% ungroup()  %>% dplyr::select(mean, m2, m3, m4, ID)
test_fourmom <- moments_dat %>% filter(J> 200 & J <= 380) %>%  dplyr::select(mean, m2, m3, m4, ID)
all_predictions_fourm <- map_dfr_progress(.x = subjects, train = train_fourmom,
                                          test = test_fourmom,
                                          .f = fit_model_moments)


# visualization
plot_preds(all_predictions_fourm, title = "First 4 Central Moments")


train_binary <- data_train %>% mutate(across(2:864,~ifelse(.x > 0, 1, 0)))

test_binary <- data_test %>% mutate(across(2:864,~ifelse(.x > 0, 1, 0)))

all_predictions_bin <- map_dfr_progress(.x = subjects, n_predictors=n_predictors, train = train_binary,
                                        test = test_binary,threshold = threshold,
                                        .f = fit_model)


# visualization
plot_preds(all_predictions_bin, title = "Binary Cells")



vary_num_seconds_ratio <- function(data, training_pct, total){
  data <- data %>% group_by(ID) %>% mutate(maxJ = max(J)) %>% filter(J <= 380*total)
  data_train <- data %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J <= maxJ*training_pct) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
  data_test <- data %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J > maxJ*training_pct) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()
  # fit models 
  all_predictions <- map_dfr_progress(.x = subjects, n_predictors=n_predictors, train = data_train,
                                      test = data_test,threshold = threshold,
                                      .f = fit_model)
  return(all_predictions)
}
fractions <- seq(.1, .9, .05) 
varied_seconds <- map(.x = fractions, .f = vary_num_seconds_ratio, training_pct = .53, data = all_data)

get_num_correct <- function(all_predictions){
  # group by the true subject and the model
  # for example, predictions from model k, row j is prob(row of j data is from subject k)
  # get the mean predicted probabilities and see if highest predicted prob is "correct" 
  df <- all_predictions %>% group_by(true_subject, model) %>% summarize(mean_pred = mean(pred)) %>% 
    group_by(true_subject) %>% summarize(
      maxprob = max(mean_pred),
      prediction = model[mean_pred==maxprob], # which subject has highest predicted probability 
      probsubj = mean_pred[true_subject==model] # which probability comes from the "true" model 
    ) %>% mutate(
      correct = ifelse(as.numeric(prediction)==true_subject, 1, 0)
    ) 
  return(sum(df$correct))
}

num_correct <- map(varied_seconds, get_num_correct)

names(num_correct) <- as.list(fractions)


results <- bind_rows(num_correct, .id = "id") 
results %>% t()


training <- seq(.1, .9, .05)

varied_ratios <- map(.x = training, .f = vary_num_seconds_ratio, total = 1, data = all_data)


num_correct <- map(varied_ratios, get_num_correct)

names(num_correct) <- as.list(training)

results <- bind_rows(num_correct, .id = "id") 


results %>% t() 


# get grid data
pipeline <- function(data, location, training_pct, time_lags, gcell_size, threshold, n_predictors){
  # get vector of subjects 
  subjects <- data %>% dplyr::select(ID2) %>% distinct() %>% unlist()
  # get grid data (predictors)  
  data_all <- map_dfr(subjects, get_grid_data,
                      time_lags = time_lags, gcell_size = gcell_size, location=location, data=data)
  # get training and testing data based on pct supplied 
  data_train <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J <= maxJ*training_pct) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
  data_test <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J > maxJ*training_pct & J<=380) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()
  # fit models 
  all_predictions <- map_dfr(.x = subjects, n_predictors=n_predictors, train = data_train,
                             test = data_test,threshold = threshold,
                             .f = fit_model)
  # print summary 
  summarize_preds(all_predictions = all_predictions)
  # store plot 
  g <- plot_preds(all_predictions = all_predictions)
  # save predictions 
  return(list(predictions = all_predictions, plot = g, data = data_all))
}

gc <- c(.5, .75, 1, 1.5)
grid_data <- map(.x = gc, .f = pipeline, data = df_all, location = "lw", 
                 training_pct = .53, time_lags = time_lags, threshold = 0.001, n_predictors=Inf)


summarize_preds(grid_data[[1]]$predictions) # size = 0.5
summarize_preds(grid_data[[2]]$predictions) # size = .75
summarize_preds(grid_data[[3]]$predictions) # size = 1
summarize_preds(grid_data[[4]]$predictions) # size = 1.5 


# changing lags 


# come back to this one 
lags <- seq(15, 90, 15)
# take powerset of all lags 
all_lags <- powerSetCond(as.character(lags))
fit_model_lags <- function(subjects, n_predictors, train, test, threshold, lags){
  columns <- colnames(data_train)
  column_lags <- sub("^.+] ", "", columns)
  selected_cols <- columns[which(column_lags %in% lags)]
  grids <- train %>% filter(ID == subject)  %>% dplyr::select(c(ID), all_of(selected_cols)) %>% pivot_longer(cols = -ID) %>%
    mutate(lag = sub("^.+] ", "", name)) %>% 
    group_by(name, lag) %>% filter(lag %in% lags) %>% 
    summarize(value = sum(value)) %>% group_by(lag) %>% mutate(group_sum = sum(value)) %>% ungroup() %>% mutate(pct = value /
                                                                                                                  group_sum) %>% filter(pct >= threshold) %>% dplyr::select(name)  %>% unlist() %>% unname()
  # make outcome binary
  train$class <- ifelse(train$ID == subject, 1, 0)
  test$class <- ifelse(test$ID == subject, 1, 0)
  # with the training data, fit a logistic regression with each predictor individually and get p-value
  # select __ lowest p values based on n_predictors input
  # if we want to use all predictors, then we set n_predictors=Inf
  if (!is.infinite(n_predictors)) {
    imp_grids <- train %>% dplyr::select(all_of(grids)) %>%
      map( ~ glm(
        train$class ~ .x,
        data = train,
        family = binomial(link = "logit")
      )) %>%
      map(summary.glm) %>% map(c("coefficients")) %>% map_dbl(8) %>% as.data.frame() %>% rename("pval" = ".") %>% slice_min(pval, n =
                                                                                                                              n_predictors) %>% rownames()
  }
  else{
    imp_grids <- grids
  }
  # select important grids from train and test data
  tmp <- train %>% dplyr::select(c(class, all_of(imp_grids)))
  tmp_test <- test %>% dplyr::select(all_of(imp_grids))
  # fit logistic regression
  mod <- glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  # extract predictors
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  # return data frame with predictions, model, and subject
  return(
    cbind(pred, rep(subject, length(pred)), test$ID) %>% data.frame() %>% rename("model" = "V2", "true_subject" = "V3")
  )
  
  # function that returns all results for one set of lags 
}

pipeline_lags <- function(data_train, data_test, lags){
  columns <- colnames(data_train)
  column_lags <- sub("^.+] ", "", columns)
  selected_cols <- columns[which(column_lags %in% lags)]
  train_tmp <- data_train %>% dplyr::select(c(ID, all_of(selected_cols)))
  test_tmp <- data_test %>% dplyr::select(c(ID, all_of(selected_cols)))
  subjects <- unique(data_train$ID)
  all_predictions <- map_dfr(.x = subjects, n_predictors=Inf, train = train_tmp,
                             test = test_tmp,threshold = 0.001,
                             .f = fit_model)
  return(predictions = all_predictions)
}

res <- map(.f = pipeline_lags, data_train = data_train, data_test=data_test, .x=all_lags)

correct_lags <- map(.f = get_num_correct, .x = res)
names(correct_lags) <- all_lags

corr_lags <- bind_rows(correct_lags, .id = "ID") %>% t()%>% data.frame()

