library(tidyverse) 
library(purrr)
library(broom)
library(gt) 

options(dplyr.summarise.inform = FALSE)
`%notin%` <- Negate(`%in%`)
location <- "lw"

get_grid_data  <- function(subject, time_lags, gcell_size, location, data){
  df <- data %>% dplyr::select(ID2, paste("signal_", location, sep = ""), t, J) %>% 
    rename("signal" = paste("signal_", location, sep = ""),
           "ID" = ID2)
  # max_signal <- round(max(df$signal), 0) 
  # to replicate our data we set to 3 
  max_signal <- 3
  tmp <- df %>% filter(ID == subject) # filter to subject 
  n <- max(tmp$J) # number of total seconds for that subject 
  seconds <- rep(seq(1, n, 1), each=length(time_lags)) # vector of seconds and lags so that we can iterate over both
  lags <- rep(time_lags, n) # vector of lags 
  get_grid_data_onesubj <- function(second, lag){
    # function that gets number of points in each "grid cell" for a given second and given lag 
    tmp %>% filter(J==second) %>% dplyr::select(signal, t) %>% mutate(
      lag_signal = lag(signal, n = lag)  # for each second, calculate signal and lagged signal 
    ) %>% filter(!is.na(lag_signal)) %>% mutate(cut_sig = cut(signal, breaks = seq(0, max_signal, by = gcell_size), include.lowest = T),
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
      map(summary.glm) %>% map(c("coefficients")) %>% map_dbl(8) %>% as.data.frame() %>% rename("pval" = ".") %>% slice_min(pval, n=n_predictors) %>% rownames()
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
  return(cbind(pred, rep(subject, length(pred)), test$ID) %>% data.frame() %>% rename("model" = "V2", "true_subject" = "V3"))
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

# funtction to run all of the above in one line 
pipeline <- function(data, location, training_pct, time_lags, gcell_size, threshold, n_predictors){
  # get vector of subjects 
  subjects <- data %>% dplyr::select(ID2) %>% distinct() %>% unlist()
  # get grid data (predictors)  
  data_all <- map_dfr(subjects, get_grid_data,
                      time_lags = time_lags, gcell_size = gcell_size, location=location, session=sessions, data=data)
  # get training and testing data based on pct supplied 
  data_train <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J <= maxJ*training_pct) %>% dplyr::select(-c(J, maxJ)) %>% ungroup()
  data_test <- data_all %>% group_by(ID) %>% 
    mutate( maxJ = max(J)) %>% filter(J > maxJ*training_pct) %>%  dplyr::select(-c(J, maxJ)) %>% ungroup()
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
