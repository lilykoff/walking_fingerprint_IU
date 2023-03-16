## generating figures for manuscript 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
# Raw and accelerometry data
plot_rawdata <- function(data, subject, seconds=20){
    sub <- data %>% filter(ID2==subject) %>% dplyr::select(time_s_2, lw_x, lw_y, lw_z, signal_lw)
    # raw 20 seconds of time series data
    p1 <- sub %>%
      filter(time_s_2 <= seconds) %>% rename(c("x"="lw_x", "y"="lw_y", "z"="lw_z")) %>%
      gather(key = Axis, value = Amplitude, 
             c("x", "y", "z")) %>% 
      ggplot(aes(x=time_s_2, y = Amplitude, group = Axis, colour = Axis)) + 
      geom_line(linewidth=0.3) +
      facet_grid(Axis~., scales = "free_y") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    
    
    p2 <- sub %>% 
      filter(time_s_2 <= seconds) %>%
      ggplot(aes(x=time_s_2, y = signal_lw)) + 
      geom_line(linewidth=0.3) +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      geom_vline(xintercept=c(2, 3), color="darkorange", linetype="solid", size=0.6) +
      geom_vline(xintercept=c(10, 15), color="purple", linetype="solid", size=0.6) +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    p3 <- sub %>% 
      filter(time_s_2>=2 & time_s_2 <=3) %>%
      ggplot(aes(x=time_s_2, y = signal_lw)) + 
      geom_line(linewidth=0.5, color="darkorange") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    
    p4 <- sub %>% 
      filter(time_s_2 >= 10 & time_s_2 <= 15) %>%
      ggplot(aes(x=time_s_2, y = signal_lw)) + 
      geom_line(linewidth=0.4, color="purple") +
      theme_classic() +
      theme(plot.title = element_text(hjust=0.5)) +
      theme(legend.position = "none") +
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")"))
    ggpubr::ggarrange(p1, p3, p2, p4, ncol = 2, nrow = 2)

}


# Translation of time series and creating 3D images
# input lags in ms
plot_3D <- function(subject_id, data, time_lags=c(15, 30)){
  train_j <- 1:200
  df_train <- subset(data, J %in% train_j)
    ## lagged plots 
    # first lag 
    df <- df_train %>% filter(time_s_2>=2 & time_s_2 <= 3+(time_lags[1]/100) & ID2==subject_id) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
      lag_signal = lag(signal_lw, n = time_lags[1])) 
    point1_x <- df_train %>% filter(time_s_2>=2 & time_s_2 <= 3+(time_lags[1]/100) & ID2==subject_id) %>% 
      dplyr::select(time_s_2, signal_lw) %>% mutate(
        lag_signal = lag(signal_lw, n = time_lags[1])) %>% filter(time_s_2==2+(.01+time_lags[1]/100)) 
    sig <- point1_x$signal_lw; lagsig <- point1_x$lag_signal
    p1 <- df %>% pivot_longer(cols = -time_s_2) %>% 
      mutate(name = ifelse(name == "signal_lw", "Original: v(s)", "Lagged: v(s-u)")) %>%
      ggplot(aes(x=time_s_2, y= value, col = name, linetype = name)) + 
      geom_line()+
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
    scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.15))+
      geom_vline(xintercept=point1_x$time_s_2, color="brown2") +
      geom_point(aes(x=point1_x$time_s_2, y = sig), color="brown2", size=3) +
      geom_point(aes(x=point1_x$time_s_2, y = lagsig), color="brown2", size=3) 
    
    
    df_dens <- df_train %>% filter(J <= 200 & ID2==subject_id) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
      lag_signal = lag(signal_lw, n = time_lags[1])) %>% drop_na()
    
    df_dens$density <- get_density(df_dens$signal_lw, df_dens$lag_signal, n=100)
    
    
    p3 <- ggplot(df_dens) +
      geom_point(aes(x=lag_signal, y=signal_lw, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() + 
      geom_point(data=(df %>% filter(time_s_2==(2+time_lags[1]/100))), aes(x=lag_signal, y=signal_lw), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) 
    
    
    df2 <- df_train %>% filter(time_s_2>=2 & time_s_2 <= 3+(time_lags[2]/100) & ID2==subject_id) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
      lag_signal = lag(signal_lw, n = time_lags[2]))
    point2_x <- df_train %>% filter(time_s_2>=2 & time_s_2 <= 3 + time_lags[2]/100 & ID2==subject_id) %>% 
      dplyr::select(time_s_2, signal_lw) %>% mutate(
        lag_signal = lag(signal_lw, n = time_lags[2])) %>% filter(time_s_2==2 + time_lags[2]/100) 
    sig <- point2_x$signal_lw; lagsig <- point2_x$lag_signal
    
    # second lag 
    p2 <- df2 %>% pivot_longer(cols = -time_s_2) %>% 
      mutate(name = ifelse(name == "signal_lw", "Original: v(s)", "Lagged: v(s-u)")) %>%
      ggplot(aes(x=time_s_2, y= value, col = name, linetype = name)) + 
      geom_line()+
      scale_linetype_manual(values=c("Original: v(s)"= "solid","Lagged: v(s-u)"="dashed")) +
      scale_color_manual(values = c("Original: v(s)" = "black", "Lagged: v(s-u)" = "black")) +
      labs(linetype = "Time Series") +
      guides(color = "none")+theme_minimal()+
      labs(x="Time (s)", y=expression("Magnitude of Acceleration" ~ "(" ~ m/s^2 ~ ")")) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_x_continuous(breaks=c(2.00,2.15,2.30,2.45,2.60,2.75,2.90,3.05,3.30))+
      geom_vline(xintercept=point2_x$time_s_2, color="brown2") +
      geom_point(aes(x=point2_x$time_s_2, y = sig), color="brown2", size=3) +
      geom_point(aes(x=point2_x$time_s_2, y = lagsig), color="brown2", size=3) 
    # full density
    
    df2_dens <- df_train %>% filter(J <= 200 & ID2==subject_id) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
      lag_signal = lag(signal_lw, n = time_lags[2])) %>% drop_na()
    
    df2_dens$density <- get_density(df2_dens$signal_lw, df2_dens$lag_signal, n=100)
    
    p4 <- ggplot(df2_dens) +
      geom_point(aes(x=lag_signal, y=signal_lw, color=density)) +
      labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
           y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
      scale_color_viridis(name = "Density", limits=c(0,2.0)) +
      theme_bw() + 
      geom_point(data=(df %>% filter(time_s_2==(2+time_lags[2]/100))), aes(x=lag_signal, y=signal_lw), color="brown2", size=4) +
      theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
      scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) +
      scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                         minor_breaks = seq(0, 3, 0.25)) 
  
    ggpubr::ggarrange(p1+labs(title=paste("Time lag of ", time_lags[1]/100, "s", sep="")), 
              p2+labs(title=paste("Time lag of ", time_lags[2]/100, "s", sep="")), 
              p3, p4, ncol = 2, nrow = 2)
}

# Grid cells with counts of observations
# input lag in ms
plot_counts <- function(subject_id, data, time_lag=30){
  train_j <- 1:200
  df_train <- subset(data, J %in% train_j)
  
  df <- df_train %>% filter(J <= 200 & ID2==subject_id) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
    lag_signal = lag(signal_lw, n = time_lag)) %>% drop_na()
  
  df$density <- get_density(df$signal_lw, df$lag_signal, n=100)
  
  p1 <- ggplot(df) +
    geom_point(aes(x=lag_signal, y=signal_lw, color=density)) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) +
    scale_color_viridis(name = "Density", limits=c(0,2.0)) +
    theme_bw() + 
    theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) 
  
  count <- df %>% mutate(cut_s = cut(signal_lw, breaks = seq(0, 3, by = 0.25), include.lowest = T),
                             cut_u = cut(lag_signal, breaks = seq(0, 3, by = 0.25), include.lowest = T)) %>% 
    drop_na() %>%
    count(cut_s, cut_u, .drop=FALSE) %>% mutate(per=n/sum(n))
  countp <- count %>% mutate(pern=case_when(per>=0.001~1, per<0.001~2))
  countp$n <- ifelse(countp$pern==2, 0, countp$n)
  
  # colored grids
  p2 <- ggplot(data=count, aes(x=cut_u, y=cut_s)) +
    geom_tile(aes(fill=n), color="grey", size=0.3) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")"),
         fill="Number of Observations") +
    geom_text(aes(label = n), color = "white", size = 2.5) +
    scale_fill_gradientn(colors = c("white", "#443A83","#27AD81", "#FDE333"), values=c(0, 0.0001, 0.5, 1))
  
  p3 <- ggplot(data=countp, aes(x=cut_u, y=cut_s)) +
    geom_tile(aes(fill=n), color="grey", size=0.3) +
    labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
         y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")"),
         fill="Key Predictors") +
    geom_text(aes(label = n), color = "white", size = 2.5) +
    scale_fill_gradientn(colors = c("white", "#453781","#27AD81","#FDE333"), values=c(0, 0.0001, 0.5, 1))
  
  ggpubr::ggarrange(p1+labs(title=paste("Time lag of ", time_lag/100, "s", sep=""))+theme(axis.title=element_text(size=8)), 
            p2+labs(title="Counts in each grid cell")+theme(axis.text=element_text(size=3.2), axis.title=element_text(size=8)), 
            p3+labs(title=paste("Key predictors for subject ", subject_id, sep=""))+theme(axis.text=element_text(size=3.2), axis.title=element_text(size=8)), 
            ncol = 3, nrow = 1, legend="none")
}


# plot results fn
plot_results <- function(all_predictions){ 
  subj <- unique(all_predictions$true_subject)
  col1 <- "grey"
  col2 <- "coral1"
  all_predictions %>% group_by(true_subject, model) %>% summarize(mean_pred = mean(pred)) %>% mutate(
    correct = ifelse(true_subject==model, "Correct", "Incorrect"))%>% ggplot(aes(x = true_subject, y= mean_pred, col = as.factor(correct)))+theme_minimal()+
    labs(x = "Subject", y = "Probability", title= "One vs. Rest Logistic Regression Results", subtitle = "100% predictive accuracy")+ 
    scale_color_manual(name = "", values=c("Correct"=col2, "Incorrect" = col1))+
    scale_x_continuous(breaks=subj)+
    geom_point(size=1.3, alpha=.8)+scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, .2))+
    theme_classic()+theme(legend.position="none")+ theme(plot.title = element_text(hjust = 0.5), 
                                                         plot.subtitle = element_text(hjust = 0.5))
}

# figure 4 function to generate full fingerprint for one subject given lags 

fingerprint <- function(subject, lags, data){
  onelag <- function(subject, lag, data){
    df_dens <- data %>% filter(ID2==subject) %>% dplyr::select(time_s_2, signal_lw) %>% mutate(
      lag_signal = lag(signal_lw, n = lag)) %>% drop_na() %>% mutate(
        lag = lag 
      )
    df_dens$density <- get_density(df_dens$signal_lw, df_dens$lag_signal, n=100)
    df_dens
  }
  
  all_densities <- map_dfr(.x = seq(15,90,15), .f = onelag, data = data, subject=21) 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_lw, col = density))+geom_point() + 
    scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
    theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x=expression("Magnitude of Acceleration from v(s-u)" ~ "(" ~ m/s^2 ~ ")"), 
                                       y=expression("Magnitude of Acceleration from v(s)" ~ "(" ~ m/s^2 ~ ")")) 
  
}

# just training data 
data <- df_all %>% filter(J <= 200) 
selected_subs <- c(21, 18, 31, 28)

fingerprints <- map(.x = selected_subs, .f = fingerprint,lags = seq(15, 90, 15), data = data)

ggpubr::ggarrange(plotlist = fingerprints, nrow=4)
sub <- c(21, 18, 31, 28)
ul <-  c(2.5, 2.5, 2.5, 4)
plots <- list()
for (i in seq_along(sub)){
  plots[[i]] <- fingerprints(sub[i], ul[i], df_train)
}
ggarrange(plotlist=plots, nrow=4)



# generate figure 1
subject <- 21

plot_rawdata(df_all, subject=subject, seconds = 20)


# figure 2
plot_3D(subject_id = 21, data = df_all, time_lags=c(15, 30))


# plotting the predicted probabilities (figure 3)
plot_results(all_predictions)


# counts 
plot_counts(subject, df_all, time_lag=30)


# figure 4 
data <- df_all %>% filter(J <= 200) 
selected_subs <- c(21, 18, 31, 28)

fingerprints <- map(.x = selected_subs, .f = fingerprint,lags = seq(15, 90, 15), data = data)

ggpubr::ggarrange(plotlist = fingerprints, nrow=4)


# supplemental figures can be generated using fingerprints function as well 


