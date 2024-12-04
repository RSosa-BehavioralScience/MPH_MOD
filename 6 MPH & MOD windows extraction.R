MM_windows_source <- MM_processed_database

#Set the extension of the sliding window (units are 1/20 seconds)
window_width <- 9600

#Set the duration of the session
session_duration <- 48000

#Set the length of window slide
slide_resolution <- 400

#We create a progress bar to monitor processing, because the routine below
#...takes a while
grand_pb <- txtProgressBar(min = 0,
                           max = length(MM_windows_source),
                           style = 3,
                           width = 200,
                           char = "=")

inverse_sigmoid <- function(x, B, Q, nu) {
  return(1 - 1/(1 + Q * exp(B * x))^(1/nu))
}

#Routine to extract behavioral indices for window slices in each session 
#...of the experiment
for (x in 1:length(MM_windows_source)){
  
  setTxtProgressBar(grand_pb, x)
  
  for (y in seq(from=window_width, to=session_duration, by=slide_resolution)){
    #This line extracts each window of specified length from the source
    window <- 
      which(MM_windows_source[[x]]$behavior$timestamp > (y-window_width) & MM_windows_source[[x]]$behavior$timestamp < y)
    #This objects marks the current window number
    window_n <- which(seq(from=window_width, to=session_duration, by=slide_resolution) == y)
    #This line appends the window data as a sublist
    MM_windows_source[[x]]$windows[[window_n]] <- list()
    #The lines below extract values of interest from the current window
    #Then, this will repeat for all sessions in the source list
    iei_wd <- MM_windows_source[[x]]$behavior$i_e_i[window]
    
    n_rew <- length(which(MM_windows_source[[x]]$behavior$i_e_i[window] >= 15))
    
    MM_windows_source[[x]]$windows[[window_n]]$rewards <- n_rew
    
    n_non_rew <- length(which(MM_windows_source[[x]]$behavior$i_e_i[window] < 15))
    
    MM_windows_source[[x]]$windows[[window_n]]$efficiency <- log10((n_rew+1)/(n_non_rew+1))
    
    n_burst <- length(which(MM_windows_source[[x]]$behavior$class[window] == "burst"))
    
    n_non_b <- length(which(MM_windows_source[[x]]$behavior$class[window] != "burst"))
    
    MM_windows_source[[x]]$windows[[window_n]]$burst_ratio <- 
      log10((n_burst+1)/(n_non_b+1))
    
    if(length(MM_windows_source[[x]]$behavior$i_e_i[window][which(MM_windows_source[[x]]$behavior$i_e_i[window] >= MM_windows_source[[x]]$t1)]) < 2){
      MM_windows_source[[x]]$windows[[window_n]]$timed_peak <- NA
    }else{
      dens_nb <- 
      density(MM_windows_source[[x]]$behavior$i_e_i[window][which(MM_windows_source[[x]]$behavior$i_e_i[window] >= MM_windows_source[[x]]$t1)], adjust = 1)
      nb_peak <- dens_nb$x[dens_nb$y==max(dens_nb$y)]
      MM_windows_source[[x]]$windows[[window_n]]$timed_peak <- nb_peak}
    
    t1 <- MM_windows_source[[x]]$t1
    
    t2 <- MM_windows_source[[x]]$t2
    
    timed_iris <-
      MM_windows_source[[x]]$behavior$i_e_i[window][which(MM_windows_source[[x]]$behavior$i_e_i[window] > t1 & MM_windows_source[[x]]$behavior$i_e_i[window] < t2)]
   
    MM_windows_source[[x]]$windows[[window_n]]$timing_precision <- sd(timed_iris)/mean(timed_iris)
    
    n_reward_timed <- length(timed_iris[which(timed_iris >= 15)])
    
    early_inh_def_r <- 
      MM_processed_database[[x]]$behavior$i_e_i[window][which(MM_processed_database[[x]]$behavior$class[window] != "burst" & MM_processed_database[[x]]$behavior$i_e_i[window] < t1)]
    
    eid_plus_timed_r <- 
      MM_processed_database[[x]]$behavior$i_e_i[window][which(MM_processed_database[[x]]$behavior$class[window] != "burst" & MM_processed_database[[x]]$behavior$i_e_i[window] < t2)]
    
    prop_eid <- length(early_inh_def_r)/length(eid_plus_timed_r)
    
    mean_eid <- inverse_sigmoid(mean(ear_inh_def_r), B=-0.78092115, Q=45.01493555, nu=0.04378572)
    
    MM_windows_source[[x]]$windows[[window_n]]$early_inh_def <- prop_eid*mean_eid
    
    left_trunc_t <- MM_windows_source[[x]]$behavior$i_e_i[window][which(MM_windows_source[[x]]$behavior$i_e_i[window] > t1)]
    
    MM_windows_source[[x]]$windows[[window_n]]$att_lapses <-
      mean(left_trunc_t) - mean(timed_iris)
    
  }
}  

close(pb)

MM_sliding_windows <- vector(mode = "list", length = length(MM_windows_source))

for (i in 1:length(MM_windows_source)){
  MM_sliding_windows[[i]] <- vector(mode = "list", length = length(MM_windows_source[[i]]$windows))
  for (n in 1:length(MM_windows_source[[1]]$windows)){
    MM_sliding_windows[[i]][[n]]$window <-
      n
    MM_sliding_windows[[i]][[n]]$subject <-
      MM_windows_source[[i]]$subject
    MM_sliding_windows[[i]][[n]]$group <-
      MM_windows_source[[i]]$group
    MM_sliding_windows[[i]][[n]]$phase <-
      MM_windows_source[[i]]$phase
    MM_sliding_windows[[i]][[n]]$session <-
      MM_windows_source[[i]]$session
    MM_sliding_windows[[i]][[n]]$box <-
      MM_windows_source[[i]]$box
    MM_sliding_windows[[i]][[n]]$researcer <-
      MM_windows_source[[i]]$researcher
    MM_sliding_windows[[i]][[n]]$start_time <-
      MM_windows_source[[i]]$start_time
    MM_sliding_windows[[i]][[n]]$rewards <-
      MM_windows_source[[i]]$windows[[n]]$rewards
    MM_sliding_windows[[i]][[n]]$efficiency <-
      MM_windows_source[[i]]$windows[[n]]$efficiency
    MM_sliding_windows[[i]][[n]]$burst_ratio <-
      MM_windows_source[[i]]$windows[[n]]$burst_ratio
    MM_sliding_windows[[i]][[n]]$timed_peak <-
      MM_windows_source[[i]]$windows[[n]]$timed_peak
    MM_sliding_windows[[i]][[n]]$timing_precision <-
      MM_windows_source[[i]]$windows[[n]]$timing_precision
    MM_sliding_windows[[i]][[n]]$early_inh_def <-
      MM_windows_source[[i]]$windows[[n]]$early_inh_def
    MM_sliding_windows[[i]][[n]]$att_lapses <-
      MM_windows_source[[i]]$windows[[n]]$att_lapses
  }
}

MM_windows_ready <- data.frame(matrix(NA, nrow = (length(MM_sliding_windows)*length(MM_windows_source[[1]]$windows)), 
                                      ncol = length(MM_sliding_windows[[1]][[1]])))

k <- 1

for (i in 1:length(MM_sliding_windows)){
  for (j in 1:length(MM_windows_source[[1]]$windows)){
    MM_windows_ready[k , ] <- unlist(MM_sliding_windows[[i]][[j]])
    k <- k + 1
  }   
}

colnames(MM_windows_ready) <- c("window", "subject", "group", "phase", "session", "box", "researcher", 
                                "start_time", "rewards", "efficiency", "burst_ratio", "timed_peak", 
                                "timed_spread", "early_inh_def", "att_lapses")

MM_windows_ready$rewards <- as.numeric(MM_windows_ready$rewards)
MM_windows_ready$efficiency <- as.numeric(MM_windows_ready$efficiency)
MM_windows_ready$burst_ratio <- as.numeric(MM_windows_ready$burst_ratio)
MM_windows_ready$timed_peak <- as.numeric(MM_windows_ready$timed_peak)
MM_windows_ready$timed_spread <- as.numeric(MM_windows_ready$timed_spread)
MM_windows_ready$early_inh_def <- as.numeric(MM_windows_ready$early_inh_def)
MM_windows_ready$att_lapses <- as.numeric(MM_windows_ready$att_lapses)
MM_windows_ready$window <- as.numeric(MM_windows_ready$window)

write.csv(MM_windows_ready, file = "windows.csv")

#Code for generating Figure S2

windows_analysis$p_t1 <- as.numeric(rep(NA, nrow(windows_analysis)))
windows_analysis$p_t2 <- as.numeric(rep(NA, nrow(windows_analysis)))
windows_analysis$p_t3 <- as.numeric(rep(NA, nrow(windows_analysis)))

for (i in 1:nrow(windows_analysis)){
  if (!is.na(windows_analysis$timed_spread[i])){
    if (windows_analysis$timed_spread[i] <= quantile(na.omit(windows_analysis$timed_spread), .05)){
      windows_analysis$p_t1[i] <- windows_analysis$timed_peak[i]
    }
    if (windows_analysis$timed_spread[i] >= quantile(na.omit(windows_analysis$timed_spread), .95)){
      windows_analysis$p_t3[i] <- windows_analysis$timed_peak[i]
    }
    if (windows_analysis$timed_spread[i] < quantile(na.omit(windows_analysis$timed_spread), .6667) & windows_analysis$timed_spread[i] > quantile(na.omit(windows_analysis$timed_spread), .3333)){
      windows_analysis$p_t2[i] <- windows_analysis$timed_peak[i]
    }}
}

#Warning: Generating this plot may take a significant amount of time, and saving the result as 
#...an SVG or similar format will produce a relatively large file
ggplot(windows_analysis, aes(x = timed_peak, y = rewards)) +
  geom_point(data = subset(windows_analysis, !is.na(p_t2)), 
             aes(x = p_t2, y = rewards, color = "Intermediate Values"), alpha = 0.8, size = 10) +
  geom_point(data = subset(windows_analysis, !is.na(p_t1)), 
             aes(x = p_t1, y = rewards, color = "5% Lower Spread"), alpha = 0.8, size = 10) +
  geom_point(data = subset(windows_analysis, !is.na(p_t3)), 
             aes(x = p_t3, y = rewards, color = "5% Higher Spread"), alpha = 0.3, size = 10) +
  geom_vline(aes(xintercept = 15), color = "black", linetype = "dashed", size = 1.8) +
  scale_color_manual(values = c("5% Lower Spread" = "yellow",
                                "5% Higher Spread" = "purple",
                                "Intermediate Values" = "gray"),
                     name = NULL,
                     breaks = c("5% Lower Spread", "5% Higher Spread", "Intermediate Values")) + # Setting the order here
  theme_classic() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = c(0.9, 0.9)) +
  xlab("Timed Peak") +
  ylab("Obtained Rewards") 
#Recommended visualization dimensions: 900 (width) x 600 (height)


#Now, we can proceed wih script number 7
