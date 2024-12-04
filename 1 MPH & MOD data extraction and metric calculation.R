#You first need to download the data files to your computer (i.e., the file named "M&M project raw data")
#Write within the quotation marks the path to the location of the folder in which you saved the files
###IMPORTANT: Do not use the 'M&M project raw data complete' file in this script.
setwd("type here the path to the files in your computer")
#If you get an error message, you may have to invert the direction the 
#backslashes in the file path 

#Create a vector with all the file names
MM_file_names <- list.files(recursive = TRUE)

#Create a list to store all the files
MM_files <- list()

#Feed the newly-created list with all the files labeled with the file names
for (i in 1:length(MM_file_names)){
  MM_files[[i]] <- list()
  MM_files[[i]][[1]] <- MM_file_names[i]
  MM_files[[i]][[2]] <- read.table (MM_file_names[i],fileEncoding="UTF-8-BOM", fill = TRUE)
}

#Create a progress bar object to obtain feedback about the longest of the processes featured for the analysis
pb <- txtProgressBar(min = 0,
                     max = length(MM_file_names),
                     style = 3,
                     width = 100,
                     char = "=")

#Create another list to extract the relevant raw data from the files
MM_database_raw_list <- vector(mode = "list", length = length(MM_file_names))

#Seek and extract some variables of interest, as well as the main dependent variable (it takes a while)
for (i in 1:length(MM_file_names)){
  setTxtProgressBar(pb, i)
  locator <- unlist(gregexpr('_', MM_files[[i]][[1]]))[1]
  MM_database_raw_list[[i]][["ID"]] <- MM_files[[i]][[1]]
  MM_database_raw_list[[i]][["subject"]] <- substring(MM_files[[i]][[1]],locator-3, locator-1)
  MM_database_raw_list[[i]][["researcher"]] <- substring(MM_files[[i]][[1]],locator+1, locator+2)
  MM_database_raw_list[[i]][["group"]] <- substring(MM_files[[i]][[1]],locator+4, locator+6)
  MM_database_raw_list[[i]][["phase"]] <- substring(MM_files[[i]][[1]],locator+15, locator+15)
  MM_database_raw_list[[i]][["session"]] <- substring(MM_files[[i]][[1]],locator+16, locator+16)
  MM_database_raw_list[[i]][["start_time"]] <- MM_files[[i]][[2]][8, 3]
  MM_database_raw_list[[i]][["box"]] <- MM_files[[i]][[2]][7, 2]
  behavior <- data.frame(matrix(NA, ncol=2, nrow = 0)) 
  locator2 <- which(MM_files[[i]][[2]] == "T:")
  for (j in (locator2+1):(nrow(MM_files[[i]][[2]]))){
    for (k in 2:6){
      datum <- strsplit(as.character(MM_files[[i]][[2]][j, k]),"\\.")
      timestamp <- datum[[1]][1]
      event <- datum[[1]][2]
      behavior <- rbind(behavior, c(timestamp, event))
    }
  }
  colnames(behavior) <- c("timestamp", "event")
  MM_database_raw_list[[i]][["behavior"]] <- behavior
}

close(pb)

#Create another list for keeping only the sessions we are going to analyze
MM_database_curated_list <- vector(mode = "list", length = length(MM_database_raw_list))

#Remove files that are not of interest
for (i in 1:length(MM_database_raw_list)){
  if (MM_database_raw_list[[i]]$subject != "R07" & MM_database_raw_list[[i]]$subject != "R24" & MM_database_raw_list[[i]]$subject != "R29" & !(MM_database_raw_list[[i]]$subject == "R11" & MM_database_raw_list[[i]]$phase == "d" & MM_database_raw_list[[i]]$session == 5) & MM_database_raw_list[[i]]$session != 1 & !(MM_database_raw_list[[i]]$phase =="w" & MM_database_raw_list[[i]]$session == 2)){
    MM_database_curated_list[[i]] <- list()
    MM_database_curated_list[[i]] <- MM_database_raw_list[[i]]
  }
}

#Eliminate empty sublists of this list of lists
MM_database_curated_list<-MM_database_curated_list[!sapply(MM_database_curated_list,is.null)]

#Create a new list that will hold the behavioral indices of interest
MM_processed_database <- MM_database_curated_list

#Create a function to obtain an inverse sigmoid function to weight responses 
#indicative of early inhibitory deficit
inverse_sigmoid <- function(x, B, Q, nu) {
  return(1 - 1/(1 + Q * exp(B * x))^(1/nu))
}

#Massive multiply nested routine to perform IRT classification and compute behavioral indices for behavioral data
for (x in 1:length(MM_processed_database)){
  MM_processed_database[[x]]$behavior <- na.omit(MM_processed_database[[x]]$behavior)
  MM_processed_database[[x]]$behavior$timestamp <- as.numeric(MM_processed_database[[x]]$behavior$timestamp)
  MM_processed_database[[x]]$behavior$event <- as.numeric(MM_processed_database[[x]]$behavior$event)
  #Create a counter index to scan behavioral data according to the length of the data columns
  c <- nrow(MM_processed_database[[x]]$behavior)
  #This subroutine searches for timestamps labeled as reward deliveries (e.g., event 218)
  #...and then creates a new event (111) to designate the reinsertion of the lever after the
  #...food consumption period (4 seconds). Note that time is coded in 1/20 second units.
  for (i in 1:c) {
    if (MM_processed_database[[x]]$behavior$event[i] == 218) {
      MM_processed_database[[x]]$behavior <- rbind(MM_processed_database[[x]]$behavior, c((MM_processed_database[[x]]$behavior$timestamp[i]+80),111))
    } 
  }
  #Sort the data to put the new event in its corresponding place within the session
  MM_processed_database[[x]]$behavior <- MM_processed_database[[x]]$behavior[order(MM_processed_database[[x]]$behavior$timestamp),]
  rownames(MM_processed_database[[x]]$behavior) <- 1:nrow(MM_processed_database[[x]]$behavior)
  #Now, we create a vector in which IRT classifications will be stored
  class <- character(length(MM_processed_database[[x]]$behavior$event))
  #Our first criterion for discarding IRTs as burst responses is to determine
  #...whether they followed a lever press or another event (e.g., food groove 
  #...approach [event 61 or 610], lever reinsertion[event 111 or 531]). In the 
  #...later case, IRTs are labeled as "non_burst".
  for (i in 2:length(MM_processed_database[[x]]$behavior$event)) {
    if (MM_processed_database[[x]]$behavior$event[i] == 307) {
      if (MM_processed_database[[x]]$behavior$event[i-1] == 61 | 
          MM_processed_database[[x]]$behavior$event[i-1] == 610 |
          MM_processed_database[[x]]$behavior$event[i-1] == 111 |
          MM_processed_database[[x]]$behavior$event[i-1] == 218 |
          MM_processed_database[[x]]$behavior$event[i-1] == 531){
        class[i] <- "non_burst"  
      }
    }
  }
  #Then, we add the vector to the data frame that contains the remaining of behavioral data.
  MM_processed_database[[x]]$behavior$class <- class
  #Once that they have served their marking function, we can remove data points from events 
  #...that are not lever presses [i.e., food groove approaches, food delivery, session
  #initiation, lever insertion, session finalization].
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 61)
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 610)
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 155)
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 531)
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 218)
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 31)
  #So, we create a new empty vector to store IRTs.
  i_e_i <- numeric(length(MM_processed_database[[x]]$behavior$timestamp))
  #The first data point needs a special treatment because it does not have a
  #...preceeding event to compute inter-event-interval as usual.
  i_e_i[1] <- MM_processed_database[[x]]$behavior$timestamp[1]/20
  #Here, we compute inter event intervals as usual; that is, subtracting
  #...the timestamp of the preceding event from the timestamp of a given event.
  for (i in 2:length(MM_processed_database[[x]]$behavior$timestamp)) {
    i_e_i[i] <- (MM_processed_database[[x]]$behavior$timestamp[i] - MM_processed_database[[x]]$behavior$timestamp[i-1])/20
  }
  #Then, we append the vector with the complete batch of IRTs to the behavior data frame.
  MM_processed_database[[x]]$behavior$i_e_i <- i_e_i
  
  MM_processed_database[[x]]$behavior <- subset(MM_processed_database[[x]]$behavior, event != 111)
  #This line creates a high-resolution density distribution form the full set of IRTs
  #...with the aim of searching for an antimode between burst and non burst IRT
  #...distribution.
  dens <- density(MM_processed_database[[x]]$behavior$i_e_i, adjust = .02)
  #For that, we reduce the search space to values ranging from zero to 5 s
  #...and look for the minimum value within that range
  antimode <- dens$x[which.min(dens$y[which(dens$x > 0 & dens$x < 5)]) + length(dens$y[which(dens$x < 0)])]
  #Save the antimode of each subject x session in case it is required later 
  MM_processed_database[[x]]$antimode <- antimode
  MM_processed_database[[x]]$antimode <- as.numeric(sapply(MM_processed_database, "[[", "antimode")[x])
  #Count and save rewarded responses
  n_rew <- length(which(MM_processed_database[[x]]$behavior$i_e_i >= 15))
  MM_processed_database[[x]]$rewards <- n_rew
  #Count unrewarded responses and save total responses
  n_non_rew <- length(which(MM_processed_database[[x]]$behavior$i_e_i < 15))
  MM_processed_database[[x]]$total_respones <- n_rew+n_non_rew
  #Compute and save the efficiency index
  MM_processed_database[[x]]$efficiency <- log10((n_rew+1)/(n_non_rew+1))
  #Label IRTs less or equal to the antimode as burst IRTs, but apply this criterion only
  #...to IRTs not already classified as non-burst according to the first criterion
  MM_processed_database[[x]]$behavior$class[which(MM_processed_database[[x]]$behavior$class != "non_burst" & MM_processed_database[[x]]$behavior$i_e_i <= MM_processed_database[[x]]$antimode)] <-
    "burst"
  #Label the remaining of the IRTs as non burst IRTs
  MM_processed_database[[x]]$behavior$class[which(MM_processed_database[[x]]$behavior$class == "")] <-
    "non_burst"
  #Count burst IRTs
  n_burst <- length(which(MM_processed_database[[x]]$behavior$class == "burst"))
  #Count non burst IRTs
  n_non_b <- length(which(MM_processed_database[[x]]$behavior$class == "non_burst"))
  #Compute the burst ratio
  MM_processed_database[[x]]$burst_ratio <- 
    log10((n_burst+1)/(n_non_b+1))
  #In case there were no burst IRTs, set the antimode as the location where the density 
  #...distribution of non burst IRTs begins  
  if (n_burst < 1){
    #But in case that the density distribution of non burst IRTs started in a negative number 
    #...the antimode would be set to zero.
    if (dens_nb$x[1] < 0){
      antimode <- 0
    }else{antimode <- dens_nb$x[1]}
  }
  #Save the definitive antimode
  MM_processed_database[[x]]$antimode <- antimode
  MM_processed_database[[x]]$antimode <- as.numeric(sapply(MM_processed_database, "[[", "antimode")[x])
  #Now, in order to tell apart the timed distribution from early inhibitory failure, we pull a 
  #...medium-resolution density distribution just this time just including the non burts IRTs
  density <- density(MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$class == "non_burst")], adjust = .6)
  #Then, we look for the peak of the timed distribution by restricting the search space
  #...to values greater than 10 s of the density distribution.
  ni_peak <- density$x[which.max(density$y[which(density$x > 10)])] + 10 - density$x[1]
  #If the peak were located to the right of 15 s, then we are just going to restrict the search
  #...for early inhibition failures in values lesser than 15 s.
  if (ni_peak > 15){
    ni_peak <- 15
  }
  #This subroutine searches for the first bulk to the right of the aforedesignated timed peak
  if (density$x[1] > 15){
    #If there were no non burst IRTs lesser than 15, then we set the cutoff between early 
    #...inhibitory failures and timed responses where de density distribution begins
    antimode_2 <- density$x[1]
  }else{
    for (i in length(which(density$x < ni_peak)) : 1){
      if (density$y[i+1] < density$y[i]){
        antimode_2 <- density$x[i]
        break
      }
    }
    #[Please note that this antimode_2 value is referred to as "the cutoff" in the text]
    antimode_2 <- density$x[i]
  }
  
  if (antimode_2 >= 15){
    antimode_2 <- 15
  }
  #Save the cutoff, for documentation purposes
  MM_processed_database[[x]]$antimode_2 <- antimode_2
  #Label IRTs to the left of the cutoff as responses reflecting early inhibitory deficit
  MM_processed_database[[x]]$behavior$class[which(MM_processed_database[[x]]$behavior$class == "non_burst" & MM_processed_database[[x]]$behavior$i_e_i < MM_processed_database[[x]]$antimode_2)] <- "early_inh_def"
  #Now, in order to get the statistics of timed IRTs, we need to pull a low_resolution density
  #...distribution of the non burst IRTs minus the early inhibitory failure IRTs
  dens_nb <- 
    density(MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$class == "non_burst" & MM_processed_database[[x]]$behavior$i_e_i > antimode_2)], adjust = 1)
  #The maximum value will be our measure of central tendency of interest
  nb_peak <- dens_nb$x[dens_nb$y==max(dens_nb$y)]
  MM_processed_database[[x]]$timed_peak <- nb_peak
  #We then determine the bounds of the timed distribution using the full width at half maximum (see text)
  t1 <- dens_nb$x[dens_nb$x < nb_peak][which.min(abs(dens_nb$y[dens_nb$x < nb_peak]-max(dens_nb$y)/2))]
  t2 <- dens_nb$x[dens_nb$x > nb_peak][which.min(abs(dens_nb$y[dens_nb$x > nb_peak]-max(dens_nb$y)/2))]
  MM_processed_database[[x]]$t1 <- t1
  MM_processed_database[[x]]$t2 <- t2
  #Find and save timed IRTs
  timed_iris <-
    MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$i_e_i > t1 & MM_processed_database[[x]]$behavior$i_e_i < t2)]
  #Compute the mean of the timed IRTs
  MM_processed_database[[x]]$timed_mean <- mean(timed_iris)
  #Compute and save the coeficient of variation of the timed IRTs as a measure of their spread
  MM_processed_database[[x]]$timing_precision <- sd(timed_iris)/mean(timed_iris)
  #Count the timed IRTs that happened to be rewarded for further index computing
  n_reward_timed <- length(timed_iris[which(timed_iris >= 15)])
  #Find IRTs presumably reflecting early inhibitory deficit
  #ear_in_def_iris<-
    #MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$class == "early_inh_def" & MM_processed_database[[x]]$behavior$i_e_i < antimode_2)]
  ear_inh_def_r <- 
    MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$i_e_i < MM_processed_database[[x]]$t1 & MM_processed_database[[x]]$behavior$class != "burst")]
  #Count IRTs presumably reflecting early inhibitory deficit
  #n_early_inh_d <- 
    #length(ear_in_def_iris)
  n_early_inh_d <- 
    length(ear_inh_def_r)
  #Count IRTs from early inhibitory category plus timed category
  n_eid_plus_timed <- 
    length(MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$i_e_i < MM_processed_database[[x]]$t2 & MM_processed_database[[x]]$behavior$class != "burst")])
  #Calculate the proportion of responses reflecting early inhibitory deficit
  prop_eid <- n_early_inh_d/n_eid_plus_timed
  #Map the mean of early inhibitory deficit IRIs to the custom inverse sigmoid function
  mean_eid <- inverse_sigmoid(mean(ear_inh_def_r), B=-0.78092115, Q=45.01493555, nu=0.04378572)
  #Compute and save the early inhibitory deficit index
  #early_inh_def <-
    #(mean(timed_iris) - mean(MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$class != "burst" & MM_processed_database[[x]]$behavior$i_e_i < t2)]))/mean(timed_iris)
    #log10((n_early_inh_d+1)/(n_reward_timed+1))
  MM_processed_database[[x]]$early_inh_def <- prop_eid*mean_eid
  #Generate a left-truncated distribution of IRTs for quantifying the influence of attentional lapses 
  left_trunc_t <- MM_processed_database[[x]]$behavior$i_e_i[which(MM_processed_database[[x]]$behavior$i_e_i > t1)]
  #Compute the attentional lapse index
  MM_processed_database[[x]]$att_lapses <-
    mean(left_trunc_t) - mean(timed_iris)
}

#Load library to create plots
library(ggplot2)

#####Create Figure 2

#You can visualize a figure of whichever session × subject data you want by changing the value of "i"
i <- 260

#For visualization purposes, we must first label unlabeled IRTs
sample <- MM_processed_database[[i]]$behavior
for (j in 1:nrow(sample)){
  if (sample$i_e_i[j] > MM_processed_database[[i]]$t2){
    sample$class[j] <- "att_lapse"
  }
  if (sample$i_e_i[j] >= MM_processed_database[[i]]$t1 & sample$i_e_i[j] <= MM_processed_database[[i]]$t2){
    sample$class[j] <- "timed"
  }
}
sample$class <- factor(sample$class, levels = c("burst", "early_inh_def", "timed", "att_lapse", "non_burst"))

#Now, we can create the plot

ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1.5, alpha=.65) +
  geom_vline(aes(xintercept=MM_processed_database[[i]]$antimode), linetype="dashed") +
  geom_text(aes(x = MM_processed_database[[i]]$antimode, y = .02, label = "burst responses / non-burst responses cutoff"), angle = 90, hjust = 0, vjust = -0.5) +
  geom_vline(aes(xintercept=MM_processed_database[[i]]$antimode_2), linetype="dashed") +
  geom_text(aes(x = MM_processed_database[[i]]$antimode_2, y = .023, label = "early inhibitory deficit responses cutoff"), angle = 90, hjust = 0, vjust = -0.5) +
  geom_vline(aes(xintercept=MM_processed_database[[i]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[i]]$t2), color="gray40") +
  geom_segment(aes(x = MM_processed_database[[i]]$t1+0.2, y = .005, xend = MM_processed_database[[i]]$t2-0.2, yend = .005), 
                                 arrow = arrow(type = "open", ends = "both", length = unit(0.15, "inches")),
                                 lineend = 'round', size = 0.4, color = "gray40") +
  geom_text(aes(x = MM_processed_database[[i]]$t1+0.5, y = .006, label = "timed responses bounds"), angle = 0, hjust = 0, vjust = -0.5, color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[i]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  geom_text(aes(x = MM_processed_database[[i]]$timed_peak, y = .031, label = "timed responses peak"), angle = 90, hjust = 0, vjust = -0.5, color = "red") +
  theme_classic(base_size = 12) + 
  theme(legend.position = c(.85, .75),
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 35, 5), limits = c(0,33),
                     expand = expansion(add=c(0,0))) +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                      "early inhibitory deficit", 
                                                                                                      "timed responses",
                                                                                                      "attentional lapses",
                                                                                                      "other non-burst responses"))
#For optimal visualization, save the plot with size 1000x500  

#Further analyses benefit from morphing our huge list to a tighter dataframe

MM_metrics_of_interest <- list()

for (i in 1:length(MM_processed_database)){
  MM_metrics_of_interest[[i]] <- list()
  MM_metrics_of_interest[[i]]$subject <- 
    MM_processed_database[[i]]$subject
  MM_metrics_of_interest[[i]]$group <- 
    MM_processed_database[[i]]$group
  MM_metrics_of_interest[[i]]$phase <- 
    MM_processed_database[[i]]$phase
  MM_metrics_of_interest[[i]]$session <- 
    MM_processed_database[[i]]$session
  MM_metrics_of_interest[[i]]$box <- 
    MM_processed_database[[i]]$box
  MM_metrics_of_interest[[i]]$researcer <- 
    MM_processed_database[[i]]$researcher
  MM_metrics_of_interest[[i]]$start_time <- 
    MM_processed_database[[i]]$start_time
  MM_metrics_of_interest[[i]]$rewards <- 
    MM_processed_database[[i]]$rewards
  MM_metrics_of_interest[[i]]$efficiency <- 
    MM_processed_database[[i]]$efficiency
  MM_metrics_of_interest[[i]]$burst_ratio <- 
    MM_processed_database[[i]]$burst_ratio
  MM_metrics_of_interest[[i]]$timed_peak <- 
    MM_processed_database[[i]]$timed_peak
  MM_metrics_of_interest[[i]]$timing_precision <- 
    MM_processed_database[[i]]$timing_precision
  MM_metrics_of_interest[[i]]$early_inh_def <- 
    MM_processed_database[[i]]$early_inh_def
  MM_metrics_of_interest[[i]]$att_lapses <- 
    MM_processed_database[[i]]$att_lapses
}

MM_met_of_int <- data.frame(matrix(NA, nrow = length(MM_metrics_of_interest), 
                                   ncol = length(MM_metrics_of_interest[[1]])))

for (i in 1:length(MM_metrics_of_interest[[1]])){
  MM_met_of_int[ , i] <- sapply(MM_metrics_of_interest, "[[", i)
}

colnames(MM_met_of_int) <- c("subject", "group", "phase", "session", "box", "researcher", 
                             "start_time", "rewards", "efficiency", "burst_ratio", "timed_peak", 
                             "timing_spread", "early_inh_def", "att_lapses")

#...such a dataframe will be named just "data"
data <- MM_met_of_int

#Now, you can continue with the script number 2

