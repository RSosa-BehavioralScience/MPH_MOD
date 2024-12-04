#Create an empty data frame to store within-session data
ws_data <- data.frame()

#Create a data frame to temporally store data from single sessions
ws_data_unit <- data.frame()

#Load the data frame with the data and include missing IRT classifications
for (i in 1:length(MM_processed_database)){
  ws_data_unit <- MM_processed_database[[i]]$behavior
  for (j in 1:nrow(ws_data_unit)){
    if (ws_data_unit$i_e_i[j] > MM_processed_database[[i]]$t2){
      ws_data_unit$class[j] <- "att_lapse"
    }
    if (ws_data_unit$i_e_i[j] >= MM_processed_database[[i]]$t1 & ws_data_unit$i_e_i[j] <= MM_processed_database[[i]]$t2){
      ws_data_unit$class[j] <- "timed"
    }
  }
  ws_data_unit$subject <- rep(MM_processed_database[[i]]$subject, nrow(MM_processed_database[[i]]$behavior))
  ws_data_unit$group <- rep(MM_processed_database[[i]]$group, nrow(MM_processed_database[[i]]$behavior))
  ws_data_unit$phase <- rep(MM_processed_database[[i]]$phase, nrow(MM_processed_database[[i]]$behavior))
  ws_data_unit$session <- rep(MM_processed_database[[i]]$session, nrow(MM_processed_database[[i]]$behavior))
  ws_data_unit$ID <- rep(MM_processed_database[[i]]$ID, nrow(MM_processed_database[[i]]$behavior))
  ws_data <- rbind(ws_data, ws_data_unit)
}

#Once we have this general data frame, we can proceed to create more specific data frames to perform
#regressions of refined attributes

#Create an empty data frame to store only inter-reward times
ws_rw <- data.frame()

#This is the total time of the session in 1/20's of second
rw_ts <- 48000

#Determine which responses were rewarded and obtain inter-reward times
for (i in 1:nrow(ws_data)){
  if (ws_data$i_e_i[i]>=15){
    if (rw_ts > ws_data$timestamp[i]){
      i_rw_t <- ws_data$timestamp[i]
    } else {
      i_rw_t <- ws_data$timestamp[i] - rw_ts
    }
    rw_ts <- ws_data$timestamp[i]
    rw_unit <- ws_data[i,]
    rw_unit$i_rw_t <- i_rw_t 
    ws_rw <- rbind(ws_rw, rw_unit)
  }
}

#Express timestamps in terms of seconds
ws_rw$timestamp <- ws_rw$timestamp/20 
#Express inter-reward times in terms of seconds
ws_rw$i_rw_t <- ws_rw$i_rw_t/20
#Generate a variable featuring squared timestamps for quadratic fit
#This new variable needs to be rescaled; otherwise, quadratic regressions will generate warnings
ws_rw$timestamp_2r <- (ws_rw$timestamp ^2)*(max(ws_rw$timestamp)/max(ws_rw$timestamp ^2))

#Load package for data frame manipulation
library(dplyr)

#Rename Vehicle (SUCrose) group as CTL
for (i in 1:nrow(ws_rw)){
  if (ws_rw$group[i] == "SUC"){
    ws_rw$group[i] <- "CTL"
  }
}

#For baseline–drug contrasts, we need to drop data from the withdrawal phase
ws_rw_bd <- ws_rw %>%
  filter(!(phase == "w"))

#Load package for mixed model analyses
library(lmerTest)

#baseline–drug contrast for obtained rewards

#This is the linear model
e <- lmer(i_rw_t ~ phase * group * timestamp + (1 | subject),
          data = ws_rw_bd)

#This is the quadratic model
f <- lmer(i_rw_t ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
           data = ws_rw_bd)

#Here, we pit the two models against each other to see which performs better
anova(e, f)
#Obtain the parameters of the best-fitting model
summary(f)

#Load package to obtain standardized beta coefficients 
library(effectsize)

#Get standardized beta coefficients
effectsize(f)

#Rename Vehicle group in the general data frame
for (i in 1:nrow(ws_data)){
  if (ws_data$group[i] == "SUC"){
    ws_data$group[i] <- "CTL"
  }
}

#Create empty vectors for further maneuvers
ft_unit <-  c()
ft_column <- c()

#Set all timed IRTs to the value of the session-subject average
for (i in 1:length(unique(ws_data$ID))){
  subject_session <- ws_data[which(ws_data$ID == unique(ws_data$ID)[i]),]
  for (j in 1:nrow(ws_data[which(ws_data$ID == unique(ws_data$ID)[i]),])){
    if (subject_session$class[j] == "timed"){
      ft_unit <- c(ft_unit, mean(subject_session$i_e_i[which(subject_session$class == "timed")])) 
    } else {ft_unit <- c(ft_unit, subject_session$i_e_i[j])}
  }
  ft_column <- c(ft_column, ft_unit)
  ft_unit <-  c()
}

#Append the vector to the general data frame
ws_data$iei_tf <- ft_column

#Tailor data
ws_data$timestamp <- ws_data$timestamp/20 
ws_data$timestamp_2r <- (ws_data$timestamp ^2)*(max(ws_data$timestamp)/max(ws_data$timestamp ^2))

#For this regression, we need to filter out data that are not of interest
ws_b_bd <- ws_data %>%
  filter(!(phase == "w"),
         class == "burst" | class == "timed")

#baseline–drug contrast for burst responding

#Linear model
e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_b_bd)

#Quadratic model
f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_b_bd)

#Model comparison
anova(e, f)
#Best fitting model
summary(f)
#Standardized beta coefficients
effectsize(f)

#Now, we need to isolate data that are relevant for testing early inhibitory deficit
ws_i_bd <- ws_data %>%
  filter(!(phase == "w"),
         class == "early_inh_def" | class == "timed" | class == "non_burst")

#baseline–drug contrast for early inhibitory control deficit

e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_i_bd)

f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_i_bd)

anova(e, f)
summary(f)
effectsize(f)

#For examining the timing peak, me need to isolate raw timed IRTs
ws_t_bd <- ws_data %>%
  filter(!(phase == "w"),
         class == "timed")

#baseline–drug contrast for timed peak

e <- lmer(i_e_i ~ phase * group * timestamp + (1 | subject),
          data = ws_t_bd)

f <- lmer(i_e_i ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_t_bd)

anova(e, f)
summary(f)
effectsize(f)

#spread_unit <-  c()

#Generate an empty vector to store local coefficients of variation
spread_column <- c()

#Obtain local coefficients of variation from timed IRTs
for (i in 1:length(unique(ws_t_bd$ID))){
  session_subject <- ws_t_bd[which(ws_t_bd$ID == unique(ws_t_bd$ID)[i]),]
  spread_unit <- vector(length = nrow(session_subject))
  spread_unit[1] <- NA
  for (j in 2:nrow(session_subject)){
    spread_unit[j] <- sqrt((session_subject$i_e_i[j] - session_subject$i_e_i[j-1])^2)/mean(session_subject$i_e_i[j], session_subject$i_e_i[j-1])
  }
  spread_column <- c(spread_column, spread_unit)
  spread_unit <- c()
}

#Append new column
ws_t_bd$t_spread <- spread_column 

#baseline–drug contrast for timing spread

e <- lmer(t_spread ~ phase * group * timestamp + (1 | subject),
          data = ws_t_bd)

f <- lmer(t_spread ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_t_bd)

anova(e, f)
summary(e)
effectsize(e)

#Now, isolate data of interest for attentional lapses
ws_a_bd <- ws_data %>%
  filter(!(phase == "w"),
         class == "att_lapse" | class == "timed")

#baseline–drug contrast for attentional lapses

e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_a_bd)

f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_a_bd)

anova(e, f)
summary(e)
effectsize(e)

#now, we conduct drug–withdrawal contrasts

for (i in 1:nrow(ws_rw)){
  if (ws_rw$group[i] == "SUC"){
    ws_rw$group[i] <- "CTL"
  }
}

#drop baseline data for drug–withdrawal contrasts
ws_rw_dw <- ws_rw %>%
  filter(!(phase == "b"))

#drug–withdrawal contrasts for obtained rewards

e <- lmer(i_rw_t ~ phase * group * timestamp + (1 | subject),
          data = ws_rw_dw)

f <- lmer(i_rw_t ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_rw_dw)

anova(e, f)
summary(f)
effectsize(f)

for (i in 1:nrow(ws_data)){
  if (ws_data$group[i] == "SUC"){
    ws_data$group[i] <- "CTL"
  }
}

ft_unit <-  c()
ft_column <- c()

for (i in 1:length(unique(ws_data$ID))){
  subject_session <- ws_data[which(ws_data$ID == unique(ws_data$ID)[i]),]
  for (j in 1:nrow(ws_data[which(ws_data$ID == unique(ws_data$ID)[i]),])){
    if (subject_session$class[j] == "timed"){
      ft_unit <- c(ft_unit, mean(subject_session$i_e_i[which(subject_session$class == "timed")])) 
    } else {ft_unit <- c(ft_unit, subject_session$i_e_i[j])}
  }
  ft_column <- c(ft_column, ft_unit)
  ft_unit <-  c()
}

ws_data$iei_tf <- ft_column

ws_data$timestamp <- ws_data$timestamp/20 
ws_data$timestamp_2r <- (ws_data$timestamp ^2)*(max(ws_data$timestamp)/max(ws_data$timestamp ^2))

ws_b_dw <- ws_data %>%
  filter(!(phase == "b"),
         class == "burst" | class == "timed")

#drug–withdrawal contrasts for burst responding

e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_b_dw)

f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_b_dw)

anova(e, f)
summary(f)
effectsize(f)

ws_i_dw <- ws_data %>%
  filter(!(phase == "b"),
         class == "early_inh_def" | class == "timed" | class == "non_burst")

#drug–withdrawal contrasts for early inhibitory control deficit

e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_i_dw)

f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_i_dw)

anova(e, f)
summary(f)
effectsize(f)

ws_t_dw <- ws_data %>%
  filter(!(phase == "b"),
         class == "timed")

#drug–withdrawal contrasts for timing peak

e <- lmer(i_e_i ~ phase * group * timestamp + (1 | subject),
          data = ws_t_dw)

f <- lmer(i_e_i ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_t_dw)

anova(e, f)
summary(f)
effectsize(f)

spread_column <- c()

for (i in 1:length(unique(ws_t_dw$ID))){
  session_subject <- ws_t_dw[which(ws_t_dw$ID == unique(ws_t_dw$ID)[i]),]
  spread_unit <- vector(length = nrow(session_subject))
  spread_unit[1] <- NA
  for (j in 2:nrow(session_subject)){
    spread_unit[j] <- sqrt((session_subject$i_e_i[j] - session_subject$i_e_i[j-1])^2)/mean(session_subject$i_e_i[j], session_subject$i_e_i[j-1])
  }
  spread_column <- c(spread_column, spread_unit)
  spread_unit <- c()
}

ws_t_dw$t_spread <- spread_column 

#drug–withdrawal contrasts for timing spread

e <- lmer(t_spread ~ phase * group * timestamp + (1 | subject),
          data = ws_t_dw)

f <- lmer(t_spread ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_t_dw)

anova(e, f)
summary(e)
effectsize(e)

ws_a_dw <- ws_data %>%
  filter(!(phase == "b"),
         class == "att_lapse" | class == "timed")

#drug–withdrawal contrasts for attentional lapses

e <- lmer(iei_tf ~ phase * group * timestamp + (1 | subject),
          data = ws_a_dw)

f <- lmer(iei_tf ~ (phase*group*timestamp) + (phase*group*timestamp_2r) + (1 | subject),
          data = ws_a_dw)

anova(e, f)
summary(f)
effectsize(f)

#Now, you may proceed with script number 9 or save the current evironment to continue later.
