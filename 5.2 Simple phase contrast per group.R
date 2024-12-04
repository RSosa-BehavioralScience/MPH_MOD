#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#Load package to perform mixed modeling
if (!require('lmerTest')) install.packages('lmerTest'); library(lmerTest)
#Load package to obtain standardized beta coefficients
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)

for (i in 1:nrow(data)){
  if (data$group[i] == "SUC"){
    data$group[i] <- "CTL"
  }
}

##############################Independent phase contrasts for the Vehicle group

#Exclude Withdrawal phase data to perform Baseline-Drug phase contrasts
data_wow <- data %>%
  filter(!(phase == "w"))

#Baseline-Drug contrast for rewards

bd_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(bd_r)

standardize_parameters(bd_r)

#Do the same for all other behavioral indices

#Baseline-Drug contrast for efficiency

bd_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_e)
standardize_parameters(bd_e)

#Baseline-Drug contrast for burst ratio

bd_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_b)
standardize_parameters(bd_b)

#Baseline-Drug contrast for early ihibitory deficit

bd_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_i)
standardize_parameters(bd_i)

#Baseline-Drug contrast for timed peak

bd_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_p)
standardize_parameters(bd_p)

#Baseline-Drug contrast for timing spread

bd_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_s)
standardize_parameters(bd_s)

#Baseline-Drug contrast for attentional lapses

bd_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="CTL"),],
             REML = F)

summary(bd_a)
standardize_parameters(bd_a)


#######drug vs withdrawal phase contrasts

#Withdraw Baseline phase data
data_wob <- data %>%
  filter(!(phase == "b"))

#Do all the same as above

#Drug-Withdrawal contrast for rewards

dw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_r)
standardize_parameters(dw_r)

#Drug-Withdrawal contrast for efficiency

dw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_e)
standardize_parameters(dw_e)

#Drug-Withdrawal contrast for burst ratio

dw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_b)
standardize_parameters(dw_b)

#Drug-Withdrawal contrast for early inhibitory deficit

dw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_i)
standardize_parameters(dw_i)

#Drug-Withdrawal contrast for timed peak

dw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_p)
standardize_parameters(dw_p)

#Drug-Withdrawal contrast for timing spread

dw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_s)
standardize_parameters(dw_s)

#Drug-Withdrawal contrast for attentional lapses

dw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="CTL"),],
             REML = F)

summary(dw_a)
standardize_parameters(dw_a)



######baseline vs withdrawal contrasts

#Withdraw Drug phase data
data_wod <- data %>%
  filter(!(phase == "d"))

#Baseline-Withdrawal contrast for rewards

bw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_r)
standardize_parameters(bw_r)

#Baseline-Withdrawal contrast for efficiency

bw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_e)
standardize_parameters(bw_e)

#Baseline-Withdrawal contrast for burst ratio

bw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_b)
standardize_parameters(bw_b)

#Baseline-Withdrawal contrast for early inhibitory deficit

bw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_i)
standardize_parameters(bw_i)

#Baseline-Withdrawal contrast for timed peak

bw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_p)
standardize_parameters(bw_p)

#Baseline-Withdrawal contrast for timing spread

bw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_s)
standardize_parameters(bw_s)

#Baseline-Withdrawal contrast for attentional lapses

bw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="CTL"),],
             REML = F)

summary(bw_a)
standardize_parameters(bw_a)


######################Independent phase contrasts for the Methylpjenidate group

#Exclude Withdrawal phase data to perform Baseline-Drug phase contrasts
data_wow <- data %>%
  filter(!(phase == "w"))


bd_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(bd_r)

standardize_parameters(bd_r)

#Do the same for all other behavioral indices

#Baseline-Drug contrast for efficiency

bd_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_e)
standardize_parameters(bd_e)

#Baseline-Drug contrast for burst ratio

bd_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_b)
standardize_parameters(bd_b)

#Baseline-Drug contrast for early ihibitory deficit

bd_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_i)
standardize_parameters(bd_i)

#Baseline-Drug contrast for timed peak

bd_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_p)
standardize_parameters(bd_p)

#Baseline-Drug contrast for timing spread

bd_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_s)
standardize_parameters(bd_s)

#Baseline-Drug contrast for attentional lapses

bd_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MPH"),],
             REML = F)

summary(bd_a)
standardize_parameters(bd_a)


#######drug vs withdrawal phase contrasts

#Withdraw Baseline phase data
data_wob <- data %>%
  filter(!(phase == "b"))

#Do all the same as above

#Drug-Withdrawal contrast for rewards

dw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_r)
standardize_parameters(dw_r)

#Drug-Withdrawal contrast for efficiency

dw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_e)
standardize_parameters(dw_e)

#Drug-Withdrawal contrast for burst ratio

dw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_b)
standardize_parameters(dw_b)

#Drug-Withdrawal contrast for early inhibitory deficit

dw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_i)
standardize_parameters(dw_i)

#Drug-Withdrawal contrast for timed peak

dw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_p)
standardize_parameters(dw_p)

#Drug-Withdrawal contrast for timing spread

dw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_s)
standardize_parameters(dw_s)

#Drug-Withdrawal contrast for attentional lapses

dw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MPH"),],
             REML = F)

summary(dw_a)
standardize_parameters(dw_a)



######baseline vs withdrawal contrasts

#Withdraw Drug phase data
data_wod <- data %>%
  filter(!(phase == "d"))

#Baseline-Withdrawal contrast for rewards

bw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_r)
standardize_parameters(bw_r)

#Baseline-Withdrawal contrast for efficiency

bw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_e)
standardize_parameters(bw_e)

#Baseline-Withdrawal contrast for burst ratio

bw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_b)
standardize_parameters(bw_b)

#Baseline-Withdrawal contrast for early inhibitory deficit

bw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_i)
standardize_parameters(bw_i)

#Baseline-Withdrawal contrast for timed peak

bw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_p)
standardize_parameters(bw_p)

#Baseline-Withdrawal contrast for timing spread

bw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_s)
standardize_parameters(bw_s)

#Baseline-Withdrawal contrast for attentional lapses

bw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MPH"),],
             REML = F)

summary(bw_a)
standardize_parameters(bw_a)


############################Independent phase contrasts for the Modafinil group

#Exclude Withdrawal phase data to perform Baseline-Drug phase contrasts
data_wow <- data %>%
  filter(!(phase == "w"))


#Baseline-Drug contrast for rewards

bd_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(bd_r)

standardize_parameters(bd_r)

#Do the same for all other behavioral indices

#Baseline-Drug contrast for efficiency

bd_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_e)
standardize_parameters(bd_e)

#Baseline-Drug contrast for burst ratio

bd_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_b)
standardize_parameters(bd_b)

#Baseline-Drug contrast for early ihibitory deficit

bd_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_i)
standardize_parameters(bd_i)

#Baseline-Drug contrast for timed peak

bd_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_p)
standardize_parameters(bd_p)

#Baseline-Drug contrast for timing spread

bd_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_s)
standardize_parameters(bd_s)

#Baseline-Drug contrast for attentional lapses

bd_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wow[which(data_wow$group=="MOD"),],
             REML = F)

summary(bd_a)
standardize_parameters(bd_a)


#######drug vs withdrawal phase contrasts

#Withdraw Baseline phase data
data_wob <- data %>%
  filter(!(phase == "b"))

#Do all the same as above

#Drug-Withdrawal contrast for rewards

dw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_r)
standardize_parameters(dw_r)

#Drug-Withdrawal contrast for efficiency

dw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_e)
standardize_parameters(dw_e)

#Drug-Withdrawal contrast for burst ratio

dw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_b)
standardize_parameters(dw_b)

#Drug-Withdrawal contrast for early inhibitory deficit

dw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_i)
standardize_parameters(dw_i)

#Drug-Withdrawal contrast for timed peak

dw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_p)
standardize_parameters(dw_p)

#Drug-Withdrawal contrast for timing spread

dw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_s)
standardize_parameters(dw_s)

#Drug-Withdrawal contrast for attentional lapses

dw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wob[which(data_wob$group=="MOD"),],
             REML = F)

summary(dw_a)
standardize_parameters(dw_a)



######baseline vs withdrawal contrasts

#Withdraw Drug phase data
data_wod <- data %>%
  filter(!(phase == "d"))

#Baseline-Withdrawal contrast for rewards

bw_r <- lmer(rewards ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_r)
standardize_parameters(bw_r)

#Baseline-Withdrawal contrast for efficiency

bw_e <- lmer(efficiency ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_e)
standardize_parameters(bw_e)

#Baseline-Withdrawal contrast for burst ratio

bw_b <- lmer(burst_ratio ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_b)
standardize_parameters(bw_b)

#Baseline-Withdrawal contrast for early inhibitory deficit

bw_i <- lmer(early_inh_def ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_i)
standardize_parameters(bw_i)

#Baseline-Withdrawal contrast for timed peak

bw_p <- lmer(timed_peak ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_p)
standardize_parameters(bw_p)

#Baseline-Withdrawal contrast for timing spread

bw_s <- lmer(timing_spread ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_s)
standardize_parameters(bw_s)

#Baseline-Withdrawal contrast for attentional lapses

bw_a <- lmer(att_lapses ~ phase + (1 | subject),
             data = data_wod[which(data_wod$group=="MOD"),],
             REML = F)

summary(bw_a)
standardize_parameters(bw_a)

#Refer to the "Simple Phase Contrast per Group Report" file to consult the 
#...table which includes the full list of effect sizes and 
#...p-values associated with this analysis.
