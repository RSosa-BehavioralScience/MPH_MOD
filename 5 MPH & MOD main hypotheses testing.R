#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
#Load package to perform mixed modeling
if (!require('lmerTest')) install.packages('lmerTest'); library(lmerTest)
#Load package to obtain standardized beta coefficients
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)

#In order to have the Vehicle group as reference, it must be labeled CTL, such
#...that it is the first in alphabetical order
for (i in 1:nrow(data)){
  if (data$group[i] == "SUC"){
    data$group[i] <- "CTL"
  }
}

#Exclude Withdrawal phase data to perform Baseline-Drug phase contrasts
data_wow <- data %>%
  filter(!(phase == "w"))

#The model that matches our study design is that accounting phase × group
#interaction and subjects' ID as a random intercept term.

#We will test each of our dependent variables using that model.

#Baseline-Drug contrast for rewards

bd_r <- lmer(rewards ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(bd_r)

standardize_parameters(bd_r)

#Do the same for all other behavioral indices

#Baseline-Drug contrast for efficiency

bd_e <- lmer(efficiency ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_e)
standardize_parameters(bd_e)

#Baseline-Drug contrast for burst ratio

bd_b <- lmer(burst_ratio ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_b)
standardize_parameters(bd_b)

#Baseline-Drug contrast for early ihibitory deficit

bd_i <- lmer(early_inh_def ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_i)
standardize_parameters(bd_i)
 
#Baseline-Drug contrast for timed peak

bd_p <- lmer(timed_peak ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_p)
standardize_parameters(bd_p)
 
#Baseline-Drug contrast for timing spread

bd_s <- lmer(timing_spread ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_s)
standardize_parameters(bd_s)
 
#Baseline-Drug contrast for attentional lapses

bd_a <- lmer(att_lapses ~ phase * group + (1 | subject),
          data = data_wow,
          REML = F)

summary(bd_a)
standardize_parameters(bd_a)
 
#Create forest plots by manually collecting the standardized betas and their
#...confidence intervals

library(ggplot2)

#Download and save file named "bl vs drug standardized betas.csv" and then
#...load it in R with the following code line
forest_drug <- df <- data.frame(Index = c("Rewards", "Rewards", "Efficiency", "Efficiency", "Burst ratio", "Burst ratio", "Early inh. def.", "Early inh. def.", "Timed peak", "Timed peak", "Timing spread", "Timing spread", "Att. lapses", "Att. lapses"),
                                Std_beta = c(0.06, -0.62, -0.04, -0.62, -0.08, 0.3, 0.16, 0.7, 0.007, -0.14, -0.27, 0.3, -0.1, 0.03),
                                Sb_minus_CI = c(-0.28, -0.95, -0.35, -0.94, -0.33, 0.05, -0.2, 0.36, -0.48, -0.62, -0.69, -0.12, -0.48, -0.36),
                                Sb_plus_CI = c(0.39, -0.28, 0.27, -0.31, 0.16, 0.54, 0.51, 1.03, 0.49, 0.35, 0.15, 0.73, 0.29, 0.41),
                                Group = c("MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD"),
                                Anno = c("", "# &", "", "# &", "", "", "", "# &", "", "", "", "", "", ""),
                                x_Anno = c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1), 
                                y_Anno = c(0, -0.18, 0, -0.21, 0, 0, 0, 0.26, 0, 0, 0, 0, 0, 0))


p <-ggplot(data=forest_drug,
           aes(x = Group,y = Std_beta, ymin=Sb_minus_CI, ymax=Sb_plus_CI))+
  geom_point(aes(col=Group), size = 3)+
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(aes(),yintercept =0, linetype=2)+
  #scale_fill_manual(values=c("white", "white"))+
  xlab('Index')+ ylab("Standardized betas for phase[baseline–drug] × group \n interactions with 95 % confidence intervals")+
  geom_errorbar(aes(ymin=Sb_minus_CI, ymax=Sb_plus_CI, col=Group),width=0.25, cex=1.2)+ 
  #scale_color_manual(values=c("blue", "magenta"))+
  facet_wrap(~factor(Index, levels = c("Rewards",
                                       "Efficiency",
                                       "Burst ratio",
                                       "Early inh. def.",
                                       "Timed peak",
                                       "Timing spread",
                                       "Att. lapses")), strip.position="left",nrow=7, scales="free_y") +
  theme(plot.title=element_text(size=16,face="bold", hjust = .5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"),
        legend.key = element_blank())+
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE)) +
  geom_text(aes(x=x_Anno, y=y_Anno, label=Anno), color = "black", size = 4)
p

p + geom_text(data = annotations, mapping = aes(x=x_Anno, y=y_Anno, label=Anno), color = "blue", size = 6, fontface = "bold")

#For optimal visualization, use 550x850 dimensions

#######drug vs withdrawal phase contrasts

#Withdraw Baseline phase data
data_wob <- data %>%
  filter(!(phase == "b"))

#Do all the same as above

#Drug-Withdrawal contrast for rewards

dw_r <- lmer(rewards ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_r)
standardize_parameters(dw_r)

#Drug-Withdrawal contrast for efficiency
 
dw_e <- lmer(efficiency ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_e)
standardize_parameters(dw_e)
 
#Drug-Withdrawal contrast for burst ratio

dw_b <- lmer(burst_ratio ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_b)
standardize_parameters(dw_b)

#Drug-Withdrawal contrast for early inhibitory deficit

dw_i <- lmer(early_inh_def ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_i)
standardize_parameters(dw_i)
 
#Drug-Withdrawal contrast for timed peak

dw_p <- lmer(timed_peak ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_p)
standardize_parameters(dw_p)

#Drug-Withdrawal contrast for timing spread
 
dw_s <- lmer(timing_spread ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_s)
standardize_parameters(dw_s)
 
#Drug-Withdrawal contrast for attentional lapses

dw_a <- lmer(att_lapses ~ phase * group + (1 | subject),
          data = data_wob,
          REML = F)

summary(dw_a)
standardize_parameters(dw_a)

library(ggplot2)

#Load the file named "drug vs wd standardized betas.csv".
forest_drug <- data.frame(
    Index = c("Rewards", "Rewards", "Efficiency", "Efficiency", "Burst ratio", "Burst ratio", "Early inh. def.", "Early inh. def.", "Timed peak", "Timed peak", "Timing spread", "Timing spread", "Att. lapses", "Att. lapses"),
    Std_beta = c(-0.02, 0.35, -0.07, 0.12, 0.08, 0.01, -0.32, -0.43, 0.12, 0.1, 0.15, -0.15, -0.19, -0.51),
    Sb_minus_CI = c(-0.36, 0.002, -0.39, -0.2, -0.15, -0.21, -0.63, -0.74, -0.37, -0.38, -0.3, -0.61, -0.59, -0.92),
    Sb_plus_CI = c(0.33, 0.69, 0.25, 0.44, 0.31, 0.24, -0.001, -0.11, 0.6, 0.59, 0.61, 0.31, 0.22, -0.11),
    Group = c("MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD")
  )

p <-ggplot(data=forest_drug,
           aes(x = Group,y = Std_beta, ymin=Sb_minus_CI, ymax=1))+
  geom_point(aes(col=Group), size = 3)+
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(aes(),yintercept =0, linetype=2)+
  #scale_fill_manual(values=c("white", "white"))+
  xlab('Index')+ ylab("Standardized betas for phase[drug–washout] × group \n interactions with 95 % confidence intervals")+
  geom_errorbar(aes(ymin=Sb_minus_CI, ymax=Sb_plus_CI, col=Group),width=0.25, cex=1.2)+ 
  #scale_color_manual(values=c("blue", "magenta"))+
  facet_wrap(~factor(Index, levels = c("Rewards",
                                       "Efficiency",
                                       "Burst ratio",
                                       "Early inh. def.",
                                       "Timed peak",
                                       "Timing spread",
                                       "Att. lapses")), strip.position="left",nrow=7, scales="free_y") +
  theme(plot.title=element_text(size=16,face="bold", hjust = .5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"),
        legend.key = element_blank())+
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE))
p

#For optimal visualization, use 550x850 dimensions

######baseline vs withdrawal contrasts

#Withdraw Drug phase data
data_wod <- data %>%
  filter(!(phase == "d"))

#Baseline-Withdrawal contrast for rewards

bw_r <- lmer(rewards ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_r)
standardize_parameters(bw_r)
 
#Baseline-Withdrawal contrast for efficiency

bw_e <- lmer(efficiency ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_e)
standardize_parameters(bw_e)

#Baseline-Withdrawal contrast for burst ratio

bw_b <- lmer(burst_ratio ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_b)
standardize_parameters(bw_b)
 
#Baseline-Withdrawal contrast for early inhibitory deficit

bw_i <- lmer(early_inh_def ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_i)
standardize_parameters(bw_i)
 
#Baseline-Withdrawal contrast for timed peak

bw_p <- lmer(timed_peak ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_p)
standardize_parameters(bw_p)
 
#Baseline-Withdrawal contrast for timing spread

bw_s <- lmer(timing_spread ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_s)
standardize_parameters(bw_s)
 
#Baseline-Withdrawal contrast for attentional lapses

bw_a <- lmer(att_lapses ~ phase * group + (1 | subject),
          data = data_wod,
          REML = F)

summary(bw_a)
standardize_parameters(bw_a)
 

library(ggplot2)

#Load the file named "bl vs wd standardized betas.csv".
forest_drug  <- data.frame(
    Index = c("Rewards", "Rewards", "Efficiency", "Efficiency", "Burst ratio", "Burst ratio", "Early inh. def.", "Early inh. def.", "Timed peak", "Timed peak", "Timing spread", "Timing spread", "Att. lapses", "Att. lapses"),
    Std_beta = c(0.04, -0.33, -0.1, -0.5, -0.0007, 0.3, 0.01, 0.31, 0.11, -0.03, -0.13, 0.18, -0.29, -0.49),
    Sb_minus_CI = c(-0.32, -0.69, -0.43, -0.83, -0.24, 0.06, -0.35, -0.05, -0.35, -0.5, -0.55, -0.25, -0.71, -0.9),
    Sb_plus_CI = c(0.41, 0.04, 0.23, -0.16, 0.24, 0.54, 0.37, 0.67, 0.58, 0.43, 0.3, 0.6, 0.13, -0.07),
    Group = c("MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD", "MPH", "MOD"),
    Anno = c("", "", "", "&", "", "", "", "", "", "", "", "", "", ""),
    x_Anno = c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1), 
    y_Anno = c(0, 0, 0, -0.1, 0, 0, 0, 0.16, 0, 0, 0, 0, 0, 0)
  )


p <-ggplot(data=forest_drug,
           aes(x = Group,y = Std_beta, ymin=-1, ymax=1))+
  geom_point(aes(col=Group), size = 3)+
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(aes(),yintercept =0, linetype=2)+
  #scale_fill_manual(values=c("white", "white"))+
  xlab('Index')+ ylab("Standardized betas for phase[baseline–washout] × group \n interactions with 95 % confidence intervals")+
  geom_errorbar(aes(ymin=Sb_minus_CI, ymax=Sb_plus_CI, col=Group),width=0.25, cex=1.2)+ 
  #scale_color_manual(values=c("blue", "magenta"))+
  facet_wrap(~factor(Index, levels = c("Rewards",
                                       "Efficiency",
                                       "Burst ratio",
                                       "Early inh. def.",
                                       "Timed peak",
                                       "Timing spread",
                                       "Att. lapses")), strip.position="left",nrow=7, scales="free_y") +
  theme(plot.title=element_text(size=16,face="bold", hjust = .5),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0.5,vjust = 1,angle=180,face="bold"),
        legend.key = element_blank())+
  coord_flip() +
  guides(color = guide_legend(reverse = TRUE))+
  geom_text(aes(x=x_Anno, y=y_Anno, label=Anno), color = "black", size = 4)
p

#550x850

#Now, you can proceed with script number 6