#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('lmerTest')) install.packages('lmerTest'); library(lmerTest)
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)

#Testing group differences in baseline performance alone

#Create a data frame including only baseline data
data_bl <- data %>%
  filter(!(phase == "d")) %>% 
  filter(!(phase == "w")) 

#Baseline difference between groups in obtained rewards
b_r <- lmer(rewards ~ group + (1 | subject),
             data = data_bl,
             REML = F)

summary(b_r)

#Baseline difference between groups in efficiency
b_e <- lmer(efficiency ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_e)

#Baseline difference between groups in burst ratio
b_b <- lmer(burst_ratio ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_b)

#Baseline difference between groups in early inhibitory deficit
b_i <- lmer(early_inh_def ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_i)

#Baseline difference between groups in timed peak
b_p <- lmer(timed_peak ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_p)

#Baseline difference between groups in timing spread
b_s <- lmer(timing_spread ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_s)

#Baseline difference between groups in attentional lapses
b_a <- lmer(att_lapses ~ group + (1 | subject),
            data = data_bl,
            REML = F)

summary(b_a)

#None of the seven dependent variables revealed significant main effects of 
#...the group during the Baseline phase, confirming that 
#...baseline conditions were sufficiently equivalent for conducting our 
#...planned analyses.