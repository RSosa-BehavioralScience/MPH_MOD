if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('effectsize')) install.packages('effectsize'); library(effectsize)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

#First, we need to create a new column in the data frame named 'data' containing per session data

data_w_avg_bl <- data %>%
  group_by(subject) %>%
  filter(phase != "w") %>% 
  mutate(avg_bl_rewards = mean(rewards[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_efficiency = mean(efficiency[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_burst = mean(burst_ratio[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_peak = mean(timed_peak[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_spread = mean(timing_spread[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_eid = mean(early_inh_def[phase == "b"], na.rm = TRUE)) %>%
  mutate(avg_bl_lapses = mean(att_lapses[phase == "b"], na.rm = TRUE)) %>%
  ungroup()

#Then, we tested whether the effect of drogs was modulated by performance in the baseline phase

#For obtained rewards

perf_modulation_r <- lmer(rewards ~ phase * group * avg_bl_rewards + (1 | subject),
             data = data_w_avg_bl,
             REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_r)

standardize_parameters(perf_modulation_r)

#For efficiency

perf_modulation_e <- lmer(efficiency ~ phase * group * avg_bl_efficiency + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_e)

standardize_parameters(perf_modulation_e)

#For burst responding

perf_modulation_b <- lmer(burst_ratio ~ phase * group * avg_bl_burst + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_b)

standardize_parameters(perf_modulation_b)

#For timed peak

perf_modulation_p <- lmer(timed_peak ~ phase * group * avg_bl_peak + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_p)

standardize_parameters(perf_modulation_p)

#For timing spread

perf_modulation_s <- lmer(timing_spread ~ phase * group * avg_bl_spread + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_s)

standardize_parameters(perf_modulation_s)

#For attentional lapses

perf_modulation_l <- lmer(att_lapses ~ phase * group * avg_bl_lapses + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_l)

standardize_parameters(perf_modulation_l)

#For early inhibitory deficit

perf_modulation_i <- lmer(early_inh_def ~ phase * group * avg_bl_eid + (1 | subject),
                        data = data_w_avg_bl,
                        REML = F)

#Parameters of the model that accounts for phase × group interactions
summary(perf_modulation_i)

standardize_parameters(perf_modulation_i)

###############Visualization

#Group data by phase averages per subject and generate a delta value indicating the change from
#baseline phase to drug phase in early inhibitory deficit
bl_mod_i <- data_w_avg_bl %>%
  filter(group %in% c("MOD", "CTL")) %>%
  group_by(subject) %>%
  summarise(
    group = first(group),
    baseline_i = mean(early_inh_def[phase == "b"], na.rm = TRUE),
    drug_i = mean(early_inh_def[phase == "d"], na.rm = TRUE)
  ) %>%
  mutate(delta_i = drug_i - baseline_i)

#Create the plot illustrating the interaction
ggplot(bl_mod_i, aes(x = baseline_i, y = delta_i, color = group)) +
  geom_point(size = 7, shape = 18) +  
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Baseline Early Inhibitory Deficit Performance",
       y = "Change in Early Inhibitory Deficit from Baseline to Drug Phase",
       color = "Group") +  
  theme_minimal(base_size = 15) + 
  theme(
    plot.title = element_blank(), 
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1.5),  
    legend.position = c(0.8, 0.8),  
    panel.grid = element_blank()  
  ) +
  scale_color_manual(values = c("CTL" = "green", "MOD" = "blue"),  
                     labels = c("CTL" = "Vehicle", "MOD" = "Modafinil")) +  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5) +  
  annotate("text", x = Inf, y = 0, label = "↑ Increased Inhibitory Deficit in the Drug Phase ↑", 
           vjust = -1, hjust = 1.1, size = 4, color = "black") +  
  annotate("text", x = Inf, y = 0, label = "↓ Decreased Inhibitory Deficit in the Drug Phase ↓", 
           vjust = 1.5, hjust = 1.1, size = 4, color = "black")   

#We thank Reviewer #1 for suggesting this analysis
