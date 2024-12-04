#Set default language to English to get common error and warning messages
Sys.setenv(LANG = "en")

windows_analysis <- MM_windows_ready 

for (i in 1:nrow(windows_analysis)){
  if (windows_analysis$group[i] == "SUC"){
    windows_analysis$group[i] <- "CTL"
  }
}

w_a_wow <- windows_analysis %>%
  filter(!(phase == "w"))

#For creating Figure 8, we need to code windows as a categorical variable
w_a_wow$window <- as.character(w_a_wow$window)

#Then, run the mixed model to extract the parameters without linear constraints
#This takes a lot more time, because of multiple window-by-window comparisons
e <- lmer(rewards ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)

#Generate SEMs
stan <- standardize_parameters(e, ci = .68)
#Isolate SEMs for three-way interactions
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

#Tabulate data of interest for plotting
for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan1 <- stan

#Create object storing the figure containing data from three-way interaction
#...for reward in baseline-drug contrast
r_bd <-
ggplot(stan1, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  #theme(legend.position = c(.75, .9))+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

#Do all the same for the other indices and contrasts

e <- lmer(efficiency ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan2 <- stan

e_bd <-
  ggplot(stan2, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(burst_ratio ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan3 <- stan

stan3 <- stan

b_bd <-
  ggplot(stan3, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(early_inh_def ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan4 <- stan

i_bd <-
  ggplot(stan4, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(timed_peak ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan5 <- stan

p_bd <-
  ggplot(stan5, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(timed_spread ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan6 <- stan

s_bd <-
  ggplot(stan6, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(att_lapses ~ phase * group * window + (1 | subject),
          data = w_a_wow,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan7 <- stan

a_bd <-
  ggplot(stan7, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

w_a_wobl <- windows_analysis %>%
  filter(!(phase == "b"))

w_a_wobl$window <- as.character(w_a_wow$window)

e <- lmer(rewards ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan8 <- stan

r_dw <-
  ggplot(stan8, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(efficiency ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan9 <- stan

e_dw <-
  ggplot(stan9, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(burst_ratio ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan10 <- stan

b_dw <-
  ggplot(stan10, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(early_inh_def ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan11 <- stan

i_dw <-
  ggplot(stan11, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(timed_peak ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan12 <- stan

p_dw <-
  ggplot(stan12, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(timed_spread ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan13 <- stan

s_dw <-
  ggplot(stan13, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))

e <- lmer(att_lapses ~ phase * group * window + (1 | subject),
          data = w_a_wobl,
          REML = F)
stan <- standardize_parameters(e, ci = .68)
stan <- stan[391:582, ]
window <- numeric(length = nrow(stan))
group <- character(length = nrow(stan))
stan <- as.data.frame(cbind(stan, group, window))

for (i in 1:nrow(stan)){
   
  window <- substring(stan$Parameter[i], 23)
  stan$window[i] <- as.numeric(window)
  group <- substring(stan$Parameter[i], 13, 15)
  stan$group[i] <- group
}

stan14 <- stan

a_dw <-
  ggplot(stan14, aes(x=window, y=Std_Coefficient, colour=group)) +
  geom_ribbon(aes(ymin=CI_low, ymax=CI_high, fill=group),
              alpha=0.3, colour=NA) +
  scale_fill_manual(values=c("blue", "magenta"))+
  geom_line(linewidth = 1.5) +
  scale_color_manual(values=c("blue", "magenta"))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme_bw()+
  scale_x_continuous(breaks = seq(5, 95, 10), limits = c(2,97))+
  scale_y_continuous(breaks = seq(-1, 1, .5), limits = c(-1.15,1.3))+
  theme(legend.position = "none")+
  labs(x="Window", y="Standardized beta coefficient")+
  guides(color=guide_legend(title ="Group", reverse = TRUE), fill=guide_legend(title ="Group", reverse = TRUE))


#Create a list containing all the plots we created above

MM_windows <- list(r_bd, r_dw,
                    e_bd, e_dw,
                    b_bd, b_dw,
                    i_bd, i_dw,
                    p_bd, p_dw,
                    s_bd, s_dw,
                    a_bd, a_dw)



#To create the plot compiling all the results of this analysis (i.e., Figure S3), we initially 
#...used the package GGally. However, this package no longer appears to 
#...function as expected. As a result, we have silenced that part of the code and replaced the 
#...creation of Figure S3 (or a close approximation of it) using a combination of separate 
#...packages in the following lines.

# #Load plot-grouping package
# library(GGally)
# 
# #Set up plot features and visualize plot
# MM_win_mat <- ggmatrix(MM_windows, 7, 2,
#                    xAxisLabels = c("Baseline–Drug", "Drug–Washout"),
#                    yAxisLabels = c("Rewards", "Efficiency", "Burst ratio", "Early inh. def.", "Timed peak", "Timing spread", "Att. lapses"),
#                    xlab = "Window", ylab = "Standardized beta coefficients -/+ SEM", legend = c(1,1))
# MM_win_mat + theme(strip.text = element_text(size = 15), 
#                    axis.title = element_text(size = 15), 
#                    legend.text = element_text(size=12),
#                    legend.title = element_text(size=15)) 

#Load required libraries
if (!require('patchwork')) install.packages('patchwork'); library(patchwork)
if (!require('cowplot')) install.packages('cowplot'); library(cowplot)

#Column labels
x_axis_labels <- c("Baseline–Drug", "Drug–Washout")
y_axis_labels <- c("Rewards", "Efficiency", "Burst ratio", "Early inh. def.", 
                   "Timed peak", "Timing spread", "Att. lapses")

#Y-axis label
y_axis_label <- ggdraw() +
  draw_text("Standardized beta coefficients -/+ SEM",
            size = 15, angle = 90, hjust = 0.5, vjust = 0.5)

#Row labels
row_labels <- cowplot::plot_grid(
  plotlist = lapply(y_axis_labels, function(label) ggdraw() + draw_label(label, size = 12, angle = 90)),
  ncol = 1,
  rel_heights = rep(1, length(y_axis_labels))  # Equal height for each label
)

#Combine the row labels with the plot grid
matrix_with_row_labels <- cowplot::plot_grid(
  row_labels,              
  MM_win_mat,              
  ncol = 2,
  rel_widths = c(0.1, 1)   # Adjust proportions
)

#Combine column labels with the grid and legend
matrix_with_labels_and_legend <- cowplot::plot_grid(
  cowplot::plot_grid(
    column_labels,         
    matrix_with_row_labels, 
    ncol = 1,
    rel_heights = c(0.05, 1) 
  ),
  legend,                  
  ncol = 2,
  rel_widths = c(0.85, 0.15) 
)

#Add the y-axis label to the left of the grid
plot_with_y_axis <- cowplot::plot_grid(
  y_axis_label,           
  matrix_with_labels_and_legend, 
  ncol = 2,
  rel_widths = c(0.1, 1)   
)

#Add the x-axis label at the bottom
final_plot <- cowplot::plot_grid(
  plot_with_y_axis,       
  ggdraw() + draw_label("Window", size = 15, hjust = 0.25),  
  ncol = 1,
  rel_heights = c(1, 0.05) # Adjust proportions
)

#Display the final plot
print(final_plot)
#Recommended visualization dimensions: 650 (width) x 1000 (height)


#Continue with script number 8