#Install or load required packages
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)

#Render a dataframe to Baseline-only data to perform descriptive statistics
subject_means <- data %>% 
  filter(phase=="b") %>% 
  group_by(subject, group, phase) %>% 
  summarise(mean_rw=(mean(rewards)),
            mean_eff=(mean(efficiency)),
            mean_br=(mean(burst_ratio)),
            mean_tp=(mean(timed_peak)), 
            mean_ts=(mean(timing_spread)),
            mean_eid=(mean(early_inh_def)), 
            mean_al=(mean(att_lapses)))

#Compute grand means with CIs for every dependent variable for reporting the
#main descriptive stats

mean(subject_means$mean_rw)
confint(lm(mean_rw ~ 1, subject_means))

mean(subject_means$mean_eff)
confint(lm(mean_eff ~ 1, subject_means))

mean(subject_means$mean_br)
confint(lm(mean_br ~ 1, subject_means))

mean(subject_means$mean_eid)
confint(lm(mean_eid ~ 1, subject_means))

mean(subject_means$mean_tp)
confint(lm(mean_tp ~ 1, subject_means))

mean(subject_means$mean_ts)
confint(lm(mean_ts ~ 1, subject_means))

mean(subject_means$mean_al)
confint(lm(mean_al ~ 1, subject_means))

#Screen for cross-sectional correlations

#This creates a vector that stores each index
index_1 <- c()
#Each index will be paired with every other index for determining correlations
index_2 <- c()
#We will store correlation coefficients in this vector
estimate <- c()
#These are vectors to store the confidence intervals and p-values of the 
#obtained coefficients
lower_confint <- c()
upper_confint <- c()
p_value <- c()

#The following for-loop routine will do the rest of the job
k <- 1
for (i in 4:9){
  for (j in (i+1):10){
    index_1[k] <- colnames(subject_means[,i])
    index_2[k] <- colnames(subject_means[,j])
    estimate[k] <- cor.test(pull(subject_means[,i]), pull(subject_means[,j]))$estimate
    lower_confint[k] <- cor.test(pull(subject_means[,i]), pull(subject_means[,j]))$conf.int[1]
    upper_confint[k] <- cor.test(pull(subject_means[,i]), pull(subject_means[,j]))$conf.int[2]
    p_value[k] <- cor.test(pull(subject_means[,i]), pull(subject_means[,j]))$p.value
    k <- k+1
  }
}
cs_correlations <- as.data.frame(cbind(index_1, index_2, estimate, lower_confint, upper_confint))
cs_correlations$p_value <- p_value

#We can screen for longitudinal correlations, acordingly, with a few added steps

data_baseline <- data %>% 
  filter(phase=="b")

subjects <- unique(data_baseline$subject)
indices <- colnames(data_baseline)[8:ncol(data_baseline)]
l_correlations <- data.frame(matrix(nrow=nrow(cs_correlations), ncol=length(subjects)+2))
colnames(l_correlations)[1:2] <- colnames(cs_correlations)[1:2]
colnames(l_correlations)[3:ncol(l_correlations)] <- subjects

k <- 1
for (i in 1:length(indices)){
  if (i == length(indices)){
    break
  }
  for (j in (i+1):length(indices)){
    l_correlations[k,1] <- indices[i]
    l_correlations[k,2] <- indices[j]
    k <- k + 1
  }
}

for (i in 1:nrow(l_correlations)){
 for (j in 1:length(subjects)){
   bl_per_subj <- data_baseline %>% filter(subject==subjects[j]) 
   k <- which(colnames(bl_per_subj) == l_correlations[i,1])
   l <- which(colnames(bl_per_subj) == l_correlations[i,2])
   l_correlations[i,j+2] <- cor.test(bl_per_subj[,k], bl_per_subj[,l])$estimate
 }
}

estimate <- c()
lower_confint <- c()
upper_confint <- c()
p_value <- c()

for (x in 1:nrow(l_correlations)){
  estimate[x] <- t.test(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]))$estimate
  lower_confint[x] <- t.test(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]))$conf.int[1]
  upper_confint[x] <- t.test(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]))$conf.int[2]
  p_value[x] <- t.test(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]))$p.value
}

l_correlations <- cbind(l_correlations, estimate, lower_confint, upper_confint, p_value)

# x <- 21
# l_correlations[x, 1:2]
# hist(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]), breaks = 10)
# t <-t.test(as.numeric(l_correlations[x, ][,3:ncol(l_correlations)]))

#Load this package to create a color gradient
library(colorspace)

colors <- sequential_hcl(200, "Dark Mint")

#Load this package to obtain intraclass correlations
library(rptR)
#Set seed for reproducible results of permutation tests
set.seed(1401)

#The following code chunks will obtain intraclass correlations for each index
#Run the seven of them at once.
#Grab a cup of coffee, because this will take a while!

rpt_rw <- rpt(rewards ~ session + (1 | subject), grname = "subject", data = data_baseline, 
              datatype = "Poisson", nboot = 1000, npermut = 1000)

rpt_eff <- rpt(efficiency ~ session + (1 | subject), grname = "subject", data = data_baseline, 
               datatype = "Gaussian", nboot = 1000, npermut = 1000)

rpt_br <- rpt(burst_ratio ~ session + (1 | subject), grname = "subject", data = data_baseline, 
              datatype = "Gaussian", nboot = 1000, npermut = 1000)

rpt_eid <- rpt(early_inh_def ~ session + (1 | subject), grname = "subject", data = data_baseline, 
               datatype = "Gaussian", nboot = 1000, npermut = 1000)

rpt_tp <- rpt(timed_peak ~ session + (1 | subject), grname = "subject", data = data_baseline, 
              datatype = "Gaussian", nboot = 1000, npermut = 1000)

rpt_ts <- rpt(timing_spread ~ session + (1 | subject), grname = "subject", data = data_baseline, 
              datatype = "Gaussian", nboot = 1000, npermut = 1000)

rpt_al <- rpt(att_lapses ~ session + (1 | subject), grname = "subject", data = data_baseline, 
              datatype = "Gaussian", nboot = 1000, npermut = 1000)

#Store the value of all correlation magnitudes to match them with the color gradient code
correlations <- c(as.numeric(cs_correlations$estimate), as.numeric(l_correlations$estimate), 
                  mean(rpt_rw$R_boot_link$subject), rpt_eff$R$subject, rpt_br$R$subject, 
                  rpt_eid$R$subject, rpt_tp$R$subject, rpt_ts$R$subject, rpt_al$R$subject)
hist(abs(correlations))
min_abs_rho <- abs(correlations[which.min(abs(correlations))])
max_abs_rho <- abs(correlations[which.max(abs(correlations))])

color <- rep(NA, nrow(cs_correlations))
cs_correlations$color <- color

for (i in 1:nrow(cs_correlations)){
  color_mapping <- 
    ((abs(as.numeric(cs_correlations$estimate[i])) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
  cs_correlations$color[i] <- colors[200 - (round(color_mapping, digits =  0))] 
    
}

#Generate significance-convention labels for cross-sectional correlations
for (i in 1:nrow(cs_correlations)){
  if (cs_correlations$p_value[i] >= 0.05){
    cs_correlations$signif_label[i] <- "ns"
  }
  if (cs_correlations$p_value[i] < 0.05){
    cs_correlations$signif_label[i] <- "*"
  }
  if (cs_correlations$p_value[i] < 0.01){
    cs_correlations$signif_label[i] <- "**"
  }
  if (cs_correlations$p_value[i] < 0.001){
    cs_correlations$signif_label[i] <- "***"
  }
}

#The following code chunks will generate plots for cross-sectional correlations
#regarding each pair of behavioral indices and store them for creating Figure 3

x_axis <- "mean_rw"
y_axis <- "mean_eff"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_eff <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_eff)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 53, y = .53, color = "antiquewhite4", size = 10, fontface = "bold")
cs_rw_vs_eff

x_axis <- "mean_rw"
y_axis <- "mean_br"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_br <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_br)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 52, y = .08, color = "antiquewhite4", size = 6, fontface = "bold")
cs_rw_vs_br

x_axis <- "mean_rw"
y_axis <- "mean_tp"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_tp <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_tp)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 53, y = 19.2, color = "antiquewhite4", size = 10, fontface = "bold")
cs_rw_vs_tp

x_axis <- "mean_rw"
y_axis <- "mean_ts"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_ts <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_ts)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")+
  annotate("text", label = target_signif_lab, x = 52, y = .215, color = "antiquewhite4", size = 6, fontface = "bold")
cs_rw_vs_ts

x_axis <- "mean_rw"
y_axis <- "mean_eid"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_eid <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_eid)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3")+
  annotate("text", label = target_signif_lab, x = 52, y = 0.42, color = "antiquewhite4", size = 10, fontface = "bold")
cs_rw_vs_eid

x_axis <- "mean_rw"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_rw_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_rw,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 52, y = 2.9, color = "antiquewhite4", size = 6, fontface = "bold") #+
  #scale_x_continuous(breaks = c(70, 80, 90, 100))
cs_rw_vs_al

x_axis <- "mean_eff"
y_axis <- "mean_br"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eff_vs_br <- ggplot(data=subject_means, mapping = aes(x=mean_eff,y=mean_br)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -.56, y = -1.8, color = "antiquewhite4", size = 10, fontface = "bold")
cs_eff_vs_br

x_axis <- "mean_eff"
y_axis <- "mean_tp"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eff_vs_tp <- ggplot(data=subject_means, mapping = aes(x=mean_eff,y=mean_tp)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -.56, y = 19.2, color = "antiquewhite4", size = 10, fontface = "bold")
cs_eff_vs_tp

x_axis <- "mean_eff"
y_axis <- "mean_ts"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eff_vs_ts <- ggplot(data=subject_means, mapping = aes(x=mean_eff,y=mean_ts)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -.58, y = .215, color = "antiquewhite4", size = 6, fontface = "bold")
cs_eff_vs_ts

x_axis <- "mean_eff"
y_axis <- "mean_eid"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eff_vs_eid <- ggplot(data=subject_means, mapping = aes(x=mean_eff,y=mean_eid)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -.58, y = 0.42, color = "antiquewhite4", size = 10, fontface = "bold")
cs_eff_vs_eid

x_axis <- "mean_eff"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eff_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_eff,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -.58, y = 2.9, color = "antiquewhite4", size = 6, fontface = "bold")
cs_eff_vs_al

x_axis <- "mean_br"
y_axis <- "mean_tp"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_br_vs_tp <- ggplot(data=subject_means, mapping = aes(x=mean_br,y=mean_tp)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -1.75, y = 19.5, color = "antiquewhite4", size = 6, fontface = "bold")
cs_br_vs_tp

x_axis <- "mean_br"
y_axis <- "mean_ts"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_br_vs_ts <- ggplot(data=subject_means, mapping = aes(x=mean_br,y=mean_ts)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -1.75, y = .215, color = "antiquewhite4", size = 6, fontface = "bold")
cs_br_vs_ts

x_axis <- "mean_br"
y_axis <- "mean_eid"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_br_vs_eid <- ggplot(data=subject_means, mapping = aes(x=mean_br,y=mean_eid)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -1.75, y = 0.43, color = "antiquewhite4", size = 6, fontface = "bold")
cs_br_vs_eid

x_axis <- "mean_br"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_br_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_br,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = -1.75, y = 2.9, color = "antiquewhite4", size = 6, fontface = "bold")
cs_br_vs_al

x_axis <- "mean_tp"
y_axis <- "mean_ts"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_tp_vs_ts <- ggplot(data=subject_means, mapping = aes(x=mean_tp,y=mean_ts)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 13.5, y = .21, color = "antiquewhite4", size = 10, fontface = "bold")
cs_tp_vs_ts

x_axis <- "mean_tp"
y_axis <- "mean_eid"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_tp_vs_eid <- ggplot(data=subject_means, mapping = aes(x=mean_tp,y=mean_eid)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 13.6, y = 0.43, color = "antiquewhite4", size = 6, fontface = "bold")
cs_tp_vs_eid

x_axis <- "mean_tp"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_tp_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_tp,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 13.5, y = 2.8, color = "antiquewhite4", size = 10, fontface = "bold")
cs_tp_vs_al

x_axis <- "mean_ts"
y_axis <- "mean_eid"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_ts_vs_eid <- ggplot(data=subject_means, mapping = aes(x=mean_ts,y=mean_eid)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 0.08, y = 0.43, color = "antiquewhite4", size = 6, fontface = "bold")
cs_ts_vs_eid

x_axis <- "mean_ts"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_ts_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_ts,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 0.085, y = 2.83, color = "antiquewhite4", size = 10, fontface = "bold")
cs_ts_vs_al

x_axis <- "mean_eid"
y_axis <- "mean_al"
target_color <- cs_correlations$color[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
target_signif_lab <- cs_correlations$signif_label[which(cs_correlations$index_1==x_axis & cs_correlations$index_2==y_axis)]
cs_eid_vs_al <- ggplot(data=subject_means, mapping = aes(x=mean_eid,y=mean_al)) + 
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_point(shape=1, stroke=4, size=5, color=target_color) +
  geom_smooth(method="lm", color="antiquewhite4", fill="antiquewhite3") +
  annotate("text", label = target_signif_lab, x = 0.03, y = 2.9, color = "antiquewhite4", size = 6, fontface = "bold")
cs_eid_vs_al

#Now, we will do the same for longitudinal correlation histograms

color <- rep(NA, nrow(l_correlations))
l_correlations$color <- color

for (i in 1:nrow(l_correlations)){
  color_mapping <- 
    ((abs(as.numeric(l_correlations$estimate[i])) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
  l_correlations$color[i] <- colors[200 - (round(color_mapping, digits =  0))] 
  
}

for (i in 1:nrow(l_correlations)){
  if (l_correlations$p_value[i] >= 0.05){
    l_correlations$signif_label[i] <- "ns"
  }
  if (l_correlations$p_value[i] < 0.05){
    l_correlations$signif_label[i] <- "*"
  }
  if (l_correlations$p_value[i] < 0.01){
    l_correlations$signif_label[i] <- "**"
  }
  if (l_correlations$p_value[i] < 0.001){
    l_correlations$signif_label[i] <- "***"
  }
}

x_axis <- "efficiency"
y_axis <- "rewards"
target_l_corrs_1 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eff_vs_r <- ggplot(data = target_l_corrs_1, mapping = aes(x=target_l_corrs_1[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_1[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_eff_vs_r

x_axis <- "burst_ratio"
y_axis <- "rewards"
target_l_corrs_2 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_br_vs_r <- ggplot(data = target_l_corrs_2, mapping = aes(x=target_l_corrs_2[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_2[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_br_vs_r

x_axis <- "timed_peak"
y_axis <- "rewards"
target_l_corrs_3 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_tp_vs_r <- ggplot(data = target_l_corrs_3, mapping = aes(x=target_l_corrs_3[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_3[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_tp_vs_r

x_axis <- "timing_spread"
y_axis <- "rewards"
target_l_corrs_4 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_ts_vs_r <- ggplot(data = target_l_corrs_4, mapping = aes(x=target_l_corrs_4[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_4[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_ts_vs_r

x_axis <- "early_inh_def"
y_axis <- "rewards"
target_l_corrs_5 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eid_vs_r <- ggplot(data = target_l_corrs_5, mapping = aes(x=target_l_corrs_5[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_5[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_eid_vs_r

x_axis <- "att_lapses"
y_axis <- "rewards"
target_l_corrs_6 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_r <- ggplot(data = target_l_corrs_6, mapping = aes(x=target_l_corrs_6[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_6[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_al_vs_r

x_axis <- "burst_ratio"
y_axis <- "efficiency"
target_l_corrs_7 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_br_vs_eff <- ggplot(data = target_l_corrs_7, mapping = aes(x=target_l_corrs_7[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_7[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_br_vs_eff

x_axis <- "timed_peak"
y_axis <- "efficiency"
target_l_corrs_8 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_tp_vs_eff <- ggplot(data = target_l_corrs_8, mapping = aes(x=target_l_corrs_8[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_8[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_tp_vs_eff

x_axis <- "timing_spread"
y_axis <- "efficiency"
target_l_corrs_9 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_ts_vs_eff <- ggplot(data = target_l_corrs_9, mapping = aes(x=target_l_corrs_9[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_9[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_ts_vs_eff

x_axis <- "early_inh_def"
y_axis <- "efficiency"
target_l_corrs_10 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eid_vs_eff <- ggplot(data = target_l_corrs_10, mapping = aes(x=target_l_corrs_10[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_10[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_eid_vs_eff

x_axis <- "att_lapses"
y_axis <- "efficiency"
target_l_corrs_11 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_eff <- ggplot(data = target_l_corrs_11, mapping = aes(x=target_l_corrs_11[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_11[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_al_vs_eff

x_axis <- "timed_peak"
y_axis <- "burst_ratio"
target_l_corrs_12 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_tp_vs_br <- ggplot(data = target_l_corrs_12, mapping = aes(x=target_l_corrs_12[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_12[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_tp_vs_br

x_axis <- "timing_spread"
y_axis <- "burst_ratio"
target_l_corrs_13 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_ts_vs_br <- ggplot(data = target_l_corrs_13, mapping = aes(x=target_l_corrs_13[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_13[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_ts_vs_br

x_axis <- "early_inh_def"
y_axis <- "burst_ratio"
target_l_corrs_14 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eid_vs_br <- ggplot(data = target_l_corrs_14, mapping = aes(x=target_l_corrs_14[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_14[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_eid_vs_br

x_axis <- "att_lapses"
y_axis <- "burst_ratio"
target_l_corrs_15 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_br <- ggplot(data = target_l_corrs_15, mapping = aes(x=target_l_corrs_15[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_15[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_al_vs_br

x_axis <- "timing_spread"
y_axis <- "timed_peak"
target_l_corrs_16 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_ts_vs_tp <- ggplot(data = target_l_corrs_16, mapping = aes(x=target_l_corrs_16[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_16[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_ts_vs_tp

x_axis <- "early_inh_def"
y_axis <- "timed_peak"
target_l_corrs_17 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eid_vs_tp <- ggplot(data = target_l_corrs_17, mapping = aes(x=target_l_corrs_17[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_17[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_eid_vs_tp

x_axis <- "att_lapses"
y_axis <- "timed_peak"
target_l_corrs_18 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_tp <- ggplot(data = target_l_corrs_18, mapping = aes(x=target_l_corrs_18[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_18[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_al_vs_tp

x_axis <- "early_inh_def"
y_axis <- "timing_spread"
target_l_corrs_19 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_eid_vs_ts <- ggplot(data = target_l_corrs_19, mapping = aes(x=target_l_corrs_19[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_19[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_eid_vs_ts

x_axis <- "att_lapses"
y_axis <- "timing_spread"
target_l_corrs_20 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_ts <- ggplot(data = target_l_corrs_20, mapping = aes(x=target_l_corrs_20[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_20[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.1, y = 9.5, color = "antiquewhite4", size = 10, fontface = "bold")
l_al_vs_ts

x_axis <- "att_lapses"
y_axis <- "early_inh_def"
target_l_corrs_21 <- as.data.frame(t(l_correlations[(which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)),3:38]))
target_color <- l_correlations$color[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
target_signif_lab <- l_correlations$signif_label[which(l_correlations$index_2==x_axis & l_correlations$index_1==y_axis)]
l_al_vs_eid <- ggplot(data = target_l_corrs_21, mapping = aes(x=target_l_corrs_21[,1])) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_histogram(binwidth = 0.15, color=target_color, fill=target_color, alpha=0.5, linewidth=1.5) +
  geom_vline(aes(xintercept=0), color="antiquewhite3", linetype="solid", linewidth=2.5) +
  geom_vline(aes(xintercept=mean(target_l_corrs_21[,1])), color="antiquewhite4", linetype="dashed", linewidth=2) +
  xlim(-1.15, 1.15) +
  ylim(0, 10) +
  annotate("text", label = target_signif_lab, x = -1.05, y = 9.5, color = "antiquewhite4", size = 6, fontface = "bold")
l_al_vs_eid

#Lastly, we create violin plots for visualizing within-session behavioral 
#consistency for all indices adjusting the color code according to the  
#magnitude of intra-class correlations

color_mapping_rw <- 
  ((abs(mean(rpt_rw$R_boot_link$subject)) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_rw_color <- colors[200 - (round(color_mapping_rw, digits =  0))] 

baseline_rewards <-
  ggplot(data = data_baseline,
       mapping = aes(x = session, y = rewards)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_rw_color, color = rpt_rw_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = 97, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_rewards

color_mapping_eff <- 
  ((abs(rpt_eff$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_eff_color <- colors[200 - (round(color_mapping_eff, digits =  0))] 

baseline_efficiency <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = efficiency)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_eff_color, color = rpt_eff_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = .8, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_efficiency

color_mapping_br <- 
  ((abs(rpt_br$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_br_color <- colors[200 - (round(color_mapping_br, digits =  0))] 

baseline_burst_ratio <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = burst_ratio)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_br_color, color = rpt_br_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = -1.98, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_burst_ratio

color_mapping_eid <- 
  ((abs(rpt_eid$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_eid_color <- colors[200 - (round(color_mapping_eid, digits =  0))] 

baseline_early_inh_def <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = early_inh_def)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_eid_color, color = rpt_eid_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = .49, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_early_inh_def

color_mapping_tp <- 
  ((abs(rpt_tp$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_tp_color <- colors[200 - (round(color_mapping_tp, digits =  0))] 

baseline_timed_peak <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = timed_peak)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_tp_color, color = rpt_tp_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = 4.2, y = 22.8, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_timed_peak

color_mapping_ts <- 
  ((abs(rpt_ts$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_ts_color <- colors[200 - (round(color_mapping_ts, digits =  0))] 

baseline_timing_spread <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = timing_spread)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_ts_color, color = rpt_ts_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = .034, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_timing_spread

color_mapping_al <- 
  ((abs(rpt_al$R$subject) - min_abs_rho) / (max_abs_rho - min_abs_rho))*199
rpt_al_color <- colors[200 - (round(color_mapping_al, digits =  0))] 

baseline_att_lapses <-
  ggplot(data = data_baseline,
         mapping = aes(x = session, y = att_lapses)) +
  geom_violin(alpha=0.5, linewidth=1.5, fill=rpt_al_color, color = rpt_al_color) +
  geom_line(mapping = aes(group = subject), linewidth=0.75,
            position = position_dodge(0.15), color = "antiquewhite4") +
  geom_point(mapping = aes(group = subject),
             size = 3, shape = 21, fill = "antiquewhite3", color = "antiquewhite4",
             position = position_dodge(0.15)) +
  theme_classic(base_size = 25) +
  theme(panel.border = element_rect(fill = NA),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate("text", label = "**", x = .8, y = 3.7, color = "antiquewhite4", size = 10, fontface = "bold")
baseline_att_lapses 

#Create a list with all of the plots that have been generated
MM_corr_plots <- list(baseline_rewards, l_eff_vs_r, l_br_vs_r, l_eid_vs_r, l_tp_vs_r, l_ts_vs_r, l_al_vs_r,
                      cs_rw_vs_eff, baseline_efficiency, l_br_vs_eff, l_eid_vs_eff, l_tp_vs_eff, l_ts_vs_eff, l_al_vs_eff,
                      cs_rw_vs_br, cs_eff_vs_br, baseline_burst_ratio, l_eid_vs_br, l_tp_vs_br, l_ts_vs_br, l_al_vs_br,
                      cs_rw_vs_eid, cs_eff_vs_eid, cs_br_vs_eid, baseline_early_inh_def, l_eid_vs_tp, l_eid_vs_ts, l_al_vs_eid,
                      cs_rw_vs_tp, cs_eff_vs_tp, cs_br_vs_tp, cs_tp_vs_eid, baseline_timed_peak, l_ts_vs_tp, l_al_vs_tp,
                      cs_rw_vs_ts, cs_eff_vs_ts, cs_br_vs_ts, cs_ts_vs_eid, cs_tp_vs_ts, baseline_timing_spread, l_al_vs_ts, 
                      cs_rw_vs_al, cs_eff_vs_al, cs_br_vs_al, cs_eid_vs_al, cs_tp_vs_al, cs_ts_vs_al, baseline_att_lapses)

#Load this package to create the correlation matrix
library(GGally)

#Pool all the plots
MM_cpm <- ggmatrix(MM_corr_plots, 7, 7,
                   xAxisLabels = c("Rewards", "Efficiency", "Burst Ratio", "Early Inh. Deficit", "Timed Peak", "Timing Spread", "Attentional Lapses"),
                   yAxisLabels = c("Rewards", "Efficiency", "Burst Ratio", "Early Inh. Deficit", "Timed Peak", "Timing Spread", "Attentional Lapses"),)
MM_cpm + theme(strip.text = element_text(size = 25)) 

#For optimal visualization, save as 2000 height and 2000 width

#The following chunk will create the color gradient scale that must 
#be added manually
xx <- c(min_abs_rho, max_abs_rho)
yy <- xx
df <- data.frame(xx, yy, colors)
colnames(df) <- c("xx", "yy", "colors")
ggplot(df) +
  geom_point(aes(xx, yy, col = yy)) +
  scale_colour_gradientn(colours = rev(colors), breaks = c(.0, .1, .2, .3, .4, .5, .6, .7, .8)) +
  theme_classic(base_size = 25) +
  theme(legend.direction = "horizontal", legend.key.width = unit(8, "cm"), 
        legend.key.height = unit(1.5, "cm")) +
  labs(color = "Absolute value of correlations") +
  guides(color=guide_colorbar(title.position = "top",
                              title.hjust = 0.5)) 

#Then, we can continue with script number 4
  