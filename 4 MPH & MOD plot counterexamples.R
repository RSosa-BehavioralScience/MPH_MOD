#Create plots with extreme favorable and unfavorable values of every couple
#...of IRT-based indices 

#Install or load required packages
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)

#High burst ratio vs low early inhibitory deficit

i <- 317

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

h_br_l_eid <-
ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[317]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[317]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[317]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[317]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[317]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
  
h_br_l_eid
y_range <- ggplot_build(h_br_l_eid)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_br_l_eid <- h_br_l_eid + annotate("text", x = 40, y = y_position, label = "Subject: R34\nGroup: MOD\nPhase: W\nSession: 3", hjust = 0, size = 3)

#High burst ratio vs high timed peak 

i <- 397

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

h_br_h_tp <-
ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[397]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[397]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[397]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[397]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[397]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_br_h_tp
y_range <- ggplot_build(h_br_h_tp)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_br_h_tp <- h_br_h_tp + annotate("text", x = 40, y = y_position, label = "Subject: R02\nGroup: MOD\nPhase: W\nSession: 6", hjust = 0, size = 3)

#High burst ratio vs low timing spread 

i <- 310

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

h_br_l_ts <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[310]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[310]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[310]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[310]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[310]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_br_l_ts
y_range <- ggplot_build(h_br_l_ts)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_br_l_ts <- h_br_l_ts + annotate("text", x = 40, y = y_position, label = "Subject: R26\nGroup: MPH\nPhase: W\nSession: 3", hjust = 0, size = 3)

#high burst ratio vs low attentional lapses 

i <- 425

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

h_br_l_al <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[425]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[425]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[425]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[425]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[425]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_br_l_al
y_range <- ggplot_build(h_br_l_al)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_br_l_al <- h_br_l_al + annotate("text", x = 40, y = y_position, label = "Subject: R34\nGroup: MOD\nPhase: W\nSession: 6", hjust = 0, size = 3)

#High early inhibitory deficit vs low burst ratio 

i <- 249

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

h_eid_l_br <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[249]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[249]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[249]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[249]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[249]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_eid_l_br
y_range <- ggplot_build(h_eid_l_br)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_eid_l_br <- h_eid_l_br + annotate("text", x = 40, y = y_position, label = "Subject: R37\nGroup: MOD\nPhase: D\nSession: 4", hjust = 0, size = 3)

#High early inhibitory deficit vs high timed peak 

i <- 185

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

h_eid_h_tp <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[185]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[185]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[185]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[185]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[185]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_eid_h_tp
y_range <- ggplot_build(h_eid_h_tp)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_eid_h_tp <- h_eid_h_tp + annotate("text", x = 40, y = y_position, label = "Subject: R05\nGroup: MOD\nPhase: D\nSession: 3", hjust = 0, size = 3)

#High early inhibitory deficit vs low timing spread 

i <- 400

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

h_eid_l_ts <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[400]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[400]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[400]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[400]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[400]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_eid_l_ts
y_range <- ggplot_build(h_eid_l_ts)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_eid_l_ts <- h_eid_l_ts + annotate("text", x = 40, y = y_position, label = "Subject: R05\nGroup: MOD\nPhase: W\nSession: 6", hjust = 0, size = 3)

#High early inhibitory defict vs low attentional lapses 

i <- 341

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

h_eid_l_al <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[341]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[341]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[341]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[341]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[341]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
h_eid_l_al
y_range <- ggplot_build(h_eid_l_al)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_eid_l_al <- h_eid_l_al + annotate("text", x = 40, y = y_position, label = "Subject: R19\nGroup: SUC\nPhase: W\nSession: 4", hjust = 0, size = 3)

#Low timed peak vs low burst ratio 

i <- 92

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

l_tp_l_br <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[92]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[92]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[92]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[92]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[92]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
l_tp_l_br
y_range <- ggplot_build(l_tp_l_br)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
l_tp_l_br <- l_tp_l_br + annotate("text", x = 40, y = y_position, label = "Subject: R21\nGroup: SUC\nPhase: B\nSession: 4", hjust = 0, size = 3)

#Low timed peak vs low early inhibitory deficit 

i <- 197

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

l_tp_l_eid <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[197]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[197]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[197]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[197]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[197]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
l_tp_l_eid
y_range <- ggplot_build(l_tp_l_eid)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
l_tp_l_eid <- l_tp_l_eid + annotate("text", x = 40, y = y_position, label = "Subject: R18\nGroup: MOD\nPhase: D\nSession: 3", hjust = 0, size = 3)

#Low timed peak vs low timing spread 

i <- 15

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

l_tp_l_ts <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[15]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[15]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[15]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[15]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[15]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
l_tp_l_ts
y_range <- ggplot_build(l_tp_l_ts)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
l_tp_l_ts <- l_tp_l_ts + annotate("text", x = 40, y = y_position, label = "Subject: R16\nGroup: MPH\nPhase: B\nSession: 2", hjust = 0, size = 3)

#Low timed peak vs low attentional lapses 

i <- 1

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

l_tp_l_al <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[1]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[1]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[1]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[1]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[1]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))
l_tp_l_al
y_range <- ggplot_build(l_tp_l_al)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
l_tp_l_al <- l_tp_l_al + annotate("text", x = 40, y = y_position, label = "Subject: R01\nGroup: MPH\nPhase: B\nSession: 2", hjust = 0, size = 3)

#High timing spread vs low burst ratio 

i <- 104

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

h_ts_l_br <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[104]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[104]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[104]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[104]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[104]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_ts_l_br
y_range <- ggplot_build(h_ts_l_br)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_ts_l_br <- h_ts_l_br + annotate("text", x = 40, y = y_position, label = "Subject: R36\nGroup: MPH\nPhase: B\nSession: 4", hjust = 0, size = 3)

#High timing spread vs low eid 

i <- 117

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

h_ts_l_eid <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[117]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[117]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[117]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[117]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[117]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
                     limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_ts_l_eid
y_range <- ggplot_build(h_ts_l_eid)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_ts_l_eid <- h_ts_l_eid + annotate("text", x = 40, y = y_position, label = "Subject: R10\nGroup: MOD\nPhase: B\nSession: 5", hjust = 0, size = 3)

#High timing spread vs high timed peak 

i <- 61

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

h_ts_h_tp <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[61]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[61]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[61]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[61]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[61]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_ts_h_tp
y_range <- ggplot_build(h_ts_h_tp)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_ts_h_tp <- h_ts_h_tp + annotate("text", x = 40, y = y_position, label = "Subject: R28\nGroup: MOD\nPhase: B\nSession: 3", hjust = 0, size = 3)

#High timing spread vs low attentional lapses 

i <- 297

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

h_ts_l_al <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[297]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[297]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[297]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[297]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[297]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_ts_l_al
y_range <- ggplot_build(h_ts_l_al)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_ts_l_al <- h_ts_l_al + annotate("text", x = 40, y = y_position, label = "Subject: R11\nGroup: MOD\nPhase: W\nSession: 3", hjust = 0, size = 3)

#High attentional lapses vs low burst ratio 

i <- 113

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

h_al_l_br <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[113]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[113]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[113]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[113]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[113]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_al_l_br
y_range <- ggplot_build(h_al_l_br)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_al_l_br <- h_al_l_br + annotate("text", x = 40, y = y_position, label = "Subject: R05\nGroup: MOD\nPhase: B\nSession: 5", hjust = 0, size = 3)

#High attentional lapses vs low early inhibitory deficit 

i <- 304

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

h_al_l_eid <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[304]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[304]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[304]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[304]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[304]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_al_l_eid
y_range <- ggplot_build(h_al_l_eid)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_al_l_eid <- h_al_l_eid + annotate("text", x = 40, y = y_position, label = "Subject: R18\nGroup: MOD\nPhase: W\nSession: 3", hjust = 0, size = 3)

#High attentional lapses vs high in timed peak 

i <- 427

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

h_al_h_tp <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[427]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[427]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[427]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[427]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[427]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_al_h_tp
y_range <- ggplot_build(h_al_h_tp)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_al_h_tp <- h_al_h_tp + annotate("text", x = 40, y = y_position, label = "Subject: R36\nGroup: MPH\nPhase: W\nSession: 6", hjust = 0, size = 3)

#High attentional lapses vs low ts 

i <- 77

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

h_al_l_ts <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=1, alpha=.4, length = unit(0.045, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
  limits = c(0,70),
  expand = expansion(add=c(0,0))) +
  #scale_y_continuous(expand = c(0,0.008,0,0.002)) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses", 
                                                                                                    "early inhibitory deficit", 
                                                                                                    "timed responses",
                                                                                                    "attentional lapses",
                                                                                                    "other non-burst responses"))

h_al_l_ts
y_range <- ggplot_build(h_al_l_ts)$layout$panel_scales_y[[1]]$range$range
y_position <- y_range[1] + 0.65 * (y_range[2] - y_range[1])
h_al_l_ts <- h_al_l_ts + annotate("text", x = 40, y = y_position, label = "Subject: R05\nGroup: MOD\nPhase: B\nSession: 4", hjust = 0, size = 3)

blank <- ggplot() + theme_void()

MM_examples <- list(blank, h_eid_l_br, l_tp_l_br, h_ts_l_br, h_al_l_br,
                    h_br_l_eid, blank, l_tp_l_eid, h_ts_l_eid, h_al_l_eid,
                    h_br_h_tp, h_eid_h_tp, blank, h_ts_h_tp, h_al_h_tp,
                    h_br_l_ts, h_eid_l_ts, l_tp_l_ts, blank, h_al_l_ts,
                    h_br_l_al, h_eid_l_al, l_tp_l_al, h_ts_l_al, blank)

library(GGally)

#Create Figure 4 by compiling all the plots

MM_cpm <- ggmatrix(MM_examples, 5, 5,
                   xAxisLabels = c("High Burst Ratio", "High Early Inhibitory Deficit", "Low Timed Peak", "High Timing Spread", "High Attentional Lapses"),
                   yAxisLabels = c("Low Burst Ratio", "Low Early Inh. Def.", "High Timed Peak", "Low Timing Spread", "Low Att. Lapses"),
                   showYAxisPlotLabels = FALSE, switch = "y")
MM_cpm + theme(strip.text = element_text(size = 10)) 

#dimensions: 1000x700

#You need to add the legend manually to this figure by cropping it from the following plot.

legend <-
  ggplot(data = sample, mapping = aes(x = i_e_i)) +
  geom_density(color="gray50", fill="#B9CCE5", alpha=.2, adjust=0.6) +
  geom_rug(mapping = aes(colour=class), linewidth=2, alpha=1, length = unit(0.09, "npc")) +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$antimode), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$antimode_2), linetype="dashed") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$t1), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$t2), color="gray40") +
  geom_vline(aes(xintercept=MM_processed_database[[77]]$timed_peak), 
             color="red", linetype="dotted", linewidth=.8) +
  theme_classic(base_size = 12) + 
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "transparent"),
        #legend.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Inter-Response Interval", y="Density", color="Class") +
  scale_x_continuous(breaks = seq(0, 70, 15), 
                     limits = c(0,70),
                     expand = expansion(add=c(0,0))) +
  #scale_y_continuous(expand = c(0,0.008,0,0.002)) +
  #scale_color_brewer(palette = "Dark2")
  #scale_color_discrete() +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"), labels=c("burst responses   ", 
                                                                                                    "early inhibitory deficit   ", 
                                                                                                    "timed responses   ",
                                                                                                    "attentional lapses   ",
                                                                                                    "other non-burst responses   "))

legend

#Now, you can continue with script number 5

