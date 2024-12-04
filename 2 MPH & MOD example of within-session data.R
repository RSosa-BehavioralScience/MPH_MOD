#We can create Figure 3 by selecting data from a single session and subject

#This is the data we are going to work with
sample

#For safety reasons, move the data to a new data frame
sample_w <- sample

#Convert units in timestamps to seconds
sample_w$timestamp <- sample_w$timestamp/20 

#Now, convert seconds to minutes
sample_w$timestamp <- sample_w$timestamp/60 

#Previsualization
plot(sample_w$i_e_i ~ sample_w$timestamp)

#Load required package
library(ggplot2)

#Generate labels for the horizontal axis
my_labels <- c(0,
               "", "", "",  "", "", "", "",  "", "", "", "", 4,
               "", "", "",  "", "", "", "",  "", "", "", "", 8,
               "", "", "",  "", "", "", "",  "", "", "", "", 12,
               "", "", "",  "", "", "", "",  "", "", "", "", 16,
               "", "", "",  "", "", "", "",  "", "", "", "", 20,
               "", "", "",  "", "", "", "",  "", "", "", "", 24,
               "", "", "",  "", "", "", "",  "", "", "", "", 28,
               "", "", "",  "", "", "", "",  "", "", "", "", 32,
               "", "", "",  "", "", "", "",  "", "", "", "", 36,
               "", "", "",  "", "", "", "",  "", "", "", "", 40)

#Create plot showing variations in IRTs with color-coded classifications through time within a session
ggplot(data=sample_w, aes(x=timestamp, y=i_e_i)) +
  geom_point(aes(color=class, size=10)) +
  geom_hline(yintercept = 15, linetype = "dashed") +
  theme_classic(base_size = 25) + 
  labs(x="Minutes from Session Start", y="Inter Response Times") +
  scale_x_continuous(labels = my_labels,
                     breaks = seq(0,40, 1/3)) +
  theme(legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.line = element_blank()) +
  #guides(x = guide_prism_minor()) +
  scale_color_manual(values=c("#44AA99", "#D55E00", "#F0E442", "#661100", "#0072B2"))

#Save with size 2000x500 for optimal visualization and add sliding windows and labels manually

#Now, you may proceed with script number 3 or save the current evironment to continue later.
