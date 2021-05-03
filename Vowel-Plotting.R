library(tidyverse)
library(scales)
library(rcompanion)
library(gmodels)
library(WellspringsR)
library(vowels)
library(graphics)
library(gmodels)
library(ggplot2)
library(ggpubr)


getwd()
setwd("C:/Users/Kate Lindsey/Desktop/R/")
read.csv(file = "PR_Vowels.csv") -> PR

PR %>% 
  group_by(vowel_id, language) %>%
  summarise(F1Mean = mean(F1), F2Mean = mean(F2)) -> PRWordAve


ggplot(PRWordAve, aes(x = F2Mean, y = F1Mean, color = vowel_id, label = language)) + 
  geom_text(alpha = 0.5, size = 3) +
  stat_ellipse(level = 0.95, aes(lty=vowel_id), geom = "polygon", alpha = 0.1) +
  scale_linetype_manual(values=c(1,3,5,2,4,6,2,3)) +
  geom_label(size = 3)+
  scale_x_reverse() + scale_y_reverse() +
  theme_classic()+
  #facet_wrap(~OriV) +
  theme(axis.text  = element_text(size=18), 
        legend.position="left", 
        legend.title=element_text(size=18),
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        plot.title = element_text(size=22)) +
  labs(x = "F2", y = "F1")+
  ggtitle("All Vowels")
