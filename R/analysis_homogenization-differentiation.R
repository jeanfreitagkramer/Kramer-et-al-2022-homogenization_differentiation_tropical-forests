
#frequency and exploratory plots 

#Kramer-et-al-2022-homogenization_differentiation_tropical-forests

library(tidyverse)
library(ggplot2)
library(scales)
library(cowplot)
library(grid)
library(gridExtra)
library(readxl)
library(ggpubr)

#############################################################

#years

ano.cap1 <- read_excel("data_homogenization_refined.xlsx", sheet = "year")

year<-ggplot(data = ano.cap1,) + geom_bar(aes(x = numero.anos, y = quantidade),color = "black", fill = "gray60", 
                                                                      stat='identity') +
  geom_smooth(aes(x = year.number, y = quantidade), size = 1.5, se = FALSE, color = 'black', linetype = 2) +
  xlab ("") +
  ylab ("Number of papers") +
  theme_classic() +
  theme (axis.text.x = element_text(size = 16, angle = 90)) +
  theme (axis.text.y = element_text(size = 16), axis.ticks.x = element_blank()) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) +
  scale_y_continuous (breaks = seq(0,30,2)) +
  scale_x_continuous (breaks = seq(1999,2021,1))
year

#############################################################

#homogenization and differentiation

types <- read_excel("data_homogenization_refined.xlsx", sheet = "type")

type<-ggplot(data = types, aes(x=Propriedade, y=n, fill=Mudanca)) + geom_bar(aes(y = (..count..)/sum(..count..)), 
                                                                                color = 'black') +
  xlab ("") +
  ylab ("Frequency") +
  labs (fill = "") +
  theme_classic() +
  theme (axis.text.x = element_text(size = 14)) +
  theme (axis.text.y = element_text(size = 14), axis.ticks.x = element_blank()) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) + 
  scale_y_continuous (breaks = seq(0,100,.15), labels=scales::percent) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = "element_text"(size = 14)) +
  scale_x_discrete(limits = c("Taxonomic","Functional","Phylogenetic"))
type

#############################################################

#spatial and temporal scales

scales.cap1 <- read_excel("data_homogenization_refined.xlsx", sheet = "scales")

scales<-ggplot(data = scales.cap1, aes(x=Escala, y=n, fill=Mudanca)) + geom_bar(aes(y = (..count..)/sum(..count..)), 
                                                                                color = 'black') +
  xlab ("Approach") +
  ylab ("Frequency") +
  labs (fill = "") +
  theme_classic() +
  theme (axis.text.x = element_text(size = 16)) +
  theme (axis.title.x = element_text(size = 19)) +
  theme (axis.text.y = element_text(size = 16), axis.ticks.x = element_blank()) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) + 
  scale_y_continuous (breaks = seq(0,100,.15), labels=scales::percent) +
  scale_fill_brewer(palette="Dark2") +
  scale_x_discrete(labels = c("Spatio-temporal" = "Temporal")) +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = "element_text"(size = 14))
scales

#############################################################

#spatial scale size
amp.spat <- read_excel("data_homogenization_refined.xlsx", sheet = "spatial")

spatial.amp<-ggplot(data = amp.spat, aes(x=size.spat, y=n.spat, fill=spat.mud)) + geom_bar(aes(y = (..count..)/sum(..count..)), 
                                                      position = "fill", color = 'black', na.rm = T) +
  xlab ("Spatial") +
  ylab ("") +
  labs (fill = "") + 
  theme_classic() +
  theme (axis.text.x = element_text(size = 14)) +
  theme (axis.title.x = element_text(size = 16)) +
  theme (axis.text.y = element_text(size = 14), axis.ticks.x = element_blank()) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) + 
  scale_y_continuous (breaks = seq(0,100,.20), labels=scales::percent) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = "element_text"(size = 14, face = "bold"), legend.text = "element_text"(size = 14)) +
  scale_x_discrete(labels = c("<_20" = "Small 
(< 20 km²)","21_200" = "Moderate
(21-200)",
"201_1000" = "Large
(201-1000)",">_1000" = "Very large
(> 1000)"), limits = c(">_1000", "201_1000","21_200", "<_20"))
spatial.amp

#############################################################

#temporal scale size

amp.temp <- read_excel("data_homogenization_refined.xlsx", sheet = "temporal")

temporal.amp<-ggplot(data = amp.temp, aes(x=size.temp, y=n.temp, fill=temp.mud)) + geom_bar(aes(y = (..count..)/sum(..count..)), 
                                                                                           position = "fill", color = 'black', na.rm = T) +
  xlab ("Temporal") +
  ylab ("") +
  labs (fill = "") + 
  theme_classic() +
  theme (axis.text.x = element_text(size = 14)) +
  theme (axis.title.x = element_text(size = 16)) +
  theme (axis.text.y = element_text(size = 14), axis.ticks.x = element_blank()) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) + 
  scale_y_continuous (breaks = seq(0,100,.20), labels=scales::percent) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = "element_text"(size = 14, face = "bold"), legend.text = "element_text"(size = 14)) +
  scale_x_discrete(labels = c("<_5" = "Small 
(< 5 years)","6_15" = "Moderate
(6-15)", "16_40" = "Large
(16-40)",">_40" = "Very large
(> 40)"), limits = c(">_40", "16_40","6_15", "<_5"))
temporal.amp

#both spatial and temporal scale sizes

scales.homo.diffe<-plot_grid(spatial.amp + xlab("") + ylab("") + theme(legend.position="none"),
                             temporal.amp + xlab("") + ylab("") + theme(legend.position="none"),
                             align = 'vh', labels = c("(a)", "(b)", nrow = 1))
scales.homo.diffe

scales.homo.diffe<-ggarrange(spatial.amp, temporal.amp, ncol = 2, 
                             labels = c("(a)", "(b)"),
                             common.legend = TRUE, legend = "top")
scales.homo.diffe

x.grob<-textGrob("Scale size", 
                 gp=gpar(fontsize=19))

y.grob<-textGrob("Frequency", 
                 gp=gpar(fontsize=19), rot=90)

scales.homo.diffe<-grid.arrange(arrangeGrob(scales.homo.diffe, left = y.grob, bottom = x.grob))
scales.homo.diffe

#############################################################

#drivers or determinants of change
drivers.cap1 <- read_excel("data_homogenization_refined.xlsx", sheet = "drivers")

drivers<-ggplot(data = drivers.cap1, aes(x=Condutor, y=n, fill=Mudanca)) + geom_bar(aes(y = (..count..)/sum(..count..)), 
                                                                                   color = 'black') +
  coord_flip() +
  xlab ("") +
  ylab ("Frequency") +
  labs (fill = "") +
  theme_classic() +
  theme (axis.text.x = element_text(size = 16), axis.ticks.y = element_blank()) +
  theme (axis.text.y = element_text(size = 16)) +
  theme (axis.title.y = element_text(size = 19, angle = 90)) + 
  theme (axis.title.x = element_text(size = 19)) +
  scale_y_continuous (breaks = seq(0,100,.15), labels=scales::percent) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = "element_text"(size = 14)) +
  scale_x_discrete(labels = c("Chronic anthropogenic disturbances" = "Chronic anthropogenic 
disturbance"),limits = c("Fragmentation","Land-use change","Habitat loss","Climate change",
                          "Biological invasion", "Chronic anthropogenic disturbances", "Fire"))
drivers

#############################################################

#saving the plots

save_plot("year.tiff", anos, dpi=300, base_aspect_ratio = 3:1)
save_plot("type.tiff", type, dpi=300, base_aspect_ratio = 3:1)
save_plot("scales.tiff", scales, dpi=300, base_aspect_ratio = 3:1)
save_plot("drivers.tiff", drivers, dpi=300, base_aspect_ratio = 3:1)
save_plot("scales.homo.diffe.tiff",  scales.homo.diffe, dpi=300, base_aspect_ratio = 3:1)
