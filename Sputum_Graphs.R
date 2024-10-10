# Sputum Graphs
# E. Lamont
# 10/3/24

source("Import_data.R") # to get my_pipeSummary

my_sputum <- my_pipeSummary %>% filter(Sample_Type == "Sputum")

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(angle = 0, size=14, vjust=0, hjust=0.5),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14), 
        plot.subtitle = element_text(size=9), 
        plot.margin = margin(10, 10, 10, 20))


###########################################################
###################### WEEK CHANGES #######################

## Number reads 
WeekVsReads_sputum <- my_sputum %>% 
  
  ggplot(aes(x = Week, y = N_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Sputum: Week vs number reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng") + 
  
  my_plot_themes

WeekVsReads_sputum

ggsave(WeekVsReads_sputum,
       file = "WeekVsReads_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")

## Percent reads 
WeekVsPercent_sputum <- my_sputum %>% 
  
  ggplot(aes(x = Week, y = P_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  # geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, 10)) + 
  
  labs(title = "Sputum: Week vs percent reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng") + 
  
  my_plot_themes

WeekVsPercent_sputum

ggsave(WeekVsPercent_sputum,
       file = "WeekVsPercent_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")


###########################################################
############## WEEK CHANGES - NICER GRAPHS ################

# Stop scientific notation
# options(scipen = 999) 
options(scipen = 0) # To revert back to default


## Number reads 
WeekVsReads_sputum2 <- my_sputum %>% 

  ggplot(aes(x = Week, y = N_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Week), size = 6) + 
  scale_color_manual(values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = paste0(P_Genomic, "%")), size= 4, box.padding = 0.4) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  # scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Sputum: Week vs number reads aligned to Mtb",
       subtitle = "Label is percent of reads aligned to Mtb", 
       x = "Weeks after start of antibiotics", 
       y = "# reads aligning to Mtb genome") + 
  
  my_plot_themes

WeekVsReads_sputum2

ggsave(WeekVsReads_sputum2,
       file = "WeekVsReads_sputum2.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")

## Percent reads 
WeekVsPercent_sputum2 <- my_sputum %>% 
  
  ggplot(aes(x = Week, y = P_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Week), size = 6) + 
  scale_color_manual(values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 4, box.padding = 0.4) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  # geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  scale_y_continuous(limits = c(0,55), breaks = seq(0, 55, 10)) + 
  
  labs(title = "Sputum: Week vs percent reads aligned to Mtb",
       subtitle = "Label is number of reads aligned to Mtb", 
       x = "Weeks after start of antibiotics", 
       y = "% reads aligning to Mtb genome") + 
  
  my_plot_themes

WeekVsPercent_sputum2

ggsave(WeekVsPercent_sputum2,
       file = "WeekVsPercent_sputum2.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")


###########################################################
######################## CT CHANGES #######################

## Number reads 
ctVsReads_sputum <- my_sputum %>% 
  
  ggplot(aes(x = ct, y = N_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.5) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Sputum: Ct value vs number reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng \n ** 2 samples removed because ct=NA **") + 
  
  my_plot_themes

ctVsReads_sputum

ggsave(ctVsReads_sputum,
       file = "ctVsReads_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")

## Percent reads 
ctVsPercent_sputum <- my_sputum %>% 
  
  ggplot(aes(x = ct, y = P_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.5) + 
  
  # geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, 10)) + 
  
  labs(title = "Sputum: Ct value vs percent reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng \n ** 2 samples removed because ct=NA **") + 
  
  my_plot_themes

ctVsPercent_sputum

ggsave(ctVsPercent_sputum,
       file = "ctVsPercent_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")


###########################################################
###################### TTD CHANGES ########################

## Number reads 
ttdVsReads_sputum <- my_sputum %>% 
  
  ggplot(aes(x = ttd, y = N_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.5) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Sputum: TTD value vs number reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng") + 
  
  my_plot_themes

ttdVsReads_sputum

ggsave(ttdVsReads_sputum,
       file = "ttdVsReads_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")

## Percent reads 
ttdVsPercent_sputum <- my_sputum %>% 
  
  ggplot(aes(x = ttd, y = P_Genomic)) + 
  geom_point(aes(color = Probe), size = 2.5) + 
  # scale_color_manual(values = cbPalette5) + 
  # scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.5) + 
  
  # geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,60), breaks = seq(0, 60, 10)) + 
  
  labs(title = "Sputum: TTD value vs percent reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion, number is probe ng") + 
  
  my_plot_themes

ttdVsPercent_sputum

ggsave(ttdVsPercent_sputum,
       file = "ttdVsPercent_sputum.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")





