# Marmoset Graphs
# E. Lamont
# 10/9/24

source("Import_data.R") # to get my_pipeSummary

my_marm <- my_pipeSummary %>% filter(Sample_Type == "Marmoset")

###########################################################
####################### NO X AXIS?  #######################

## Number reads 
WeekVsReads_marm <- my_marm %>% 
  
  ggplot(aes(x = Week, y = N_Genomic)) + 
  geom_point(aes(color = Sample_Type), size = 3) + 
  scale_color_manual(values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  # scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = P_Genomic), size= 2) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  # scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Marmoset: Number reads aligned to Mtb",
       subtitle = "Label is percent of reads aligned to Mtb", 
       # x = "Weeks after start of antibiotics", 
       y = "# reads aligning to Mtb genome") + 
  
  my_plot_themes

WeekVsReads_marm

ggsave(WeekVsReads_marm,
       file = "WeekVsReads_marm.pdf",
       path = "Figures/Marmoset",
       width = 6, height = 4, units = "in")

## Percent reads 
WeekVsPercent_marm <- my_marm %>% 
  
  ggplot(aes(x = Week, y = P_Genomic)) + 
  geom_point(aes(color = Sample_Type), size = 3) + 
  scale_color_manual(values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  # scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = N_Genomic), size= 2) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  # geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  # scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 500000)) + 
  
  labs(title = "Marmoset: Percent reads aligned to Mtb",
       subtitle = "Label is number of reads aligned to Mtb", 
       # x = "Weeks after start of antibiotics", 
       y = "% reads aligning to Mtb genome") + 
  
  my_plot_themes

WeekVsPercent_marm

ggsave(WeekVsPercent_marm,
       file = "WeekVsPercent_marm.pdf",
       path = "Figures/Marmoset",
       width = 6, height = 4, units = "in")
