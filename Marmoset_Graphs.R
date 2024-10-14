# Marmoset Graphs
# E. Lamont
# 10/9/24

source("Import_data.R") # to get my_pipeSummary

my_marm <- my_pipeSummary %>% filter(Sample_Type == "Marmoset")

my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=14), 
        axis.text.x = element_text(angle = 0, size=14, vjust=0, hjust=0.5),
        # axis.text.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text.y = element_text(size=14), 
        plot.subtitle = element_text(size=9), 
        plot.margin = margin(10, 10, 10, 20),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_blank()
  )


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

###########################################################
################ PRETTY GRAPH FOR SHERM  ##################

## Number reads 
Reads_marm <- my_marm %>% 
  
  ggplot(aes(x = Week, y = N_Genomic)) + 
  geom_point(aes(fill = Sample_Type,), shape = 21, size = 6, alpha = 0.8, stroke = 0.8) + 
  scale_fill_manual("Sample type", values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  # scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 3, box.padding = 0.4, segment.color = NA) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  facet_grid(~ Sample_Type, scales = "free", space = "free") + 
  
  scale_y_continuous(limits = c(0,6500000), breaks = seq(0, 6500000, 1000000)) +
  
  labs(title = NULL,
       subtitle = NULL, 
       x = NULL, 
       y = "# reads aligning to Mtb genome") + 
  
  my_plot_themes

Reads_marm

ggsave(Reads_marm,
       file = "Reads_marm.pdf",
       path = "Figures/Marmoset",
       width = 6, height = 4, units = "in")


###########################################################
################### N_GENOMIC VS CFU/G ####################

ReadsVsCFU_marm <- my_marm %>% 
  
  ggplot(aes(x = CFU_per_g, y = N_Genomic)) + 
  geom_point(aes(fill = Sample_Type,), shape = 21, size = 6, alpha = 0.8, stroke = 0.8) + 
  scale_fill_manual("Sample type", values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  # scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 3, box.padding = 0.4, segment.color = NA) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  # facet_grid(~ Sample_Type, scales = "free", space = "free") + 
  
  scale_y_continuous(limits = c(0,6500000), breaks = seq(0, 6500000, 1000000)) +
  
  labs(title = "Marmoset samples",
       subtitle = NULL, 
       x = "CFU/g", 
       y = "# reads aligning to Mtb genome") + 
  
  my_plot_themes

ReadsVsCFU_marm

ggsave(ReadsVsCFU_marm,
       file = "ReadsVsCFU_marm.pdf",
       path = "Figures/Marmoset",
       width = 6, height = 4, units = "in")


class(my_marm$CFU_per_g)
