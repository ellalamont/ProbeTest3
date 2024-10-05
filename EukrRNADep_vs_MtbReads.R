# Compare eukaryotic rRNA depletion to Mtb reads (# or %)
# E. Lamont
# 10/3/24

source("Import_data.R") # to get my_pipeSummary

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 12),
        plot.title = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        axis.text.x = element_text(angle = 0, size=10, vjust=0, hjust=0.5),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=9), 
        plot.margin = margin(10, 10, 10, 20))

###########################################################
################## SCATTER NUMBER READS ###################

DepVsReads_scatter <- my_pipeSummary %>% 
  filter(Sample_Type == "THP1") %>%
  filter(Hyb_Time == 4) %>%
  filter(Probe == "4A") %>% # All the Eukaryotic mRNA depleted samples are 4A, but different concentrations, want to keep probe type consistant with the non-eukaryotic mRNA depleted samples
  
  ggplot(aes(x = EukrRNADep, y = N_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Ra_cells), size = 2.5) + 
  scale_color_manual(values = cbPalette5[2]) + 
  scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 

  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,6500000), breaks = seq(0, 6500000, 1000000)) + 
  
  labs(title = "Eukaryotic rRNA depletion vs number reads aligned to Mtb",
       subtitle = "4Hr hybridisation, all Probe 4A") + 
  
  my_plot_themes

DepVsReads_scatter

ggsave(DepVsReads_scatter,
       file = "EukrRNADepVsReads_scatter.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")


###########################################################
################# SCATTER PERCENT READS ###################

DepVsPercent_scatter <- my_pipeSummary %>% 
  filter(Sample_Type == "THP1") %>%
  filter(Hyb_Time == 4) %>%
  filter(Probe == "4A") %>% # All the Eukaryotic mRNA depleted samples are 4A, but different concentrations, want to keep probe type consistant with the non-eukaryotic mRNA depleted samples
  
  ggplot(aes(x = EukrRNADep, y = P_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Ra_cells), size = 2.5) + 
  scale_color_manual(values = cbPalette5[2]) + 
  scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  labs(title = "Eukaryotic rRNA depletion vs percent reads aligned to Mtb",
       subtitle = "4Hr hybridisation, all Probe 4A") + 
  
  my_plot_themes

DepVsPercent_scatter

ggsave(DepVsPercent_scatter,
       file = "EukrRNADepVsPercent_scatter.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")





  
  
  
  
  
  
  
  