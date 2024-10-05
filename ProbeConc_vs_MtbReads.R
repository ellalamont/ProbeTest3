# Compare probe concentration to Mtb reads (# or %)
# E. Lamont
# 10/2/24

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
################# SCATTER NUMBER READS ####################

ProbeConcVsReads_scatter <- my_pipeSummary %>% 
  filter(Sample_Type %in% c("Saliva", "THP1")) %>%
  filter(Hyb_Time == 4) %>%
  filter(EukmRNADep != "Yes") %>% 
  
  ggplot(aes(x = Probe_ng, y = N_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Ra_cells), size = 2.5) + 
  scale_color_manual(values = cbPalette5) + 
  scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe), size= 1.5, nudge_x = 3) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  labs(title = "Probe Concentration vs number reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion") + 
  
  my_plot_themes
  
ProbeConcVsReads_scatter

ggsave(ProbeConcVsReads_scatter,
       file = "ProbeConcVsReads_scatter.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")

###########################################################
################# SCATTER PERCENT READS ###################

ProbeConcVsPercent_scatter <- my_pipeSummary %>% 
  filter(Sample_Type %in% c("Saliva", "THP1")) %>%
  filter(Hyb_Time == 4) %>%
  filter(EukmRNADep != "Yes") %>% 
  
  ggplot(aes(x = Probe_ng, y = P_Genomic)) + 
  geom_point(aes(color = Sample_Type, shape = Ra_cells), size = 2.5) + 
  scale_color_manual(values = cbPalette5) + 
  scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe), size= 2) + 
  geom_text(aes(label = Probe), size= 1.5, nudge_x = 3) + 
  
  labs(title = "Probe Concentration vs % reads aligned to Mtb",
       subtitle = "4Hr hybridisation, no EukmRNA depletion") + 
  
  my_plot_themes

ProbeConcVsPercent_scatter

ggsave(ProbeConcVsPercent_scatter,
       file = "ProbeConcVsPercent_scatter.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")
