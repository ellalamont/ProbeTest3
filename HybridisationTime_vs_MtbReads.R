# Compare hybridization time to Mtb reads (# or %)
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
################### PAIRED NUMBER READS ###################

HybTimeVsReads_paired <- my_pipeSummary %>% 
  filter(X %in% c(18,19, 34,35, 21,22, 37,38)) %>% # Just easier to pull out the 16hr hyb and their matching 4hr ones
  
  ggplot(aes(x = Hyb_Time, y = N_Genomic, group = Hyb_Group)) + 
  geom_line(linewidth = 0.2) + 
  
  geom_point(aes(color = Sample_Type), shape = 16, size = 2.5) + 
  scale_color_manual(values = cbPalette5) + 
  
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  labs(title = "Hybridisation Time vs number reads aligned to Mtb",
       subtitle = "no EukmRNA depletion") + 
  
  my_plot_themes

HybTimeVsReads_paired

ggsave(HybTimeVsReads_paired,
       file = "HybTimeVsReads_paired.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")


###########################################################
################## PAIRED PERCENT READS ###################

HybTimeVsPercent_paired <- my_pipeSummary %>% 
  filter(X %in% c(18,19, 34,35, 21,22, 37,38)) %>% # Just easier to pull out the 16hr hyb and their matching 4hr ones
  
  ggplot(aes(x = Hyb_Time, y = P_Genomic, group = Hyb_Group)) + 
  geom_line(linewidth = 0.2) + 
  
  geom_point(aes(color = Sample_Type), shape = 16, size = 2.5) + 
  scale_color_manual(values = cbPalette5) + 
  
  geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  scale_y_continuous(limits = c(10,100), breaks = seq(10, 100, 10)) + 
  
  labs(title = "Hybridisation Time vs number reads aligned to Mtb",
       subtitle = "no EukmRNA depletion") + 
  
  my_plot_themes

HybTimeVsPercent_paired

ggsave(HybTimeVsPercent_paired,
       file = "HybTimeVsPercent_paired.pdf",
       path = "Figures",
       width = 6, height = 4, units = "in")











# GGPAIRED BELOW... NOT WORKING AS WELL

###########################################################
################## GGPAIRED NUMBER READS ##################

HybTimeVsReads_ggpaired <- my_pipeSummary %>% 
  filter(Sample_Type == "THP1") %>%
  filter(X %in% c(18,19, 34,35, 21,22, 37, 38)) %>% # Just easier to pull out the 16hr hyb and their matching 4hr ones
  
  ggpaired(x = "Hyb_Time", y = "N_Genomic",
           line.size = 0.4) + 
  geom_point(aes(color = Sample_Type, shape = Ra_cells), size = 2.5) + 
  scale_color_manual(values = cbPalette5) + 
  scale_shape_manual(values=c(1, 16)) +
  
  # geom_text_repel(aes(label = Probe_ng), size= 2) + 
  
  labs(title = "Hybridisation Time vs number reads aligned to Mtb",
       subtitle = "no EukmRNA depletion") + 
  
  my_plot_themes

HybTimeVsReads_ggpaired



