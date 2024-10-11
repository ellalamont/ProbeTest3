# Scatter plot comparing P_Genomic and N_Genomic for the THP1 cells
# E. Lamont
# 10/11/24

source("Import_data.R") # to get my_pipeSummary

my_THP1 <- my_pipeSummary %>% filter(Sample_Type == "THP1")
my_THP1 <- my_THP1 %>% 
  mutate(Probe_w_ng = paste0(Probe, ", ", Probe_ng, "ng"))
orderd_Probe <- c("None", "1", "3A", "3D", "4A")
my_THP1$Probe <- factor(my_THP1$Probe, levels = orderd_Probe)
my_THP1$Probe_ng_chr <- as.character(my_THP1$Probe_ng)
orderd_Probe_ng_chr <- c("0", "10", "15.4", "16.4", "25", "50", "100")
my_THP1$Probe_ng_chr <- factor(my_THP1$Probe_ng_chr, levels = orderd_Probe_ng_chr)

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
        plot.margin = margin(10, 10, 10, 20),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_blank()
  )

###########################################################
################# P_GENOMIC VS N_GENOMIC ##################

# Stop scientific notation
options(scipen = 999) 
options(scipen = 0) # To revert back to default

Scatter_THP1Reads <- my_THP1 %>% 
  ggplot(aes(x = P_Genomic, y = N_Genomic)) +
  geom_point(aes(shape = Ra_cells, fill = Probe_w_ng), alpha = 0.8, stroke = 0.8, size = 6) +
  scale_shape_manual("H37Ra cells \nspiked", values=c(24, 21), labels = c("1e6", "1e8")) +
  scale_fill_manual("Probe prep, \nconcentration", values = c11) + 
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21)))) + 
  # geom_text_repel(aes(label = format(EukrRNADep, big.mark = ",")), size= 2.5, box.padding = 0.4) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,9000000), breaks = seq(0, 9000000, 1000000)) +
  scale_x_continuous(limits = c(0,70), breaks = seq(0, 70, 10)) + 
  
  labs(title = "THP1 Number reads aligning to Mtb vs % Mtb reads",
       subtitle = NULL,
       x = "% of reads aligned to Mtb", y = "Number of reads aligned to Mtb") + 
  my_plot_themes
Scatter_THP1Reads


ggsave(Scatter_THP1Reads,
       file = "Scatter_THP1Reads.pdf",
       path = "Figures",
       width = 8, height = 5, units = "in")

###########################################################
################ N_GENOMIC VS PROBE PREP ##################

Scatter_THP1Reads2 <- my_THP1 %>% 
  ggplot(aes(x = Probe, y = N_Genomic)) +
  geom_point(aes(shape = Ra_cells, fill = Probe_ng_chr), alpha = 0.8, stroke = 0.8, size = 6) +
  scale_shape_manual("H37Ra cells \nspiked", values=c(24, 21), labels = c("1e6", "1e8")) +
  scale_fill_manual("ng of probe", values = c7) + 
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21, 21, 21, 21)))) + 
  # geom_text_repel(aes(label = format(EukrRNADep, big.mark = ",")), size= 2.5, box.padding = 0.4) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,9000000), breaks = seq(0, 9000000, 1000000)) +
  scale_x_discrete(labels=c("None" = "Not captured", "1" = "Probe A", "3A" = "Probe B", "3D" = "Probe C", "4A" = "Probe D")) +
  
  labs(title = "THP1 Number reads aligning to Mtb vs % Mtb reads",
       subtitle = NULL,
       x = "Probe Prep", 
       y = "Number of reads aligned to Mtb") + 
  my_plot_themes
Scatter_THP1Reads2


ggsave(Scatter_THP1Reads2,
       file = "Scatter_THP1Reads2.pdf",
       path = "Figures",
       width = 8, height = 5, units = "in")







