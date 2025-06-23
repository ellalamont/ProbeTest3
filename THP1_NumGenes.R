# Looking at the THP1 samples, how many genes have at least 10 or 100 reads
# E. Lamont
# 1/24/25

source("Import_data.R") # to get my_pipeSummary

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

# Stop scientific notation
# options(scipen = 999) 
options(scipen = 0) # To revert back to default

###########################################################
################### ALL CAPTURED THP1 #####################

my_THP1 <- my_pipeSummary %>% filter(Sample_Type == "THP1")
my_THP1 <- my_THP1 %>% 
  mutate(Probe_w_ng = paste0(Probe, ", ", Probe_ng, "ng"))
orderd_Probe <- c("None", "1", "3A", "3D", "4A")
my_THP1$Probe <- factor(my_THP1$Probe, levels = orderd_Probe)
my_THP1$Probe_ng_chr <- as.character(my_THP1$Probe_ng)
orderd_Probe_ng_chr <- c("0", "10", "15.4", "16.4", "25", "50", "100")
my_THP1$Probe_ng_chr <- factor(my_THP1$Probe_ng_chr, levels = orderd_Probe_ng_chr)


###########################################################
############## COMPARE NUMBER OF GENES (10) ###############

THP1_10Genes <- my_THP1 %>% 
  ggplot(aes(x = Probe, y = AtLeast.10.Reads, text = SampleID)) + 
  geom_point(aes(shape = Ra_cells, fill = Probe_ng_chr), alpha = 0.8, stroke = 0.8, size = 6) +
  scale_shape_manual("H37Ra cells \nspiked", values=c(24, 21), labels = c("1e6", "1e8")) +
  scale_fill_manual("ng of probe", values = c7) + 
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21, 21, 21, 21)))) + 
  # geom_text_repel(aes(label = format(AtLeast.10.Reads, big.mark = ",")), size= 2.5, box.padding = 0.4, segment.color = NA, max.overlaps = Inf) + 
  scale_y_continuous(limits = c(0,4499), breaks = seq(0, 4500, 500)) + 
  scale_x_discrete(labels=c("None" = "Not captured", "1" = "Probe A", "3A" = "Probe B", "3D" = "Probe C", "4A" = "Probe D")) +
  labs(title = "Spiked THP1 # genes aligned to Mtb with at least 10 TPM",
       subtitle = NULL,
       x = "Probe prep", 
       y = "# genes with at least 10 reads") + 
  geom_hline(yintercept = 4499/2, linetype = "dashed", alpha = 0.5) + 
  my_plot_themes
THP1_10Genes
ggsave(THP1_10Genes,
       file = "Scatter_THP1_Num10Genes_v1.pdf",
       path = "Figures",
       width = 8, height = 5, units = "in")


###########################################################
############## COMPARE NUMBER OF GENES (100) ###############

THP1_100Genes <- my_THP1 %>% 
  ggplot(aes(x = Probe, y = AtLeast.100.Reads, text = SampleID)) + 
  geom_point(aes(shape = Ra_cells, fill = Probe_ng_chr), alpha = 0.8, stroke = 0.8, size = 6) +
  scale_shape_manual("H37Ra cells \nspiked", values=c(24, 21), labels = c("1e6", "1e8")) +
  scale_fill_manual("ng of probe", values = c7) + 
  guides(fill = guide_legend(override.aes = list(shape = c(21, 21, 21, 21, 21, 21, 21)))) + 
  # geom_text_repel(aes(label = format(AtLeast.10.Reads, big.mark = ",")), size= 2.5, box.padding = 0.4, segment.color = NA, max.overlaps = Inf) + 
  scale_y_continuous(limits = c(0,4499), breaks = seq(0, 4500, 500)) + 
  scale_x_discrete(labels=c("None" = "Not captured", "1" = "Probe A", "3A" = "Probe B", "3D" = "Probe C", "4A" = "Probe D")) +
  labs(title = "Spiked THP1 # genes aligned to Mtb with at least 100 transcripts",
       subtitle = NULL,
       x = "Probe prep", 
       y = "# genes with at least 100 reads") + 
  geom_hline(yintercept = 4499/2, linetype = "dashed", alpha = 0.5) + 
  my_plot_themes
THP1_100Genes
ggsave(THP1_100Genes,
       file = "Scatter_THP1_Num100Genes_v1.pdf",
       path = "Figures",
       width = 8, height = 5, units = "in")

