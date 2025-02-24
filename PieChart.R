# Make Pie chart of where reads are going 
# E. Lamont
# 1/20/25

# https://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
# https://r-graph-gallery.com/pie-plot.html
# https://ggplot2.tidyverse.org/reference/coord_polar.html



source("Import_data.R") # to get my_pipeSummary

# Plot basics
my_plot_themes <- theme_void() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none",legend.text=element_text(size=14),
        strip.text = element_text(size = 12, face = "bold"), # For the facet
        legend.title = element_text(size = 14),
        plot.title = element_text(size=10, hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(), # Remove facet panel borders
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_blank())


###########################################################
###################### REORDER DF #########################

my_pipeSummary_ReadCount <- my_pipeSummary %>% 
  mutate(N_NotMtb = (RawReads - N_RiboClear - N_Genomic)) %>% 
  pivot_longer(cols = c("N_RiboClear", "N_Genomic", "N_NotMtb"), names_to = "Number", values_to = "ReadCount") %>% 
  mutate(ReadPercent = round((ReadCount/RawReads)*100, 1)) 

replacement_values <- c(N_Genomic = "mRNA", N_RiboClear = "rRNA", N_NotMtb = "other RNA")
my_pipeSummary_ReadCount <- my_pipeSummary_ReadCount %>% 
  mutate(Number = replacement_values[Number])

###########################################################
###################### SINGLE PIE #########################

PieChart_1 <- my_pipeSummary_ReadCount %>% 
  filter(SampleID == "S_250754_Probe_4A_50_S24") %>% 
  arrange(desc(Number)) %>%  # Ensure consistent ordering of slices
  mutate(
    cumulative = cumsum(ReadPercent),       # Cumulative percentage
    midpoint = cumulative - ReadPercent / 2 # Midpoint for each slice
  ) %>%
  ggplot(aes(x=SampleID, y=ReadPercent, fill=Number)) +
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar(theta = "y", start = 0) + 
  scale_fill_manual(values = c("#00CED1", "#708090", "#E0D8B0")) + 
  geom_text(aes(y = midpoint, label = paste(Number, "\n", scales::percent(ReadPercent / 100))), size = 4, color = "black") +
  labs(title = "S_250754_Probe_4A_50_S24") + 
  my_plot_themes
PieChart_1
ggsave(PieChart_1,
       file = "S_250754_Probe_4A_50_S24_PIE.pdf",
       path = "Pie_Figures",
       width = 6, height = 4, units = "in")


###########################################################
##################### MULTIPLE PIE ########################

PieChart_2 <- my_pipeSummary_ReadCount %>% 
  filter(SampleID %in% c("THP1_1e6_8_Probe_4A_50_S38", "THP1_1e6_6_Probe_None_S36")) %>% 
  arrange(SampleID, desc(Number)) %>%  # Group-wise consistent ordering
  group_by(SampleID) %>%
  mutate(cumulative = cumsum(ReadPercent), midpoint = cumulative - ReadPercent / 2) %>%
  ungroup() %>%
  ggplot(aes(x = "", y = ReadPercent, fill = Number)) +
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar(theta = "y", start = 0) + 
  facet_wrap(~SampleID) +
  scale_fill_manual(values = c("#00CED1", "#708090", "#E0D8B0")) + 
  geom_text(aes(y = midpoint, label = paste(Number, "\n", scales::percent(ReadPercent / 100))), size = 4, color = "black") +
  my_plot_themes
PieChart_2
ggsave(PieChart_2,
       file = "THP1_1e6_CaptureVsNone_PIE.pdf",
       path = "Pie_Figures",
       width = 6, height = 4, units = "in")

PieChart_3 <- my_pipeSummary_ReadCount %>% 
  filter(SampleID %in% c("THP1_1e8_2_Probe_4A_10_16hr_S7", "THP1_1e8_6_Probe_None_S39")) %>%
  mutate(SampleID = factor(SampleID, levels = c("THP1_1e8_6_Probe_None_S39", "THP1_1e8_2_Probe_4A_10_16hr_S7"))) %>% 
  arrange(SampleID, desc(Number)) %>%  # Need this so the numbers go to the correct slices
  group_by(SampleID) %>% # Need this or it won't be a round pie! 
  mutate(cumulative = cumsum(ReadPercent), midpoint = cumulative - ReadPercent / 2) %>%
  ungroup() %>%
  ggplot(aes(x = "", y = ReadPercent, fill = Number)) +
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar(theta = "y", start = 0) + 
  facet_wrap(~SampleID, labeller = as_labeller(c("THP1_1e8_6_Probe_None_S39" = "Not captured", "THP1_1e8_2_Probe_4A_10_16hr_S7" = "Transcript captured"))) +
  scale_fill_manual(values = c("#00CED1", "#708090", "#E0D8B0")) + 
  # geom_text(aes(y = midpoint, label = paste(Number, "\n", scales::percent(ReadPercent / 100))), size = 4, color = "black") +
  geom_text_repel(aes(y = midpoint, label = paste(Number, "\n", scales::percent(ReadPercent / 100))), size = 4, color = "black", box.padding = 0.3, force = 2, force_pull = 2, min.segment.length = 0.2, segment.size = 0.5) + 
  labs(title = "THP1 cells spiked with H37Ra") + 
  my_plot_themes
PieChart_3
ggsave(PieChart_3,
       file = "THP1_1e8_CaptureVsNone_PIE.pdf",
       path = "Pie_Figures",
       width = 6, height = 4, units = "in")


