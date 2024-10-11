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
        plot.margin = margin(10, 10, 10, 20),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_blank()
  )


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
###### WEEK CHANGES - EVEN NICER GRAPHS w/ MARM ###########

# Stop scientific notation
# options(scipen = 999) 
options(scipen = 0) # To revert back to default

# For including the marmoset samples
sputum_w_marm <- my_pipeSummary %>% filter(Sample_Type %in% c("Sputum", "Marmoset"))
sputum_w_marm$Week <- as.character(sputum_w_marm$Week)
sputum_w_marm[c("Week")][1:3,1] <- "Marmoset"
ordered_Week2 <- c("0", "2", "4", "Marmoset")
sputum_w_marm$Week <- factor(sputum_w_marm$Week, levels = ordered_Week2)
ordered_Sample_Type <- c("Sputum", "Marmoset")
sputum_w_marm$Sample_Type <- factor(sputum_w_marm$Sample_Type, levels = ordered_Sample_Type)

## Number reads 
WeekVsReads_sputum3 <- sputum_w_marm %>% 
  
  ggplot(aes(x = Week, y = N_Genomic)) + 
  geom_point(aes(fill = Sample_Type,), shape = 21, size = 6, alpha = 0.8, stroke = 0.8) + 
  scale_fill_manual("Sample type", values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  # scale_shape_manual(values=c(`0` = 15, `2` = 0, `4` = 3)) + 
  
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 3, box.padding = 0.4, segment.color = NA) + 
  # geom_text(aes(label = Probe_ng), size= 1.5, nudge_x = 0.07) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  
  facet_grid(~ Sample_Type, scales = "free", space = "free") + 
  
  scale_y_continuous(limits = c(0,6500000), breaks = seq(0, 6500000, 1000000)) +
  
  labs(title = "Sputum: Week vs number reads aligned to Mtb",
       subtitle = "Label is percent of reads aligned to Mtb", 
       x = "Weeks after start of antibiotics", 
       y = "# reads aligning to Mtb genome") + 
  
  my_plot_themes

WeekVsReads_sputum3

ggsave(WeekVsReads_sputum3,
       file = "WeekVsReads_sputum_w_marm3.pdf",
       path = "Figures/Sputum",
       width = 8.5, height = 5, units = "in")

ggsave(WeekVsReads_sputum3,
       file = "WeekVsReads_sputum_w_marm3_Thumbnail.pdf",
       path = "Figures/Sputum",
       width = 5, height = 3, units = "in")



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


###########################################################
################# P_GENOMIC VS G_GENOMIC ##################

# Stop scientific notation
options(scipen = 999) 
options(scipen = 0) # To revert back to default

set.seed(23)
Scatter_SputumReads <- my_sputum %>% 
  ggplot(aes(x = P_Genomic, y = N_Genomic)) +
  geom_point(aes(shape = Week), alpha = 0.8, stroke = 0.8, size = 6, fill = "#0072B2") +
  scale_shape_manual("Sputum \ncollection week", values = c(21,22,24)) +
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 2.5, box.padding = 0.4) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  # scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 1000000)) +
  # scale_x_continuous(limits = c(0,55), breaks = seq(0, 55, 10)) + 
  
  scale_y_continuous(limits = c(0,4000000), breaks = seq(0, 4000000, 1000000)) +
  scale_x_continuous(limits = c(0,55), breaks = seq(0, 55, 10)) + 
  
  labs(title = "Sputum Number reads aligning to Mtb vs % Mtb reads",
       subtitle = NULL,
       x = "% of reads aligned to Mtb", y = "Number of reads aligned to Mtb") + 
  my_plot_themes
Scatter_SputumReads

ggsave(Scatter_SputumReads,
       file = "Scatter_SputumReads.pdf",
       path = "Figures/Sputum",
       width = 6, height = 4, units = "in")




###########################################################
################ SCATTER SPUTUM WITH MARM #################

sputum_w_marm <- my_pipeSummary %>% filter(Sample_Type %in% c("Sputum", "Marmoset"))
sputum_w_marm$Week <- as.character(sputum_w_marm$Week)
sputum_w_marm[c("Week")][1:3,1] <- "Marm"
ordered_Week2 <- c("0", "2", "4", "Marm")
sputum_w_marm$Week <- factor(sputum_w_marm$Week, levels = ordered_Week2)

set.seed(23)
Scatter_Sputum_w_Marm_Reads <- sputum_w_marm %>% 
  ggplot(aes(x = P_Genomic, y = N_Genomic)) +
  geom_point(aes(fill = Sample_Type, shape = Week), alpha = 0.8, stroke = 0.8, size = 6) + 
  scale_fill_manual("Sample Type", values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00")) +  
  scale_shape_manual(values = c(21,22,24,23)) +
  guides(fill = guide_legend(override.aes = list(shape=c(23, 21)))) + 
  
  geom_text_repel(aes(label = format(N_Genomic, big.mark = ",")), size= 3, box.padding = 0.4, segment.color = NA) + 
  
  geom_hline(yintercept = 1000000, linetype = "dashed", alpha = 0.5) + 
  scale_y_continuous(limits = c(0,6500000), breaks = seq(0, 6500000, 1000000)) +
  scale_x_continuous(limits = c(0,80), breaks = seq(0, 80, 10)) + 
  
  labs(title = "Sputum + Marm Number reads aligning to Mtb vs % Mtb reads",
       subtitle = NULL,
       x = "% of reads aligned to Mtb", y = "Number of reads aligned to Mtb") + 
  my_plot_themes
Scatter_Sputum_w_Marm_Reads

ggsave(Scatter_Sputum_w_Marm_Reads,
       file = "Scatter_Sputum_w_Marm_Reads.pdf",
       path = "Figures/Sputum",
       width = 8.5, height = 5, units = "in")

ggsave(Scatter_Sputum_w_Marm_Reads,
       file = "Scatter_Sputum_w_Marm_Reads_Thumbnail.pdf",
       path = "Figures/Sputum",
       width = 5, height = 3, units = "in")


