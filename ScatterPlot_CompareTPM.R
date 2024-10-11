# Scatter plots to compare TPM between the same samples sequenced differently 

# E. Lamont
# 10/10/24

# I don't think I can do this for the THP1 samples withouth probe because there just aren't enough reads. Would have to sequence deeper for it to work. I think Mark sequenced deeper when he did these graphs

source("Import_data.R") # to get my_tpm
my_tpm$Gene <- rownames(my_tpm)

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

# To add a regression to a graph
my_regression_line <- stat_poly_line(method = "lm", se = TRUE, level = 0.95)
my_regression_equations <- stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                                          after_stat(rr.label), 
                                                          after_stat(p.value.label),
                                                          sep = "*\", \"*")),
                                        label.x = "right")


###########################################################
################ SALIVA CAPTURED VS NONE ##################

# Compare Saliva_1e6_6_Probe_None to Saliva_1e6_1_Probe_3D_25

Scatter_SalivaVsNone <- my_tpm %>% 
  ggplot(aes(x = Saliva_1e6_6_Probe_None, y = Saliva_1e6_1_Probe_3D_25)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "Saliva 1e6 captured vs not captured TPM",
       subtitle = "Captured sample with probe 3D @ 25ng",
       x = "Not captured TPM", y = "Captured TPM") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_SalivaVsNone
ggplotly(Scatter_SalivaVsNone)

# Scatter_SalivaVsNone + my_regression_line + my_regression_equations

ggsave(Scatter_SalivaVsNone,
       file = "Scatter_SalivaVsNone.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")


###########################################################
################ THP1 COMPARE PROBE CONC ##################

# Compare THP1_1e6_1_Probe_3D_100 AND THP1_1e6_3_Probe_3D_25
Scatter_THP1_3D_100vs25 <- my_tpm %>% 
  ggplot(aes(x = THP1_1e6_1_Probe_3D_100, y = THP1_1e6_1_Probe_3D_100)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e6 cells H37Ra",
       subtitle = NULL,
       x = "TPM (Probe 3D @ 100ng)", y = "TPM (Probe 3D @ 25ng)") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_3D_100vs25
ggplotly(Scatter_THP1_3D_100vs25)

ggsave(Scatter_THP1_3D_100vs25,
       file = "Scatter_THP1_3D_100vs25.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")

# Compare THP1_1e6_4_Probe_3D_10 AND THP1_1e6_3_Probe_3D_25
Scatter_THP1_3D_10vs25 <- my_tpm %>% 
  ggplot(aes(x = THP1_1e6_4_Probe_3D_10, y = THP1_1e6_1_Probe_3D_100)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e6 cells H37Ra",
       subtitle = NULL,
       x = "TPM (Probe 3D @ 10ng)", y = "TPM (Probe 3D @ 25ng)") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_3D_10vs25
ggplotly(Scatter_THP1_3D_10vs25)

ggsave(Scatter_THP1_3D_10vs25,
       file = "Scatter_THP1_3D_10vs25.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")

# Compare THP1_1e6_2_Probe_3D_50 AND THP1_1e6_3_Probe_3D_25
Scatter_THP1_3D_50vs25 <- my_tpm %>% 
  ggplot(aes(x = THP1_1e6_2_Probe_3D_50, y = THP1_1e6_1_Probe_3D_100)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e6 cells H37Ra",
       subtitle = NULL,
       x = "TPM (Probe 3D @ 50ng)", y = "TPM (Probe 3D @ 25ng)") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_3D_50vs25
ggplotly(Scatter_THP1_3D_50vs25)

ggsave(Scatter_THP1_3D_50vs25,
       file = "Scatter_THP1_3D_50vs25.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")


###########################################################
################ THP1 COMPARE PROBE TYPE ##################

# Compare THP1_1e6_2_Probe_3D_50 AND THP1_1e6_8_Probe_4A_50
Scatter_THP1_50ng_Probe3Dvs4A <- my_tpm %>% 
  ggplot(aes(x = THP1_1e6_2_Probe_3D_50, y = THP1_1e6_8_Probe_4A_50)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e6 cells H37Ra",
       subtitle = NULL,
       x = "TPM (Probe 3D @ 50ng)", y = "TPM (Probe 4A @ 50ng)") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_50ng_Probe3Dvs4A
# ggplotly(Scatter_THP1_50ng_Probe3Dvs4A)

ggsave(Scatter_THP1_50ng_Probe3Dvs4A,
       file = "Scatter_THP1_50ng_Probe3Dvs4A.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")


###########################################################
############# THP1 COMPARE EukrRNA DEPLETION ##############

# Compare THP1_1e6_EukmRNADep_1_Probe_4A_50 AND THP1_1e6_8_Probe_4A_50
Scatter_THP1_1e6_4A.50ng_EukrRNADepVsNot <- my_tpm %>% 
  ggplot(aes(x = THP1_1e6_EukmRNADep_1_Probe_4A_50, y = THP1_1e6_8_Probe_4A_50)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e6 cells H37Ra",
       subtitle = "THP1_1e6_EukmRNADep_1_Probe_4A_50 vs THP1_1e6_8_Probe_4A_50",
       x = "TPM (with Eukaryotic rRNA depletion)", y = "TPM (no additional depletion)") + 
  
  scale_y_continuous(limits = c(0,20000), breaks = seq(0, 20000, 5000)) +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 5000)) +
  
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_1e6_4A.50ng_EukrRNADepVsNot
ggplotly(Scatter_THP1_1e6_4A.50ng_EukrRNADepVsNot)

ggsave(Scatter_THP1_1e6_4A.50ng_EukrRNADepVsNot,
       file = "Scatter_THP1_1e6_4A.50ng_EukrRNADepVsNot.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")

# Compare THP1_1e8_EukmRNADep_1_Probe_4A_50 AND THP1_1e8_8_Probe_4A_50
Scatter_THP1_1e8_4A.50ng_EukrRNADepVsNot <- my_tpm %>% 
  ggplot(aes(x = THP1_1e8_EukmRNADep_1_Probe_4A_50, y = THP1_1e8_8_Probe_4A_50)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e8 cells H37Ra",
       subtitle = "THP1_1e8_EukmRNADep_1_Probe_4A_50 vs THP1_1e8_8_Probe_4A_50",
       x = "TPM (with Eukaryotic rRNA depletion)", y = "TPM (no additional depletion)") + 
  
  scale_y_continuous(limits = c(0,20000), breaks = seq(0, 20000, 5000)) +
  scale_x_continuous(limits = c(0,20000), breaks = seq(0, 20000, 5000)) +

  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_1e8_4A.50ng_EukrRNADepVsNot
ggplotly(Scatter_THP1_1e8_4A.50ng_EukrRNADepVsNot)

ggsave(Scatter_THP1_1e8_4A.50ng_EukrRNADepVsNot,
       file = "Scatter_THP1_1e8_4A.50ng_EukrRNADepVsNot.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")


###########################################################
############ THP1 COMPARE HYBRIDIZATION TIME ##############

# Compare THP1_1e8_1_Probe_4A_25_16hr AND THP1_1e8_4_Probe_4A_25
Scatter_THP1_4A.25ng_16hrVs4hr <- my_tpm %>% 
  ggplot(aes(x = THP1_1e8_1_Probe_4A_25_16hr, y = THP1_1e8_4_Probe_4A_25)) + 
  geom_point(aes(text = Gene), alpha = 1, size = 2, color = "black") +
  labs(title = "THP1 with 1e8 cells H37Ra",
       subtitle = "THP1_1e8_1_Probe_4A_25_16hr vs THP1_1e8_4_Probe_4A_25",
       x = "TPM (16 hr hybridization)", y = "TPM (4 hr hybridization)") + 
  stat_cor(method="pearson") + # add a correlation to the plot
  my_plot_themes
Scatter_THP1_4A.25ng_16hrVs4hr
# ggplotly(Scatter_THP1_4A.25ng_16hrVs4hr)

ggsave(Scatter_THP1_4A.25ng_16hrVs4hr,
       file = "Scatter_THP1_4A.25ng_16hrVs4hr.pdf",
       path = "ScatterPlots_TPM",
       width = 6, height = 4, units = "in")







