# Make a correlation plot
# 10/5/24


source("Import_data.R") # to get my_tpm


###########################################################
#################### MAKE CORRELATION #####################

# Make the correlation using stats package
my_cor_spearman <- cor(my_tpm, method = "spearman")
my_cor_pearson <- cor(my_tpm, method = "pearson")

# Get the p-values using ggcorplot package
my_pvalue_spearman <- cor_pmat(my_tpm, method = "spearman")
my_pvalue_pearson <- cor_pmat(my_tpm, method = "pearson")
# They're all zeros.... not sure if this is true or not?


###########################################################
################### CORRPLOT FUNCTION #####################

# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

corrplot(my_cor_spearman, method = "square", order = "AOE",
         addCoef.col = "black", number.font = 0.01,
         tl.cex = 0.5, tl.col = "black" # Change variable name parameters
         )

# This method not so good because I can't figure out how to scale the colors!

###########################################################
################ GGCORRPLOT ALL TOGETHER ##################

# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

min(my_cor_spearman) # 0.1021687
min(my_cor_pearson) # 0.02287277
# So I know what my lower limit should be

# Plot basics
my_plot_themes <- theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right",legend.text=element_text(size=10),
        legend.title = element_text(size = 10),
        plot.title = element_text(size=10), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, size=10, vjust=0, hjust=0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=10), 
        plot.subtitle = element_text(size=10), 
        plot.margin = margin(10, 10, 10, 20))

# Plot spearman
spearman_all_plot <- ggcorrplot(my_cor_spearman, hc.order = FALSE, 
           lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.1,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Spearman Correlation all", fill = "Correlation")
spearman_all_plot

ggsave(spearman_all_plot,
       file = "Spearman_CorrelationPlot_all.pdf",
       path = "Correlation_Figures",
       width = 12, height = 12, units = "in")

# Plot pearson
pearson_all_plot <- ggcorrplot(my_cor_pearson, hc.order = FALSE, 
                                lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.02,1), low = "blue", high =  "red", mid = "white", midpoint = 0.50) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Pearson Correlation all", fill = "Correlation")
pearson_all_plot

ggsave(pearson_all_plot,
       file = "Pearson_CorrelationPlot_all.pdf",
       path = "Correlation_Figures",
       width = 12, height = 12, units = "in")


###########################################################
################# GGCORRPLOT SALIVA ONLY ##################

my_tpm_Saliva <- my_tpm %>% select(contains("Saliva"))
my_cor_spearman_Saliva <- cor(my_tpm_Saliva, method = "spearman")
my_cor_pearson_Saliva <- cor(my_tpm_Saliva, method = "pearson")

min(my_cor_spearman_Saliva) # 0.8741
min(my_cor_pearson_Saliva) # 0.7710974

# Plot spearman
spearman_plot_Saliva <- my_cor_spearman_Saliva %>% 
  ggcorrplot(hc.order = FALSE, 
                                lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.85,1), low = "blue", high =  "red", mid = "white", midpoint = 0.93) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = "Saliva Spearman Correlation", fill = "Correlation")
spearman_plot_Saliva

ggsave(spearman_plot_Saliva,
       file = "Spearman_CorrelationPlot_Saliva.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")

# Plot pearson
pearson_plot_Saliva <- my_cor_pearson_Saliva %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.75,1), low = "blue", high =  "red", mid = "white", midpoint = 0.875) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Saliva Pearson Correlation", fill = "Correlation")
pearson_plot_Saliva

ggsave(pearson_plot_Saliva,
       file = "Pearson_CorrelationPlot_Saliva.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")

###########################################################
################## GGCORRPLOT THP1 ONLY ###################

my_tpm_THP1 <- my_tpm %>% select(contains("THP1"))
my_cor_spearman_THP1 <- cor(my_tpm_THP1, method = "spearman")
my_cor_pearson_THP1 <- cor(my_tpm_THP1, method = "pearson")

min(my_cor_spearman_THP1) # 0.3991988
min(my_cor_pearson_THP1) # 0.6791915

# Plot spearman
spearman_plot_THP1 <- my_cor_spearman_THP1 %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.35,1), low = "blue", high =  "red", mid = "white", midpoint = 0.675) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "THP1 Spearman Correlation", fill = "Correlation")
spearman_plot_THP1

ggsave(spearman_plot_THP1,
       file = "Spearman_CorrelationPlot_THP1.pdf",
       path = "Correlation_Figures",
       width = 8, height = 8, units = "in")

# Plot pearson
pearson_plot_THP1 <- my_cor_pearson_THP1 %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.67,1), low = "blue", high =  "red", mid = "white", midpoint = 0.835) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "THP1 Pearson Correlation", fill = "Correlation")
pearson_plot_THP1

ggsave(pearson_plot_THP1,
       file = "Pearson_CorrelationPlot_THP1.pdf",
       path = "Correlation_Figures",
       width = 8, height = 8, units = "in")

###########################################################
################# GGCORRPLOT SPUTUM ONLY ##################

my_tpm_Sputum <- my_tpm %>% select(contains("S_"))
my_cor_spearman_Sputum <- cor(my_tpm_Sputum, method = "spearman")
my_cor_pearson_Sputum <- cor(my_tpm_Sputum, method = "pearson")

min(my_cor_spearman_Sputum) # 0.1021687
min(my_cor_pearson_Sputum) # 0.03714109

# Plot spearman
spearman_plot_Sputum <- my_cor_spearman_Sputum %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.1,1), low = "blue", high =  "red", mid = "white", midpoint = 0.55) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Sputum Spearman Correlation", fill = "Correlation")
spearman_plot_Sputum

ggsave(spearman_plot_Sputum,
       file = "Spearman_CorrelationPlot_Sputum.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")

# Plot pearson
pearson_plot_Sputum <- my_cor_pearson_Sputum %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5) + 
  scale_fill_gradient2(limit = c(0.03,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Sputum Pearson Correlation", fill = "Correlation")
pearson_plot_Sputum

ggsave(pearson_plot_Sputum,
       file = "Pearson_CorrelationPlot_Sputum.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")



# Include significance... is it working? there are warnings...
my_pvalue_spearman_Sputum <- cor_pmat(my_tpm_Sputum, method = "spearman")
spearman_plot_Sputum_Sig <- my_cor_spearman_Sputum %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, p.mat = my_pvalue_spearman_Sputum, insig = "blank") + 
  scale_fill_gradient2(limit = c(0.1,1), low = "blue", high =  "red", mid = "white", midpoint = 0.55) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "Sputum Spearman Correlation", fill = "Correlation")
spearman_plot_Sputum_Sig


