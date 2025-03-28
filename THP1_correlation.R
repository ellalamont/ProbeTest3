# Correlation plot comparing all the THP1 samples together
# E. Lamont
# 1/25/25

source("Import_data.R") # to get my_tpm

my_tpm$Gene <- rownames(my_tpm)

my_THP1_tpm <- my_tpm[, grepl("THP1", colnames(my_tpm))]

# Log10 transform the data
my_THP1_tpm_Log10 <- my_THP1_tpm %>% 
  mutate(across(where(is.numeric), ~ .x + 1)) %>% # Add 1 to all the values
  mutate(across(where(is.numeric), ~ log10(.x))) # Log transform the values


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


###########################################################
##################### LOG10 GGCORRPLOT ####################
# Only going to do Pearson because the number of genes is so high CLT applies and parametric tests can be used

# remove the Euk dep samples
my_THP1_tpm_Log10_2 <- my_THP1_tpm_Log10[, !grepl("Euk", colnames(my_THP1_tpm_Log10))]

# Make the correlation
my_cor_pearson <- cor(my_THP1_tpm_Log10_2, method = "pearson")

min(my_cor_pearson) # 0.3743603

# Plot pearson
Corr_plot <- my_cor_pearson %>% 
  ggcorrplot(hc.order = FALSE, 
             lab = TRUE, lab_size = 1.5,
             type = c("full")) + 
  scale_fill_gradient2(limit = c(0.37,1), low = "blue", high =  "red", mid = "white", midpoint = 0.68) + # Make sure to change based on the min!
  my_plot_themes + 
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  labs(title = "THP1 Pearson Correlation Log10 transformed", 
       subtitle = NULL, 
       fill = "Correlation")
Corr_plot

ggsave(Corr_plot,
       file = "AllrRNATHP1_PearsonLog10.pdf",
       path = "Correlation_Figures",
       width = 6, height = 6, units = "in")





