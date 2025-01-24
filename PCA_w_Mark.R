# PCA plot - ProbeTest3 with Mark's data to compare
# 11/26/24

# Look into ggbiplot for more PCA stuff??
# https://cran.r-project.org/web/packages/ggbiplot/readme/README.html

source("Import_data.R") # to get combined_tpm


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
######################## MAKE PCA #########################

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

# Two options in base R, prcomp() and princomp()
# prcomp() is preferred according to the website above

# Think I need to transform the data first
my_tpm_t <- as.data.frame(t(combined_TPM))

# Remove columns that are all zero so the scale works for prcomp
my_tpm_t2 <- my_tpm_t %>% select_if(colSums(.) != 0)

# Make the actual PCA
my_PCA <- prcomp(my_tpm_t2, scale = TRUE)

# See the % Variance explained
summary(my_PCA)
summary_PCA <- format(round(as.data.frame(summary(my_PCA)[["importance"]]['Proportion of Variance',]) * 100, digits = 1), nsmall = 1) # Adding round and digits here to set number of digits after the decimal place.
summary_PCA[1,1] # PC1 explains 21.997% of variance
summary_PCA[2,1] # PC2 explains 12.119% of variance
summary_PCA[3,1] # PC3 explains 4.651% of variance


###########################################################
################ MAKE PCA PLOT with GGPLOT ################

my_PCA_df <- as.data.frame(my_PCA$x[, 1:3]) # Extract the first 3 PCs
my_PCA_df <- data.frame(SampleID = row.names(my_PCA_df), my_PCA_df)
my_PCA_df <- merge(my_PCA_df, combined_metadata, by = "SampleID")

fig_PC1vsPC2 <- my_PCA_df %>%
  ggplot(aes(x = PC1, y = PC2, color = Sample_Type, label = SampleID, label2 = Week, label3 = Batch)) + 
  geom_point(size = 3, alpha = 0.8) +
  # geom_text_repel(aes(label = SampleID), color = "black", size = 2) + 
  scale_color_manual(values = c(`Marmoset` = "#CAB2D6", `Sputum` = "#0072B2", `Saliva` = "#009E73", `THP1` = "#FF7F00", `rabbit` = "orange", `broth` = "black", `mimic` = "purple", `thp1` = "green")) + 
  labs(title = "PCA plot ProbeTest3: PC1 vs PC2. Including Mark's data",
       x = paste0("PC1: ", summary_PCA[1,1], "%"),
       y = paste0("PC2: ", summary_PCA[2,1], "%")) +
  my_plot_themes
fig_PC1vsPC2
ggplotly(fig_PC1vsPC2)

ggsave(fig_PC1vsPC2,
       file = "PCA_PC1vsPC2_AllPlusMark.pdf",
       path = "PCA_Figures",
       width = 9, height = 6, units = "in")


htmlwidgets::saveWidget(ggplotly(fig_PC1vsPC2), "PCA_Figures/PCA_PC1vsPC2_AllPlusMark.html")

###########################################################
################### MAKE 3D PCA PLOT ######################

# https://plotly.com/r/pca-visualization/

PCA_3D <- plot_ly(my_PCA_df, x = ~PC1, y = ~PC2, z = ~PC3,
                  type = "scatter3d", mode = "markers",
                  color = ~Sample_Type# , 
                  # colors = c("#CAB2D6", "#009E73", "#0072B2", "#FF7F00", "black", "purple", "orange", "green") # ,
                  # text = ~Replicate
)
PCA_3D
htmlwidgets::saveWidget(as_widget(PCA_3D), "PCA_Figures/PCA_PC1vsPC3_AllPlusMark_3D.html")

