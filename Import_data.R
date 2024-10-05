# Probe Prep testing 3
# August probes with test samples, Sputum, and Mamorset samples
# 10/2/24


################################################
################ LOAD PACKAGES #################

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(knitr)
library(plotly)
library(ggprism) # for add_pvalue()
library(rstatix) # for adjust_pvalue
library(ggpmisc) # https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
library(ggrepel)
library(pheatmap)
library(ggplotify) # To convert pheatmaps to ggplots
library(corrplot)
library(ggcorrplot)
library(ggfortify) # To make pca plots with plotly



cbPalette_1 <- c("#999999", "#E69F00") # Gold and Grey
cbPalette_1.5 <- c("#E69F00", "#999999") # Gold and Grey
cbPalette_2 <- c( "#0072B2", "#999999") # Blue and Grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette2 <-  c("#bfbfbf", "#56B4E9")
cbPalette3 <-  c("#bfbfbf", "#E69F00")
cbPalette4 <- c("#56B4E9", "#009E73", "#F0E442")
cbPalette5 <- c("#009E73", "#FF7F00")
c25 <- c(
  "dodgerblue2", "#E31A1C", "green4",
  "#6A3D9A","#FF7F00","black", "gold1",
  "skyblue2", "#FB9A99","palegreen2","#CAB2D6",
  "#FDBF6F","gray70", "khaki2","maroon", "orchid1", "deeppink1", "blue1", "steelblue4","darkturquoise", "green1", "yellow4", "yellow3","darkorange4", "brown"
)
c12 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "palegreen2", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4") 
c16 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black","gold1", "#FB9A99", "#CAB2D6", "palegreen2", "gray70", "maroon", "orchid1", "blue1", "darkturquoise", "darkorange4") 


# Stop scientific notation
options(scipen = 999) 
# options(scipen = 0) # To revert back to default

###########################################################
############### IMPORT PIPELINE SUMMARY DATA ##############

my_pipeSummary <- read.csv("Pipeline.Summary.Details.csv")
# This has been edited to include more metadata

my_pipeSummary$Hyb_Time <- as.character(my_pipeSummary$Hyb_Time)
ordered_Hyb_Time <- c("4", "16")
my_pipeSummary$Hyb_Time <- factor(my_pipeSummary$Hyb_Time, levels = ordered_Hyb_Time)

my_pipeSummary$Week <- as.character(my_pipeSummary$Week)
ordered_Week <- c("0", "2", "4")
my_pipeSummary$Week <- factor(my_pipeSummary$Week, levels = ordered_Week)



# 10/4/24: STARTED BELOW

###########################################################
############ IMPORT AND PROCESS ALL TPM VALUES ############

my_tpm <- read.csv("Mtb.Expression.Gene.Data.SCALED.TPM.csv")

my_tpm <- my_tpm[,-ncol(my_tpm)] # remove the last column which is the Undetermined

# Adjust the names so they are slightly shorter
# names(my_tpm) <- gsub(x = names(my_tpm), pattern = "_2_2_ng_mL", replacement = "")
names(my_tpm) <- gsub(x = names(my_tpm), pattern = "_S.*", replacement = "") # This regular expression removes the _S and everything after it (I think...)

# Grab the metadata I added to my_pipeSummary
my_metadata <- my_pipeSummary %>% select(SampleID, Sample_Type, Ra_cells, EukrRNADep, Week, ct, ttd, Hyb_Time, Hyb_Group, Probe, Probe_ng)

# Adjust the metadata names so they are the same
my_metadata$SampleID <- sub(x = my_metadata$SampleID, pattern = "_S.*", replacement = "")

# add rownames to the tpm and metadata dataframes
rownames(my_tpm) <- my_tpm[,1] # add the rownames
my_tpm <- my_tpm[,-1] # Remove the old column of rownames
rownames(my_metadata) <- my_metadata[,1] # add the rownames
# my_metadata <- my_metadata[,-1] # Remove the old column of rownames

