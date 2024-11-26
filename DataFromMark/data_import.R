library(tidyverse)
library(readxl)
theme_Publication <- function(base_size=18, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="bold"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}

MtbReads <- read_csv('./Data/Updated.Mtb.Expression.Gene.Data.ReadsM.csv')
SampleInfo <- read_csv('./Data/Annotation_including_new_timecourse_batch.txt')
RabbitInfo <- read_excel('./Data/master_rabbit_data.xlsx')

##Add in lesion density metric
RabbitInfo$cfu_mtb_per_gram <- as.numeric(RabbitInfo$cfu_mtb_per_gram)
RabbitInfo$tissue_weight <- as.numeric(RabbitInfo$tissue_weight)
RabbitInfo$lesion_size_mm <- as.numeric(RabbitInfo$lesion_size_mm)
RabbitInfo <- RabbitInfo %>%
  mutate(total_cfu = cfu_mtb_per_gram * tissue_weight)
RabbitInfo <- RabbitInfo %>%
  mutate(lesion_density = total_cfu/lesion_size_mm)



MtbTPM <- read_csv('./Data/Updated.Mtb.Expression.Gene.Data.TPM.csv')

SampleInfo <- left_join(SampleInfo, RabbitInfo, by = "SampleID")

Summary <- read_csv('./Data/Pipeline.Summary.Details.updated.csv')

