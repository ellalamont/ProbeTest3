## Figure
## MDS plot and PCA plot for comparing sample types broadly

## Ideas to change:
## Only use original two broth samples
## Only use some mimic samples (D28 and D42?)
## Only use certain rabbits?
## Check on Batch numbers

library(edgeR)
library(ggrepel)
library(sva)

## Import the data to make MtbReads, SampleInfo, RabbitInfo (which gets joined 
## to SampleInfo), and Summary. These contain information about coverage,
## metadata, and raw aligned reads from DuffyTools which are not normalized

source("data_import.R")


low_coverage <- Summary$N_Genomic < 1000000
low_coverage_SIDS <- Summary$SampleID[low_coverage]

include <- !SampleInfo$GroupStrain =='unenriched' & 
  !SampleInfo$SampleID == 'iMTB_14_S18' &  
  !SampleInfo$SampleID == 'R011_L6_12_S18' & 
  !SampleInfo$SampleID == 'RUL_Cav_R2_18mm_X_12mm_8_S38'

MtbReads <- MtbReads %>%
  select(1, SampleInfo$SampleID[include]) %>%
  select(!any_of(low_coverage_SIDS))

keeper_ids <- colnames(MtbReads[-1])

Summary <- Summary %>%
  filter(SampleID %in% keeper_ids)

SampleInfo <- SampleInfo %>%
  filter(SampleID %in% keeper_ids)



countdata <- MtbReads[,-1]
countdata <- as.matrix(countdata)
names <- as.data.frame(MtbReads[,1])
rownames(countdata) <- names[,1]

#Batch correction with ComBat-seq
batch <- SampleInfo$Batch
counts_corrected <- ComBat_seq(countdata, batch = batch)

y <- DGEList(counts_corrected)
z <- DGEList(countdata)

group <- factor(SampleInfo$GroupStrain)
y$samples$group <- group
y$samples$mimic_timepoint <- factor(SampleInfo$Mimic_timepoint)
y$samples$batch <- factor(SampleInfo$Batch)
y$samples$rabbit <- factor(SampleInfo$Rabbit_number)
y$samples$rabbit_lesion <- factor(SampleInfo$Rabbit_lesion)
y$samples$rabbit_replicate <- factor(SampleInfo$Rabbit_replicate)
y$samples$cfu <- as.numeric(SampleInfo$cfu_mtb_log10, na = "NA")
y$samples$age <- parse_factor(SampleInfo$cavity_age, levels = c("na", "under 100", "over 100, under 135", "over 120", "over 120 under 148", "over 135", "over 148"), ordered = TRUE)
y$samples$air <- SampleInfo$`cavity volume_air_percentage`
y$samples$stability <- factor(SampleInfo$stability)
y$samples$size <- as.numeric(SampleInfo$lesion_size_mm, na = "na")
y$samples$wall_type <- factor(SampleInfo$wall_type)
y$samples$caseum_type <- factor(SampleInfo$caseum_type)
y$samples$cluster <- factor(SampleInfo$cluster)
y$samples$lesion_density <- as.numeric(SampleInfo$lesion_density)

logcounts <- cpm(y,log=TRUE)
logcounts_notcorrected <- cpm(z, log=TRUE)

y <- calcNormFactors(y)
z <- calcNormFactors(z)
design <- model.matrix(~ 0 + group)
colnames(design) <- levels(group)

par(mfrow=c(1,1))
v <- voom(y, design, plot = TRUE)
w <- voom(z, design, plot = FALSE)

cols <- c("rabbit" = "darkorange", "mimic" = "purple", "broth" = "black", "thp1" = "darkgreen")

plotMDS(z)
mds <- plotMDS(y)
toplot <- data.frame(Dim1 = mds$x, 
                     Dim2 = mds$y, 
                     Group = y$samples$group, 
                     Mimic = y$samples$mimic_timepoint, 
                     Rabbit = y$samples$rabbit,
                     Replicate = y$samples$rabbit_replicate,
                     Lesion = y$samples$rabbit_lesion,
                     Density = y$samples$lesion_density
)

ggplot(toplot, aes(Dim1, Dim2, fill = Group)) +
  geom_point(size = 5, alpha = 0.8, shape = 21) +
  #geom_label_repel(data = subset(toplot, Group == "rabbit"), aes(label = Rabbit), fill = "white", show.legend = F, max.overlaps = 30) +
  scale_fill_manual(values = cols, labels = c("Broth", "Mimic", "Rabbit", "THP1")) +
  theme_bw() +
  theme(legend.title = element_text(size = 18, face = 'bold'), legend.text = element_text(size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 16))

ggplot(toplot, aes(Dim1, Dim2, fill = Density)) +
  geom_point(size = 5, alpha = 0.9, shape = 21) +
  #geom_label_repel(data = subset(toplot, Group == "rabbit" & Replicate != "na"), aes(label = Replicate), fill = "white", show.legend = F, max.overlaps = 30) +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(legend.title = element_text(size = 18, face = 'bold'), legend.text = element_text(size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 16))


# ggsave("MDS_batch_corrected.png")
