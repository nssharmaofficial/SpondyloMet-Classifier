library(ggpubr)
library(plotly)
library(readxl)

features <- read_excel("../data/features.xlsx")
# change the quality column to a factor type
features$Disease <- as.factor(features$Disease)

wilcox.test(GLCM_entropy_log2 ~ Disease, data = features) # p=0.03586 SIG
ggboxplot(features, x = "Disease", y = "GLCM_entropy_log2", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_entropy_log2", xlab = "Disease")
ggplotly()

wilcox.test(GLCM_contrast ~ Disease, data = features) # p=2.432e-07 SIG
ggboxplot(features, x = "Disease", y = "GLCM_contrast", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_contrast", xlab = "Disease")
ggplotly()

wilcox.test(GLCM_correlation ~ Disease, data = features) # p=0.02116 SIG
ggboxplot(features, x = "Disease", y = "GLCM_correlation", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_correlation", xlab = "Disease")
ggplotly()

wilcox.test(GLCM_dissimilarity ~ Disease, data = features) # p=1.682e-07 SIG
ggboxplot(features, x = "Disease", y = "GLCM_dissimilarity", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_dissimilatity", xlab = "Disease")
ggplotly()

wilcox.test(GLCM_energy ~ Disease, data = features) # p=0.01339 SIG
ggboxplot(features, x = "Disease", y = "GLCM_energy", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_energy", xlab = "Disease")
ggplotly()

wilcox.test(GLCM_homogeneity ~ Disease, data = features) # p=1.117e-06 SIG
ggboxplot(features, x = "Disease", y = "GLCM_homogeneity", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLCM_homogeneity", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_SRE~ Disease, data = features) # p=1.25e-06 SIG
ggboxplot(features, x = "Disease", y = "GLRLM_SRE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_SRE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_LRE~ Disease, data = features) # p=4.353e-06 SIG
ggboxplot(features, x = "Disease", y = "GLRLM_LRE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_LRE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_LGRE~ Disease, data = features) # p=0.8746 
ggboxplot(features, x = "Disease", y = "GLRLM_LGRE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_LGRE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_HGRE~ Disease, data = features) # p=0.003702 SIG 
ggboxplot(features, x = "Disease", y = "GLRLM_HGRE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_HGRE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_SRLGE~ Disease, data = features) # p= 0.6772
ggboxplot(features, x = "Disease", y = "GLRLM_SRLGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_SRLGE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_SRHGE~ Disease, data = features) # p=0.001346 SIG
ggboxplot(features, x = "Disease", y = "GLRLM_SRHGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_SRHGE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_LRLGE~ Disease, data = features) # p=0.1302 
ggboxplot(features, x = "Disease", y = "GLRLM_LRLGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_LRLGE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_LRHGE~ Disease, data = features) # p=0.9124 
ggboxplot(features, x = "Disease", y = "GLRLM_LRHGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_LRHGE", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_GLNU~ Disease, data = features) # p=8.962e-11 SIG 
ggboxplot(features, x = "Disease", y = "GLRLM_GLNU", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_GLNU", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_RLNU~ Disease, data = features) # p=0.0001311 SIG 
ggboxplot(features, x = "Disease", y = "GLRLM_RLNU", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_RLNU", xlab = "Disease")
ggplotly()

wilcox.test(GLRLM_RP~ Disease, data = features) # p=5.327e-06 SIG 
ggboxplot(features, x = "Disease", y = "GLRLM_RP", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLRLM_RP", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_SZE~ Disease, data = features) # p=0.000602 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_SZE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_SZE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_LZE~ Disease, data = features) # p=1.156e-07 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_LZE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_LZE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_LGZE~ Disease, data = features) # p=0.1653
ggboxplot(features, x = "Disease", y = "GLZLM_LGZE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_LGZE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_HGZE~ Disease, data = features) # p=0.004769 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_HGZE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_HGZE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_SZLGE~ Disease, data = features) # p=0.3501
ggboxplot(features, x = "Disease", y = "GLZLM_SZLGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_SZLGE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_SZHGE~ Disease, data = features) # p=0.00178 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_SZHGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_SZHGE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_LZLGE~ Disease, data = features) # p=2.06e-05 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_LZLGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_LZLGE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_LZHGE~ Disease, data = features) # p= 1.018e-07 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_LZHGE", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_LZHGE", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_GLNU~ Disease, data = features) # p= 2.847e-06 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_GLNU", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_GLNU", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_ZLNU~ Disease, data = features) # p= 0.7413
ggboxplot(features, x = "Disease", y = "GLZLM_ZLNU", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_ZLNU", xlab = "Disease")
ggplotly()

wilcox.test(GLZLM_ZP~ Disease, data = features) # p= 6.93e-08 SIG
ggboxplot(features, x = "Disease", y = "GLZLM_ZP", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "GLZLM_ZP", xlab = "Disease")
ggplotly()

wilcox.test(NGLDM_Coarseness~ Disease, data = features) # p= 1.699e-05 SIG
ggboxplot(features, x = "Disease", y = "NGLDM_Coarseness", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "NGLDM_Coarseness", xlab = "Disease")
ggplotly()

wilcox.test(NGLDM_Contrast~ Disease, data = features) # p= 1.484e-08 SIG
ggboxplot(features, x = "Disease", y = "NGLDM_Contrast", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "NGLDM_Contrast", xlab = "Disease")
ggplotly()

wilcox.test(NGLDM_Busyness~ Disease, data = features) # p= 1.311e-07 SIG
ggboxplot(features, x = "Disease", y = "NGLDM_Busyness", 
          fill = "Disease", palette = c("#1B9E77", "#D95F02"),
          order = c("M", "S"),
          ylab = "NGLDM_Busyness", xlab = "Disease")
ggplotly()