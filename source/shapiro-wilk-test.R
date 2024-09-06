
library(moments)
library(readxl)

features <- read_excel("../data/features.xlsx")
# change the quality column to a factor type
features$Disease <- as.factor(features$Disease)
M <- features[41:80, ]
S <- features[0:40, ]

#kontrola normalneho rozlozenia dat pre Spondylo 

skewness(features$GLCM_contrast[0:40]) #1.11
skewness(sqrt(features$GLCM_contrast[0:40])) # 0.38
shapiro.test(sqrt(features$GLCM_contrast[0:40])) # p=0.08 OK
S$GLCM_contrast <- sqrt(features$GLCM_contrast[0:40])
features_n$GLCM_contrast[0:40] <- sqrt(features$GLCM_contrast[0:40])

skewness(features$GLCM_correlation[0:40])
shapiro.test(features$GLCM_correlation[0:40]) # p=0.14 OK

skewness(features$GLCM_dissimilarity[0:40]) #0.31
shapiro.test(features$GLCM_dissimilarity[0:40]) #p=0.14 OK

skewness(features$GLCM_energy[0:40]) #3.02
skewness(log10(features$GLCM_energy[0:40])) #0.89
shapiro.test(log10(features$GLCM_energy[0:40])) # p=0.02626 NIE OK

skewness(features$GLCM_homogeneity[0:40]) #1.06
skewness(log10(features$GLCM_homogeneity[0:40])) #0.0.157928
shapiro.test(log10(features$GLCM_homogeneity[0:40])) # p=0.7955  NIE OK

skewness(features$GLCM_entropy_log2[0:40]) # -1.08
shapiro.test(features$GLCM_entropy_log2[0:40]) #p=0.0.03927 NIE OK

skewness(features$GLRLM_SRE[0:40]) #-2.2
skewness(log10(max(features$GLRLM_SRE[0:40]+1)-features$GLRLM_SRE[0:40])) #1.9
shapiro.test(log10(max(features$GLRLM_SRE[0:40]+1)-features$GLRLM_SRE[0:40])) #NIE OK

skewness(features$GLRLM_LRE[0:40]) #3.47
skewness(log10(features$GLRLM_LRE[0:40])) #2.13 
shapiro.test(log10(features$GLRLM_LRE[0:40])) #NIE OK

skewness(features$GLRLM_LGRE[0:40]) #1.72
skewness(sqrt(features$GLRLM_LGRE[0:40])) #0.64
shapiro.test(sqrt(features$GLRLM_LGRE[0:40])) # p=0.11 OK
S$GLRLM_LGRE <- sqrt(features$GLRLM_LGRE[0:40])
features_n$GLRLM_LGRE[0:40] <- sqrt(features$GLRLM_LGRE[0:40])

skewness(features$GLRLM_HGRE[0:40]) #1.14
skewness(sqrt(features$GLRLM_HGRE[0:40])) #0.41
shapiro.test(sqrt(features$GLRLM_HGRE[0:40])) # p=0.67 OK
S$GLRLM_HGRE <- sqrt(features$GLRLM_HGRE[0:40])
features_n$GLRLM_HGRE[0:40] <- sqrt(features$GLRLM_HGRE[0:40])

skewness(features$GLRLM_LRHGE[0:40]) #2.71
skewness(log10(features$GLRLM_LRHGE[0:40])) #0.61
shapiro.test(log10(features$GLRLM_LRHGE[0:40])) # p=0.23 OK
S$GLRLM_LRHGE <- log10(features$GLRLM_LRHGE[0:40])
features_n$GLRLM_LRHGE[0:40] <-log10(features$GLRLM_LRHGE[0:40])

skewness(features$GLRLM_LRLGE[0:40]) #4.0
skewness(1/(features$GLRLM_LRLGE[0:40])) #1.42
shapiro.test(1/(features$GLRLM_LRLGE[0:40])) # NIE OK

skewness(features$GLRLM_SRLGE[0:40]) # 0.8
skewness(sqrt(features$GLRLM_SRLGE[0:40])) #0.33
shapiro.test(sqrt(features$GLRLM_SRLGE[0:40])) # p=0.31 OK
S$GLRLM_SRLGE <- sqrt(features$GLRLM_SRLGE[0:40])
features_n$GLRLM_SRLGE[0:40] <-sqrt(features$GLRLM_SRLGE[0:40])

skewness(features$GLRLM_SRHGE[0:40]) #1.13
skewness(sqrt(features$GLRLM_SRHGE[0:40])) #0.28
shapiro.test(sqrt(features$GLRLM_SRHGE[0:40])) # p=0.83 OK
S$GLRLM_SRHGE <- sqrt(features$GLRLM_SRHGE[0:40])
features_n$GLRLM_SRHGE[0:40] <-sqrt(features$GLRLM_SRHGE[0:40]) 

skewness(features$GLRLM_GLNU[0:40]) #1.75
skewness(log10(features$GLRLM_GLNU[0:40])) #0.26
shapiro.test(log10(features$GLRLM_GLNU[0:40])) # p=0.21 OK
S$GLRLM_GLNU <- log10(features$GLRLM_GLNU[0:40])
features_n$GLRLM_GLNU[0:40] <- log10(features$GLRLM_GLNU[0:40])

skewness(features$GLRLM_RLNU[0:40]) #1.62
skewness(log10(features$GLRLM_RLNU[0:40])) #0.043
shapiro.test(log10(features$GLRLM_RLNU[0:40])) # p=0.29 OK
S$GLRLM_RLNU <- log10(features$GLRLM_RLNU[0:40])
features_n$GLRLM_RLNU[0:40] <- log10(features$GLRLM_RLNU[0:40])

skewness(features$GLRLM_RP[0:40]) # -1.88
skewness(log10(max(features$GLRLM_RP[0:40]+1)-features$GLRLM_RP[0:40])) #1.61
shapiro.test(log10(max(features$GLRLM_RP[0:40]+1)-features$GLRLM_RP[0:40])) # NIE OK

skewness(features$NGLDM_Busyness[0:40]) #3.0
skewness(log10(features$NGLDM_Busyness[0:40])) #0.47
shapiro.test(log10(features$NGLDM_Busyness[0:40])) # p=0.1 OK
S$NGLDM_Busyness <- log10(features$NGLDM_Busyness[0:40])
features_n$NGLDM_Busyness[0:40] <- log10(features$NGLDM_Busyness[0:40])

skewness(features$NGLDM_Coarseness[0:40]) # 1.71
skewness(log10(features$NGLDM_Coarseness[0:40])) # 0.07
shapiro.test(log10(features$NGLDM_Coarseness[0:40])) # p= 0.46 OK
S$NGLDM_Coarseness <- log10(features$NGLDM_Coarseness[0:40])
features_n$NGLDM_Coarseness[0:40] <- log10(features$NGLDM_Coarseness[0:40])

skewness(features$NGLDM_Contrast[0:40]) # 1.46
skewness(sqrt(features$NGLDM_Contrast[0:40])) # 0.28
shapiro.test(sqrt(features$NGLDM_Contrast[0:40])) # p=0.51 OK
S$NGLDM_Contrast <- sqrt(features$NGLDM_Contrast[0:40])
features_n$NGLDM_Contrast[0:40] <- sqrt(features$NGLDM_Contrast[0:40])

skewness(features$GLZLM_SZE[0:40]) #-0.02
shapiro.test(features$GLZLM_SZE[0:40]) # p=0.6 OK

skewness(features$GLZLM_LZE[0:40]) # 3.87
skewness(log10(features$GLZLM_LZE[0:40])) # 0.35
shapiro.test(log10(features$GLZLM_LZE[0:40])) # p=0.059 NIE OK

skewness(features$GLZLM_LGZE[0:40]) # 2.31
skewness(log10(features$GLZLM_LGZE[0:40])) #0.49
shapiro.test(log10(features$GLZLM_LGZE[0:40])) # p=0.37 OK
S$GLZLM_LGZE <- log10(features$GLZLM_LGZE[0:40])
features_n$GLZLM_LGZE[0:40]<- log10(features$GLZLM_LGZE[0:40])

skewness(features$GLZLM_HGZE[0:40]) # 1.01
skewness(sqrt(features$GLZLM_HGZE[0:40])) # 0.24
shapiro.test(sqrt(features$GLZLM_HGZE[0:40])) # p=0.76 OK
S$GLZLM_HGZE <- sqrt(features$GLZLM_HGZE[0:40])
features_n$GLZLM_HGZE[0:40]<- sqrt(features$GLZLM_HGZE[0:40])

skewness(features$GLZLM_SZLGE[0:40]) #2.8
skewness(log10(features$GLZLM_SZLGE[0:40])) #0.82
shapiro.test(log10(features$GLZLM_SZLGE[0:40])) # p=0.036 NIE OK

skewness(features$GLZLM_SZHGE[0:40]) # 1.17
skewness(sqrt(features$GLZLM_SZHGE[0:40])) # 0.46
shapiro.test(sqrt(features$GLZLM_SZHGE[0:40])) # p=0.23 OK
S$GLZLM_SZHGE <- sqrt(features$GLZLM_SZHGE[0:40])
features_n$GLZLM_SZHGE[0:40]<- sqrt(features$GLZLM_SZHGE[0:40])

skewness(features$GLZLM_LZHGE[0:40]) #2.34
skewness(log10(features$GLZLM_LZHGE[0:40])) #0.25
shapiro.test(log10(features$GLZLM_LZHGE[0:40])) # p=0.037 NIE OK

skewness(features$GLZLM_LZLGE[0:40]) #4.13
skewness(log10(features$GLZLM_LZLGE[0:40])) #0.374
shapiro.test(log10(features$GLZLM_LZLGE[0:40])) # p=0.15 OK
S$GLZLM_LZLGE <- log10(features$GLZLM_LZLGE[0:40])
features_n$GLZLM_LZLGE[0:40]<- log10(features$GLZLM_LZLGE[0:40])

skewness(features$GLZLM_GLNU[0:40]) #1.74
skewness(sqrt(features$GLZLM_GLNU[0:40])) #0.7
shapiro.test(sqrt(features$GLZLM_GLNU[0:40])) # p=0.057 OK
S$GLZLM_GLNU<- sqrt(features$GLZLM_GLNU[0:40])
features_n$GLZLM_GLNU[0:40]<- sqrt(features$GLZLM_GLNU[0:40])

skewness(features$GLZLM_ZLNU[0:40]) #2.21
skewness(log10(features$GLZLM_ZLNU[0:40])) #-0.37
shapiro.test(log10(features$GLZLM_ZLNU[0:40])) # p=0.81 OK
S$GLZLM_ZLNU<- log10(features$GLZLM_ZLNU[0:40])
features_n$GLZLM_ZLNU[0:40]<- log10(features$GLZLM_ZLNU[0:40])

skewness(features$GLZLM_ZP[0:40]) #0.41
skewness(sqrt(features$GLZLM_ZP[0:40])) #0.008
shapiro.test(sqrt(features$GLZLM_ZP[0:40])) # p=0.02 NIE OK


##################################################################################################################

# kontrola normalneho rozlozenia dat pre Metastazy

skewness(features$GLCM_contrast[41:80]) # 1.75
skewness(log10(features$GLCM_contrast[41:80])) # -0.19
shapiro.test(log10(features$GLCM_contrast[41:80])) # p=0.36 OK
M$GLCM_contrast <- log10(features$GLCM_contrast[41:80])
features_n$GLCM_contrast[41:80] <- log10(features$GLCM_contrast[41:80])

skewness(features$GLCM_correlation[41:80]) #0.2
shapiro.test(features$GLCM_correlation[41:80]) # p=0.13 OK

skewness(features$GLCM_homogeneity[41:80]) # 0.35
shapiro.test(features$GLCM_homogeneity[41:80])  # p=0.06 OK

skewness(features$GLCM_energy[41:80]) # 3.72
skewness(1/(features$GLCM_energy[41:80])) # 1.54
shapiro.test(1/(features$GLCM_energy[41:80])) # NIE OK

skewness(features$GLCM_dissimilarity[41:80]) #0.86
skewness(sqrt(features$GLCM_dissimilarity[41:80])) #0.24
shapiro.test(sqrt(features$GLCM_dissimilarity[41:80])) # p=0.27 OK
M$GLCM_dissimilarity <- sqrt(features$GLCM_dissimilarity[41:80])
features_n$GLCM_dissimilarity[41:80] <- sqrt(features$GLCM_dissimilarity[41:80])

skewness(features$GLCM_entropy_log2[41:80]) #0.35
shapiro.test(features$GLCM_entropy_log2[41:80]) # p=0.32 OK

skewness(features$GLRLM_SRE[41:80]) # -1.17
skewness(log10(max(features$GLRLM_SRE[41:80]+1)-features$GLRLM_SRE[41:80])) #1.08
shapiro.test(log10(max(features$GLRLM_SRE[41:80]+1)-features$GLRLM_SRE[41:80])) # NIE OK

skewness(features$GLRLM_LRE[41:80]) #2.75
skewness(log10(features$GLRLM_LRE[41:80])) # 1.94
shapiro.test(log10(features$GLRLM_LRE[41:80])) # NIE OK

skewness(features$GLRLM_LGRE[41:80]) #1.89
skewness(sqrt(features$GLRLM_LGRE[41:80])) #0.49
shapiro.test(sqrt(features$GLRLM_LGRE[41:80])) # p=0.3
M$GLRLM_LGRE <- sqrt(features$GLRLM_LGRE[41:80]) 
features_n$GLRLM_LGRE[41:80]<- sqrt(features$GLRLM_LGRE[41:80]) 

skewness(features$GLRLM_HGRE[41:80]) # 3.19
skewness(log10(features$GLRLM_HGRE[41:80])) #0.53
shapiro.test(log10(features$GLRLM_HGRE[41:80])) # p=0.34 OK
M$GLRLM_HGRE <- log10(features$GLRLM_HGRE[41:80]) 
features_n$GLRLM_HGRE[41:80]<- log10(features$GLRLM_HGRE[41:80]) 

skewness(features$GLRLM_SRLGE[41:80]) #1.99
skewness(sqrt(features$GLRLM_SRLGE[41:80])) #0.51
shapiro.test(sqrt(features$GLRLM_SRLGE[41:80])) # p=0.22 OK
M$GLRLM_SRLGE <- sqrt(features$GLRLM_SRLGE[41:80]) 
features_n$GLRLM_SRLGE[41:80] <- sqrt(features$GLRLM_SRLGE[41:80]) 

skewness(features$GLRLM_SRHGE[41:80]) # 2.65
skewness(log10(features$GLRLM_SRHGE[41:80])) # 0.31
shapiro.test(log10(features$GLRLM_SRHGE[41:80])) # p=0.67 OK
M$GLRLM_SRHGE <- log10(features$GLRLM_SRHGE[41:80]) 
features_n$GLRLM_SRHGE[41:80]<- log10(features$GLRLM_SRHGE[41:80])

skewness(features$GLRLM_LRLGE[41:80]) # 1.98
skewness(sqrt(features$GLRLM_LRLGE[41:80])) # 0.73
shapiro.test(sqrt(features$GLRLM_LRLGE[41:80])) # p= 0.077 OK
M$GLRLM_LRLGE <- sqrt(features$GLRLM_LRLGE[41:80])
features_n$GLRLM_LGRE[41:80]<- sqrt(features$GLRLM_LRLGE[41:80])

skewness(features$GLRLM_LRHGE[41:80]) #4.04
skewness(1/(features$GLRLM_LRHGE[41:80])) # 0.83
shapiro.test(1/(features$GLRLM_LRHGE[41:80])) # p=0.01 NIE OK

skewness(features$GLRLM_GLNU[41:80]) # 0.42
skewness(sqrt(features$GLRLM_GLNU[41:80])) # -0.002
shapiro.test(sqrt(features$GLRLM_GLNU[41:80])) # p=0.065 OK
M$GLRLM_GLNU <- sqrt(features$GLRLM_GLNU[41:80]) 
features_n$GLRLM_GLNU[41:80] <- sqrt(features$GLRLM_GLNU[41:80]) 

skewness(features$GLRLM_RLNU[41:80]) # 1.02
skewness(log10(features$GLRLM_RLNU[41:80])) # 0.4
shapiro.test(log10(features$GLRLM_RLNU[41:80])) # p=0.07 OK
M$GLRLM_RLNU <- log10(features$GLRLM_RLNU[41:80]) 
features_n$GLRLM_RLNU[41:80]<- log10(features$GLRLM_RLNU[41:80]) 

skewness(features$GLRLM_RP[41:80]) # -1.6
skewness(log10(max(features$GLRLM_RP[41:80]+1)-features$GLRLM_RP[41:80])) # 1.59
shapiro.test(log10(max(features$GLRLM_RP[41:80]+1)-features$GLRLM_RP[41:80])) # NIE OK

skewness(features$NGLDM_Coarseness[41:80]) #0.55
shapiro.test(features$NGLDM_Coarseness[41:80]) # p=0.06 OK

skewness(features$NGLDM_Contrast[41:80]) #2.0
skewness(log10(features$NGLDM_Contrast[41:80])) # -0.02
shapiro.test(log10(features$NGLDM_Contrast[41:80])) # p=0.84 OK
M$NGLDM_Contrast <- log10(features$NGLDM_Contrast[41:80])
features_n$NGLDM_Contrast[41:80]<- log10(features$NGLDM_Contrast[41:80])

skewness(features$NGLDM_Busyness[41:80]) # 0.84
skewness(sqrt(features$NGLDM_Busyness[41:80])) #0.31
shapiro.test(sqrt(features$NGLDM_Busyness[41:80])) # p=0.28 OK
M$NGLDM_Busyness <- log10(features$NGLDM_Busyness[41:80])
features_n$NGLDM_Busyness[41:80]<- log10(features$NGLDM_Busyness[41:80])

skewness(features$GLZLM_SZE[41:80]) #0.05
shapiro.test(features$GLZLM_SZE[41:80]) # p=0.43 OK

skewness(features$GLZLM_LZE[41:80]) # 3.75
skewness(log10(features$GLZLM_LZE[41:80])) # 0.2
shapiro.test(log10(features$GLZLM_LZE[41:80])) # p=0.11 OK

skewness(features$GLZLM_LGZE[41:80]) # 2.32
skewness(sqrt(features$GLZLM_LGZE[41:80])) #0.84
shapiro.test(sqrt(features$GLZLM_LGZE[41:80])) # p=0.06 OK
M$GLZLM_LGZE <- sqrt(features$GLZLM_LGZE[41:80])
features_n$GLZLM_LGZE[41:80]<- sqrt(features$GLZLM_LGZE[41:80])

skewness(features$GLZLM_HGZE[41:80]) # 2.57
skewness(log10(features$GLZLM_HGZE[41:80])) # 0.34
shapiro.test(log10(features$GLZLM_HGZE[41:80])) # p=0.5 OK
M$GLZLM_HGZE <- log10(features$GLZLM_HGZE[41:80])
features_n$GLZLM_HGZE[41:80]<- log10(features$GLZLM_HGZE[41:80])

skewness(features$GLZLM_SZLGE[41:80]) #2.51
skewness(log10(features$GLZLM_SZLGE[41:80])) #-0.56
shapiro.test(log10(features$GLZLM_SZLGE[41:80])) # p=0.01 NIE OK

skewness(features$GLZLM_SZHGE[41:80]) # 2.78
skewness(log10(features$GLZLM_SZHGE[41:80])) # 0.14
shapiro.test(log10(features$GLZLM_SZHGE[41:80])) # p=0.81 OK
M$GLZLM_SZHGE <- log10(features$GLZLM_SZHGE[41:80])
features_n$GLZLM_SZHGE[41:80]<- log10(features$GLZLM_SZHGE[41:80]) #2.34

skewness(log10(features$GLZLM_LZHGE[0:40])) #0.25
shapiro.test(log10(features$GLZLM_LZHGE[0:40])) # p=0.037 NIE OK

skewness(features$GLZLM_LZHGE[41:80]) #4.28
skewness(log10(features$GLZLM_LZHGE[41:80])) #1.41
shapiro.test(log10(features$GLZLM_LZHGE[41:80])) # NIE OK

skewness(features$GLZLM_LZLGE[41:80]) #5.33
skewness(log10(features$GLZLM_LZLGE[41:80])) #0.009
shapiro.test(log10(features$GLZLM_LZLGE[41:80])) # p=0.2 OK
M$GLZLM_LZLGE <- log10(features$GLZLM_LZLGE[41:80])
features_n$GLZLM_LZLGE[41:80]<- log10(features$GLZLM_LZLGE[41:80])

skewness(features$GLZLM_GLNU[41:80]) #1.48
skewness(log10(features$GLZLM_GLNU[41:80])) #0.51
shapiro.test(log10(features$GLZLM_GLNU[41:80])) # p=0.06 OK
M$GLZLM_GLNU<- log10(features$GLZLM_GLNU[41:80])
features_n$GLZLM_GLNU[41:80]<- log10(features$GLZLM_GLNU[41:80])

skewness(features$GLZLM_ZLNU[41:80]) #1.87
skewness(log10(features$GLZLM_ZLNU[41:80])) # 0.29
shapiro.test(log10(features$GLZLM_ZLNU[41:80])) # p=0.23 OK
M$GLZLM_ZLNU<- log10(features$GLZLM_ZLNU[41:80])
features_n$GLZLM_ZLNU[41:80]<- log10(features$GLZLM_ZLNU[41:80])

skewness(features$GLZLM_ZP[41:80]) #0.06
shapiro.test(features$GLZLM_ZP[41:80]) # p=0.09 OK