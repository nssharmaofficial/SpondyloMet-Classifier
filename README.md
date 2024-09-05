# Shiny app for random forest predictor

[Web app](https://natasabrisudova.shinyapps.io/ML-app/) using Shiny for binary prediction. 

Prediction is made using the final random forest model from publication:

---

## Radiomics Analysis of PET/CT Findings with Fludeoxyglucose (18F) as a Biomarker in the Differentiation of Spondylodiscitis and Bone Metastasis in Patients with Focal Lesions in the Spine

**Authors:** Natasa Brisudova¹, Sona Balogova², Iveta Waczulikova¹  
**Correspondence:** Natasa Brisudova

**Affiliations:**
¹Faculty of Mathematics, Physics and Informatics, Charles University, Bratislava  
²Faculty of Medicine, Charles University, Bratislava

### Introduction

Early initiation of targeted treatment can prevent possible irreversible neurological complications of spondylodiscitis (SD) and/or spinal metastases (MET). However, differentiation between these conditions can be challenging, especially in the early stages. 

**Aim:** To identify the radiometric characteristics of PET with FDG that help distinguish SD from MET.

### Methods

A retrospective analysis was performed on 31 second- and higher-order radiometric elements in 60 patients, with 30 confirmed cases of SD and 30 cases of MET from various malignancies. A total of 40 SD findings and 40 MET findings were analyzed using LIFEx freeware, which calculates conventional, textural, and shape elements of diagnostic images.

Clinical characteristics of the patients were compared using the non-parametric Wilcoxon rank-sum test. Diagnostic accuracy was assessed using the ROC curve. Additionally, the predictive ability to distinguish SD and MET was evaluated using machine learning. Three methods were tested: multiple logistic regression, random forest, and support vector machines, with three different data selection methods: K-fold cross-validation, Leave-One-Out Cross-Validation, and Train-Test Split.

### Results

Among the 31 radiometric elements, 24 were statistically significant (p < 0.05) in distinguishing SD from MET. Out of these, 9 elements had an AUC > 80% for diagnostic accuracy. The highest values were achieved by the following parameters:

- **GLZLM_ZP** (cut-off = 0.38, AUC = 83.25%)
- **NGLDM_Contrast** (cut-off = 0.17, AUC = 84.7%)
- **GLRLM_GLNU** (cut-off = 46.1, AUC = 88.8%)

In machine learning, the Random Forest method with Train-Test Split data selection was the most effective, achieving a cut-off of 0.28 and an AUC of 98.61%.

### Conclusions

The results confirm that radiomic analysis and machine learning are promising approaches for distinguishing between SD and MET in PET/CT with FDG. Further validation of these methods is supported by the findings.
