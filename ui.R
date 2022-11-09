library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinytitle)





dashboardPage(skin="black",
              
              dashboardHeader(title="Spondylo vs Met"),
              
              dashboardSidebar(
                               sidebarMenu(
                                 br(),
                                 menuItem(tags$em("Introduction",style="font-size:120%"),
                                          icon=icon("info"),
                                          tabName="intro"),
                                 menuItem(tags$em("Upload Test Data",style="font-size:120%"),
                                          icon=icon("upload"),
                                          tabName="data"),
                                 menuItem(tags$em("Download predictions",style="font-size:120%"),
                                          icon=icon("download"),
                                          tabName="download")
                                 
                               )
              ),
              
              dashboardBody(
                
                tabItems(
                  tabItem(tabName="intro",
                          
                          tags$h4("With this prediction app, you can upload your data and get back predictions.", 
                                  style="font-size:150%"),
                          
                          br(),
                          
                          tags$h4("This is a random forest model that predicts whether the spinal lession is spondylodiscitis or metastasis 
                                  according to the radiomic features extracted from a lession (e.x using Lifex software).", 
                                  style="font-size:150%"),
                          
                          br(),
                          br(),
                          br(),
                          
                          fluidRow(
                            # A static valueBox
                            valueBox(83.3, "Training set accuracy", icon = icon("percent"), width = 2, color = "olive"),
                            
                            valueBox(93.4, "Test set accuracy", icon = icon("percent"), width = 2, color = "orange")
                          ),
                          
                          br(),
                          
                          fluidRow(
                            # A static valueBox
                            valueBox(98.6, "Training set AUC", icon = icon("percent"), width = 2, color = "olive"),
                            
                            valueBox(97.0, "Test set AUC", icon = icon("percent"), width = 2, color = "orange")
                          )
                          
                  ),
                  
                  
                  
                  tabItem(tabName="data",
                          
                          
                          tags$h4("To predict using this model, upload test data in excel format. ",style="font-size:150%"),
                          br(),
                          
                          tags$h4("Please make sure that your excel file contains features:", style="font-size:150%"),
                          tags$h4("GLCM_homogeneity, GLCM_contrast, 
                                  GLCM_dissimilarity, GLRLM_SRE, GLZLM_GLNU, GLZLM_ZP, NGLDM_Busyness, GLRLM_LGRE, GLRLM_SRLGE,
                                   GLZLM_SZHGE, GLRLM_GLNU, GRLM_RLNU, GLRLM_RP, GLZLM_LZE, GLZLM_LZLGE, NGLDM_Coarseness,
                                   , NGLDM_Contrast, GLZLM_SZE", style="font-size:150%"),
                          br(),
                          
                          tags$h4("Then, go to the", 
                                  tags$span("Download predictions",style="color:green"),
                                  tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),
                          
  
                          br(),
                          column(width = 6,
                                 fileInput('file1', 
                                           em('Upload test data in excel format ',style="text-align:center;color:blue;font-size:150%"),
                                           multiple = FALSE,
                                           accept=c('.xlsx')),

                          )
                          
                  ),
                  
                  
                  tabItem(tabName="download",
                          
                          fluidRow(

                            column(width = 12,
                                   tags$h4("After you upload a test dataset, you can download the test data set together with predictions in xlsx format by
                                    clicking the button below.", style="font-size:150%"),
                                   br(),
                                   br()
                            )),
                          
                          fluidRow(
                            column(width = 4,
                                   uiOutput("sample_prediction_heading"),
                                   tableOutput("sample_predictions")
                                   
                            )),
                          
                          fluidRow(
                            
                            column(width = 3,
                                   downloadButton("downloadData", em('Download predictions',style="text-align:center;color:green;font-size:160%")),
                                   br(),
                                   br(),
                                   br()
                            ))
                                


                          
                  ))))
              
            
