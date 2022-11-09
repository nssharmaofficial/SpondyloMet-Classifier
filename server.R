library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(randomForest)
library(pROC)
library(caret)
library(kernlab)
library(e1071)
library(readxl)
library(writexl)
library(DT)



load("MyML.rda")    # Load saved model



shinyServer(function(input, output, session) {
    
    options(shiny.maxRequestSize = 800*1024^2)  
    # This is a number which specifies the maximum web request size, 
    # which serves as a size limit for file uploads. 
    # If unset, the maximum request size defaults to 5MB.
    # The value I have put here is 80MB
  
  
    
    
    predictions<-reactive({
        
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }else{
            withProgress(message = 'Predictions in progress. Please wait ...', {
                input_data =  readxl::read_excel(input$file1$datapath, col_names = TRUE)
                prediction = predict(rf, input_data, type = "prob")
                input_data_with_prediction = cbind(prediction, input_data)
                input_data_with_prediction
            })
        }
    })
    
    
    
    output$sample_prediction_heading = renderUI({  
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }else{
            tags$h4('Preview of the first few predictions')
        }
    })
    
    output$sample_predictions = renderTable({
        
        inFile<- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }else{
          pred = predictions()
          head(pred[ ,c("S", "M")]*100)
        }
        
          
        
    })
    
    
    
    
    # Downloadable csv of predictions ----
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("input_data_with_predictions", ".xlsx")
        },
        content = function(file) {
            write_xlsx(predictions(), file)
        })
    
    
})
