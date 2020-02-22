library(caret)
library(shiny)
library(xgboost)
library(readr)

load("xg.rda")   
load("linear.rda")

shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize = 8000*1024^2)
  
  output$sample_input_data_heading = renderUI({  
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable({    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      head(input_data)
    }
  })
  predictions<-reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =readr::read_csv(input$file1$datapath, col_names = TRUE)
        prediction = predict(xg,as.matrix(input_data[,-1]))
        input_data_with_pm = cbind(input_data[,1],prediction) 
        input_data_with_pm
        
      })
    }
  })
  output$distplot <- renderTable({
      withProgress(message = 'Predictions in progress. Please wait ...', {
        nw=data.frame(ambient=input$num1,
                      coolant=input$num2,
                      u_d=input$num3,
                      u_q=input$num4,
                      motor_speed=input$num5,
                      torque=input$num6,
                      i_d=input$num7,
                      i_q=input$num8,
                      stator_yoke=input$num9,
                      stator_tooth=input$num10,
                      stator_winding=input$num11,
                      profile_id=input$num12)
        prediction2 = predict(linear,nw)
        prediction2
      })
    })
  
  
  output$sample_prediction_heading = renderUI({  
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable({ 
    pred = predictions()
    head(pred,20)
    
  })
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
      
})


