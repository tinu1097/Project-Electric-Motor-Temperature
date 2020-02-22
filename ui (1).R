library(shinydashboard)
library(shiny)
library(shinythemes)

dashboardPage(skin="black",
              dashboardHeader(title=tags$em("Shiny prediction app", style="text-align:center;color:#006600;font-size:100%"),titleWidth = 800),
              
              dashboardSidebar(width = 250,
                               sidebarMenu(
                                 br(),
                                 menuItem(tags$em("Upload Test Data",style="font-size:120%"),icon=icon("upload"),tabName="data"),
                                 menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download"),
                                 menuItem(tags$em("predictt",style="font-size:120%"),icon = icon("laptop-code"),tabName = "predictt")
                                 
                               )
              ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName="data",
                                 column(width = 7,
                                 fileInput('file1', em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                           accept=c('.csv')),
                                 
                                 uiOutput("sample_input_data_heading"),
                                 tableOutput("sample_input_data"),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                          ),
                          br()
                          
                  ),
                  
                  
                  tabItem(tabName="download",
                          fluidRow(
                            br(),
                            br(),
                            br(),
                            br(),
                            column(width = 7,
                                   tags$h4("Electric motor Temperature.", 
                                           style="font-size:200%"),
                                   br(),
                                   br()
                            )),
                          fluidRow(
                            
                            column(width = 12,
                                   downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
                                   plotOutput('plot_predictions')
                            ),
                            br(),
                            br(),
                            br(),
                                   uiOutput("sample_prediction_heading"),
                                   tableOutput("sample_predictions")
                            )
                            

                                                      ),
                  tabItem(tabName="predictt",
                          fluidRow(
                            column(width=7,
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                    sidebarPanel(
                                    
                                    numericInput("num1","Enter Ambient",1,step = 0.5),
                                    numericInput("num2","Enter Coolant  ",1,step = 0.5),
                                    numericInput("num3","Enter u_d ",1,step = 0.5),
                                    numericInput("num4","Enter u_q ",1,step = 0.5),
                                    numericInput("num5","ENTER motor_speed",1,step = 0.5),
                                    numericInput("num6","ENTER torque ",1,step = 0.5),
                                    numericInput("num7","ENTER i_d ",1,step = 0.5),
                                    numericInput("num8","ENTER i_q ",1,step = 0.5),
                                    numericInput("num9","ENTER stator_yoke ",1,step = 0.5),
                                    numericInput("num10","ENTER stator_tooth",1,step = 0.5),
                                    numericInput("num11","Enter stator_winding",1,step = 0.5),
                                    numericInput("num12","Enter profile_id",1,step = 1),
                                    submitButton('submit'),
                                    
                              )
                                   )
                            ),
                          fluidRow(
                            column(width=5,
                                   tableOutput('distplot')))
                  )
                  )
                  
                          
                              
                              
                          )
                          
)
                  







