library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Bell-shaped dose-response",
                  titleWidth = 300

  ),
  dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 75px}"),  width = 300,
                   sidebarMenu(
                     selectInput("data_set","Data Set",choices = c("data_set_1","data_set_2","data_set_3")),
                    # actionButton("refit", "Refit Models"),
                     textOutput("message")
                    
                     
                     
                   ) 
            
                   
  ),
  dashboardBody(
   

              
         plotOutput ("plot") %>% withSpinner(color="#0dc5c1"),
        
          fluidRow(
            column(width=5,tags$h3("Input Data")),
            column(width=3,tags$h3("EC50 and GI50 results"))
          ),
          fluidRow(
           column(width=5,
                  DT::dataTableOutput('data')
           ),
           
           column(width=3,
                  DT::dataTableOutput('ec50')
           )
         )  
              
      ) #end plot 
      
    )







