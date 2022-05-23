
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


source("make_fits.R")
source("make_plots.R")

library(shiny)
library(tidyr)
library(dplyr)
library(minpack.lm)
library(nls2)
library(stringr)
library(ggplot2)

shinyServer(function(input, output) {

  
  
  observeEvent(input$refit,
               {
                 
                                  
                 output$plot<-renderPlot({  
                 
                   fit<-get_fit()
                 
                 
                 make_plots(fit$fit1,fit$fit2,fit$fit3,fit$fit_x,fit$means)       
                 })
  
  
  })
  
 
  
  output$plot <- renderPlot({ 
    

    
    
    fit<-get_fit()
    
  
    make_plots(fit$fit1,fit$fit2,fit$fit3,fit$fit_x,fit$means)
   
   #plot(c(1,2))
    })
  
  
 output$data <- DT::renderDataTable({
    
 data <- get_data()
    
 names(data)<- c("logc","obs","response")
 
 data<-tidyr::spread(data,obs,response)
 
 colnames(data) <- c("log concentration", "obs_1","obs_2","obs_3","obs_4")
 
 DT::datatable(
   data, options = list(
     pageLength = 5,
     searching = FALSE,
     paging =FALSE)
   )

  })
  
 output$ec50 <- DT::renderDataTable({
   
   fit<-get_fit()
   
   print(fit)
   
   if(is.na(fit$ec50[3,2]) ) fit$ec50[3,2] <- "Did not converge"
   
   DT::datatable(
     fit$ec50, options = list(
       pageLength = 5,
       searching = FALSE,
       paging =FALSE)
   )
   
 })
 
 
 
get_data<-reactive({
  
  data <- readRDS("tdata.RDS")
  
  data <- data[[input$data_set]]
  
  isolate(s<-input$data_rows_selected)
  if(!is.null(s))  data <- data[-s,]
  

  data
  
})  
  
get_fit<-reactive({
  
  data <-get_data()

  fit<-make_fits(data)
})  


})
