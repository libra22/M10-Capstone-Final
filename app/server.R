library(shiny)
library(tm)

source('model.R')
library(data.table)
load("plist.RData")
load('ngram.RData')

novalue <- "No Input Found"

shinyServer(function(input, output) {
  dataInput <- reactive({
    if(input$radio == 1){
      predictSBO(input$entry,plist,unigramDT,bigramDT,trigramDT) #substitute your own input here
    }else{
      predictSGT(input$entry,plist,unigramDT,bigramDT,trigramDT) #substitute your own input here
    }
  })
  
  output$text <- renderText({
    dataInput()
  })
  
  output$sent <- renderText({
    if (nchar(input$entry) != 0) {
      input$entry
    } else
    {
      "no input entered!"
    }
  })
})