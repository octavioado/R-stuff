## Twilo app using SHINY and twilo API
# 

library(shiny)
library(jsonlite)
library(ggplot2)
library(XML)
library(httr)
library(rjson)
library(RCurl)


# shiny app
# UI
shinyUI<- fluidPage(    
        
        textInput(inputId = 'char',label = 'Write message',value = 'Please write your message here'),
        textInput(inputId = 'num', label = 'Write number', value = 'Write number to send message'),
        submitButton(text = 'send message'),
        
        
        mainPanel(
            h4("Message"),
            textOutput("message")
            
        )

     
      
    )


# Server
shinyServer<-function(input, output) {
    
    output$message<- renderPrint({print('Message send')
        
        
        # Twilio info(id, token number)
        
        turl <- 'https://api.twilio.com/2010-04-01/Accounts/'
        aid  <- 'user id here'
        atk  <- 'authentication token here'
        twn  <- 'twilio number here' 
        pswd <- paste0(aid,':',atk)
        sms  <- 'Test this program rocks'        
        rurl<- paste0(turl,aid,'/Messages')
        
    POST(rurl,body = list(From = twn, To = input$num, Body = input$char), config = authenticate(aid,atk,type = 'basic'))
            })

    
    
}


shinyApp(ui=shinyUI,server = shinyServer)
