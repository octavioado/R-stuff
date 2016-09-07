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
        aid  <- 'AC44e7a369b9c75945fd0038be2a8d1cc5'
        atk  <- 'a2d2cbcc2c192e38ac171bbcffd37cd1'
        twn  <- '+37259122176' # twilio number
       # myn  <- '+37257878028' # number that receives the message
        pswd <- paste0(aid,':',atk)
        sms  <- 'Test this program rocks'        
        rurl<- paste0(turl,aid,'/Messages')
        
    POST(rurl,body = list(From = twn, To = input$num, Body = input$char), config = authenticate(aid,atk,type = 'basic'))
            })

    
    
}


shinyApp(ui=shinyUI,server = shinyServer)

#  post message to your phone first using httr

# getURL(rurl,userpwd=pswd)

# POST(rurl,body = list(From = twn, To = myn, Body = sms), config = authenticate(aid,atk,type = 'basic'))

# now with curl (works but i get a weird error message)

#postForm(rurl,
#         .opts = list(userpwd = pswd, useragent = "RCurl", verbose = TRUE),
#         .params = c(From = twn, To = myn, Body = paste(sms, 'with CURL'))
#        )

lurl<-paste0('https://lookups.twilio.com/v1/PhoneNumbers/myn')
x<-fromJSON(getURL(lurl,userpwd=pswd))