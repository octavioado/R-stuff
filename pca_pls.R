#PCA & PLS

library(shiny)
library(xlsx)
library(tidyr)
library(qcc)
library(MASS)
library(ggplot2)

# load data


df<-read.xlsx2('testdata.xls',sheetName = 'Sheet1',stringsAsFactors=FALSE)

# dividing in training and testing and scaling

tf<-df[1:300,]
vf<-df[301:dim(df)[1], ]





shinyUI<-fluidPage(
    
    sidebarPanel(
        
        
        # Header1
        tags$h1("Process modelling with PCA"),
        # Paragraph 1
        p("This application shows an example of PCA and PLS for process monitoring"),
        
        
        # Select algorithm
        selectInput(inputId = "algorithm",
                    label = "Monitoring algorithm",
                    choices = c("PCA", "PLS")),
        
        
        
        # Available variables
        textOutput('Monitoring')
        
    ),
    
    mainPanel(
        
        # Chart plot
        plotOutput(outputId = "plot1"),
        plotOutput(outputId = "plot2")
    )
    
    
)


# PCA
tf<-scale(data.matrix(tf))
vf<-scale(data.matrix(vf))

tf.pca<-prcomp(tf)
s<-summary(tf.pca)$importance

# Calculate number of principal components

pca <- as.numeric(which(s[3,]>0.95)[1])



# Hotelling T2 limits
C <- cov(tf)
E <- eigen(C)
sigma <-  E$values[order(E$values,decreasing = FALSE)][1:pca]
N<-as.numeric(dim(tf)[1])
nvars<-as.numeric(dim(tf)[2])

T2lim <- limits.T2(pca, N, nvars, 0.95)
T2lims<- sqrt(sigma)*qt(0.98, N-pca)


# SPE

zeta1<-0
zeta2<-0
zeta3<-0

for (i in (pca+1):nvars){
    
    zeta1 <- zeta1 + E$values[order(E$values)][i]
    zeta2 <- zeta2 + E$values[order(E$values)][i]^2
    zeta3 <- zeta3 + E$values[order(E$values)][i]^3
    
    
}
    
  

h0 <- 1-(2*zeta1*zeta3)/(3*zeta2^2)
ca <- qnorm(0.95,0,1)
SPElim <- zeta1*(ca*h0*sqrt(2*zeta2)/zeta1+1+zeta2*h0*(h0-1)/zeta1^2)^(1/h0)  



# p <- predict(tf.pca,vf)

# Monitoring new data with PCA
T2indexes<-rep(0,800)
SPEindexes<-rep(0,800)
score<- t(E$vectors[,c(11,10,9,8,7,6,5)]) %*% t(vf)

for(i in 1:dim(vf)[1]){
   
    T2indexes[i] <- vf[i,]%*%(E$vectors[,c(11,10,9,8,7,6,5)])%*%ginv(diag(sigma))%*%t(E$vectors[,c(11,10,9,8,7,6,5)])%*%(vf[i,])
    
    r = (diag(1,nvars) - (E$vectors[,c(11,10,9,8,7,6,5)])%*%t(E$vectors[,c(11,10,9,8,7,6,5)]))%*%(vf[i,])
    SPEindexes[i]<-t(r)%*%r
}



pl<- ggplot(as.data.frame(T2indexes), aes(x =seq(1,length(T2indexes)),y=T2indexes)) +geom_point(colour='black',size=1)

pl<- pl+
    geom_line(aes(x =seq(1,length(T2indexes)), T2lim$control[2], colour = "Control"), data = as.data.frame(T2indexes), lwd = 2) +
    geom_line(aes(x =seq(1,length(T2indexes)), T2lim$prediction[2], colour = "Prediction"), data = as.data.frame(T2indexes), lwd = 2) 

sl<- ggplot(as.data.frame(SPEindexes), aes(x =seq(1,length(SPEindexes)),y=SPEindexes)) +geom_point(colour='black',size=1)

sl<- sl+
    geom_line(aes(x =seq(1,length(SPEindexes)), SPElim, colour = "SPE limit"), data = as.data.frame(SPEindexes), lwd = 2) 


# PLS
library(pls)

# supervised method, thus separating data in to input and quality measures

# training data
X <- tf[,1:9]
Y <- tf[,10:11]

# Testing data

Xt <- vf[,1:9]
Yt <- vf[,10:11]



# APPLY NIPALS PLS regression algorithm

tf.pls <- plsr(Y ~ X,5, method = "oscorespls", model= TRUE)
B <- coef(tf.pls,ncomp = 5)
B <- as.data.frame(B)
ap <- Xt%*%as.matrix(B)
p1<-ggplot(as.data.frame(ap[,1]), aes(x =seq(1,length(ap[,1])),y=ap[,1])) +geom_line(colour='black',lwd=1)+ xlab('TIME')+ ylab('Values')+
    geom_line(aes(x =seq(1,length(Yt[,1])), Yt[,1], colour = "Measured"), data = as.data.frame(Yt[,1]), lwd = 1) 

p2<-ggplot(as.data.frame(ap[,2]), aes(x =seq(1,length(ap[,1])),y=ap[,2])) +geom_line(colour='black',lwd=1)+ xlab('TIME')+ ylab('Values')+
    geom_line(aes(x =seq(1,length(Yt[,2])), Yt[,2], colour = "Measured"), data = as.data.frame(Yt[,2]), lwd = 1) 

shinyServer<- function(input, output) {
    
    load_algorithm <- reactive({
        algo <- input$algorithm
        algo
        
    })
    
    
    output$Monitoring<- renderPrint({
        
        algo<-load_algorithm()
        
        
        print(paste0('Monitoring with ', algo) )})   
    
    
    # Render plot
    output$plot1 <- renderPlot({
        
        
        
        
        # algorithm results
        
        algo<-load_algorithm()
        
        if(algo == 'PCA'){
            
            
            print(pl)
            
            
        } else if(algo == 'PLS'){
            
            
            print(p1)
            
            
        } 
        
        
    })
    
    
    output$plot2 <- renderPlot({
        
        
        
        
        # algorithm results
        
        algo<-load_algorithm()
        
        if(algo == 'PCA'){
            
            
            print(sl)
            
            
        } else if(algo == 'PLS'){
            
            
            print(p2)
            
            
        } 
        
        
    })
    
    
    
    
}


shinyApp(ui=shinyUI,server = shinyServer)
