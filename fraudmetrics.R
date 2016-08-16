# Fraud metrics (early prototype)
# Data from DMwR

library(DMwR)
data(sales)
summary(sales)

# reports per sales person
totS <-table(sales$ID)
barplot(totS, main="Transactions per salespeople", names.arg="", xlab="Salespeople", ylab="Amount")

# reports per product

totP <- table(sales$Prod)
barplot(totP, main="Transactions per product", names.arg="", xlab="Products", ylab="Amount", ylim=c(0,4000))

# create unit price
# maybe attach function helps here attach(sales)

sales$Uprice <- sales$Val/sales$Quant

upp <- aggregate(sales$Uprice, list(sales$Prod), median, na.rm=TRUE)

topP <- sapply(c(T,F), function(o)  upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP) <- c('Expensive', 'Cheap')
tops <- sales[sales$Prod %in% topP[1,], c("Prod", "Uprice")]

# sales people and unit price ratio

vsales <- aggregate(sales$Val, list(sales$ID), sum, na.rm=TRUE)
scoresSs <- sapply(c(T,F), function(o)  vsales[order(vsales$x, decreasing=o)[1:5],1])
colnames(scoresSs) <- c("Most", "Least")



# normalized distances of typical prices
# takes the difference of the unit price of that transaction and the overall unit median price of that product
# and divide it by the range of that product prices (maybe could also be done with variance but less robustness??)
 avgNDTP <- function( toInsp, train, stats ) {
       if(missing(train) && missing(stats))
             stop('Provide either the training data or the product stats')
       if(missing(stats)) {
             notF <- which(train$Insp != 'fraud')
             stats <- tapply(train$Uprice[notF],
                                                   list(Prod=train$Prod[notF]),
                                                   function(x) {
                                                         bp <- boxplot.stats(x)$stats
                                                         c(median=bp[3],iqr=bp[4]-bp[2])
                                                       })
             stats <- matrix(unlist(stats),
                                                   length(stats), 2, byrow=T,
                                                   dimnames=list(names(stats), c('median','iqr')))
             stats[which(stats[,'iqr']==0),'iqr'] <- stats[which(stats[,'iqr']==0),'median']
           }
       
           mdtp <- mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) / stats[toInsp$Prod,'iqr'])
           return(mdtp)
         }
