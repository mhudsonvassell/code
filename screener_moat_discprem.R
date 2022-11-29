#set working directory
setwd("C:/Users/musik/Desktop/Fundamental Stock Picker")


#load required libraries
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(corrplot)


#open full Jan 2010-Dec 2019 .csv file
sp_500=read.csv(file="allfinaltic.csv",header=TRUE)



#replace . with - so it plays nicely with Finviz
sp_500$sym <- gsub(".", "-", sp_500$tic,fixed=TRUE)

sp_500$tic <- NULL

#load XML package
library(XML)

#if one of the stocks creates a problem in the data scrape, this formula helps you remove it
#sp_500 <- sp_500[-c(10,14), ]

#create vector from sp500 symbols
stocks <- unlist(sp_500)


for (s in stocks) {
  url <- paste0("http://finviz.com/quote.ashx?t=", s)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
  # ASSIGN TO STOCK NAMED DFS
  assign(s, readHTMLTable(tableNodes[[8]], 
                          header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                    "data7", "data8", "data9", "data10", "data11", "data12")))
  
  # ADD COLUMN TO IDENTIFY STOCK 
  df <- get(s)
  df['stock'] <- s
  assign(s, df)
}

# COMBINE ALL STOCK DATA 
stockdatalist <- cbind(mget(stocks))
stockdata <- do.call(rbind, stockdatalist)

# MOVE STOCK ID TO FIRST COLUMN
stockdata <- stockdata[, c(ncol(stockdata), 1:ncol(stockdata)-1)]


#create new column that assigns original row numbers 
stockdata$row <- rep(c(1:12), times = 17)


#isolate rows with relevant information into separate tables
debteq <- stockdata[ which(stockdata$row=='10'), ]
SMA <- stockdata[ which(stockdata$row=='12'), ]
price <- stockdata[ which(stockdata$row=='11'), ]

#Further isolate columns so that only relevant data remains
de.data <- subset(debteq,select=c(stock,data4))
SMA20.data <- subset(SMA,select=c(stock,data4))
SMA50.data <- subset(SMA,select=c(stock,data6))
SMA200.data <- subset(SMA,select=c(stock,data8))
price.data <- subset(price,select=c(stock,data12))

#rename columns
names(de.data) <- c("stock", "deratio")
names(SMA20.data) <- c("stock", "sma20")
names(SMA50.data) <- c("stock", "sma50")
names(SMA200.data) <- c("stock", "sma200")
names(price.data) <- c("stock", "price")

#merge tables on stock symbol
data <- merge(de.data,SMA20.data,by=c("stock"))
data <- merge(data,SMA50.data,by=c("stock"))
data <- merge(data,SMA200.data,by=c("stock"))
data <- merge(data,price.data,by=c("stock"))

#change variable types to numerical
data$deratio <- as.numeric(as.character(data$deratio))
data$price <- as.numeric(as.character(data$price))
data$sma20 <- as.numeric(sub("%","",as.character(data$sma20)))/100
data$sma50 <- as.numeric(sub("%","",as.character(data$sma50)))/100
data$sma200 <- as.numeric(sub("%","",as.character(data$sma200)))/100

#save dataset as .csv file
write.table(data,file="pricecheck.csv",quote=FALSE,append=FALSE,sep=",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE)

#create new column that indicates whether long position is warranted
data$position<-ifelse(data$sma50<0,1,
                      ifelse(data$sma50>=0.3,-1,0))

class(data$position)

#create new table that only includes new positions to take, then view positions
positions <- subset(data, position == "1"| position == "-1")
view(positions)

#create template that can be used to insert current date into file name
thedate <- strftime(Sys.Date(),"%y%m%d")
thefile <- paste0("moatpositions",thedate,".csv")

#save dataset as .csv file
write.table(positions,file=thefile,quote=FALSE,append=FALSE,sep=",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE)
