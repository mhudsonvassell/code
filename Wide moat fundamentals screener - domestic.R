#set working directory
setwd("C:/Users/Michael/Documents/MSF Files/Backtest Resources")

#open full Jan 2010-Dec 2019 .csv file
Entire=read.csv(file="gfundamentals.csv",header=TRUE)

#load packages
library(dplyr)
library(fastmatch)
library(data.table)
library(rvest)

#disable scientific notation
options(scipen = 999)

#create calculated columns for gross profit and total debt
Entire$gp <- with(Entire, revt-cogs)
Entire$dt <- with(Entire, dlc+dltt)

#rename columns
x=rename(Entire, tot.asts=at
       , cash=che
       , ltd=dltt
       , tot.inv=invt
       , tot.liab=lt
       , treas=tstk
       , receiv=rect
       , ni=nicon
       , depr=dp
       , op.inc=oiadp
       , tot.rev=revt
       , int.exp=xint
       , sga=xsga
       , cf.inv=ivncf
       , shares=cshoi
       , tot.debt=dt
       , eq=teq)

#create calculated columns for derived metrics
x$gpmargin <- with(x, gp/tot.rev)
x$sga.gp <- with(x, sga/gp)
x$depr.gp <- with(x, depr/gp)
x$intexp.opinc <- with(x, int.exp/op.inc)
x$ni.totrev <- with(x, ni/tot.rev)
x$eps <- with(x, ni/shares)
x$receiv.totrev <- with(x, receiv/tot.rev)
x$ni.totasts <- with(x, ni/tot.asts)
x$ltdpay <- with(x, ltd/ni)
x$adj.de <- with(x, tot.liab/(eq+treas))
x$adj.roe <- with(x, ni/(eq+treas))
x$capx.ni <- with(x, capx/ni)

#sort x dataset by gvkey isin and fyear
sortx <- x[
  with(x, order(gvkey, isin, fyear)),
  ]

#remove unnecessary columns
#find column number
#fmatch("prstkcc",names(sortx))
#remove columns by number
#sortx = subset(sortx, select = -c(24) )

#remove missing data
#sortx <- na.omit(sortx)

#remove entries with values that cause problems with derived metric means for industries
new <- subset(sortx, tot.rev != 0 & gp != 0 & (eq+treas) !=0) 

sortx <- subset(sortx, tot.rev != 0 & gp != 0)

#isolate retained earnings variable
#re <- subset(sortx, select=c("gvkey","isin","fyear","re"))

#save re table as .csv file
#write.table(re,file="globalre.csv",quote=FALSE,append=FALSE,sep=",",
#eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE)

#bring in retained earnings data for companies with a possible dca, as indicated by 
#an increasing trend in retained earnings from 2010 to 2018
dca.re=read.csv(file="globalreplus.csv",header=TRUE)

dca.re <- na.omit(dca.re)

#create new columns indicating whether retained earnings increases from year to year
dca.re$plus2011 <- ifelse(dca.re$X2011>dca.re$X2010,1,0)
dca.re$plus2012 <- ifelse(dca.re$X2012>dca.re$X2011,1,0)
dca.re$plus2013 <- ifelse(dca.re$X2013>dca.re$X2012,1,0)
dca.re$plus2014 <- ifelse(dca.re$X2014>dca.re$X2013,1,0)
dca.re$plus2015 <- ifelse(dca.re$X2015>dca.re$X2014,1,0)
dca.re$plus2016 <- ifelse(dca.re$X2016>dca.re$X2015,1,0)
dca.re$plus2017 <- ifelse(dca.re$X2017>dca.re$X2016,1,0)
dca.re$plus2018 <- ifelse(dca.re$X2018>dca.re$X2017,1,0)

#create column that gives total years with increasing retained earnings
dca.re$plustotal <- with(dca.re, plus2011+plus2012+plus2013+plus2014+plus2015+plus2016
                         +plus2017+plus2018 )
#calculate CAGR of retained earnings for date range where most companies have data
dca.re$re.cagr <- with(dca.re, ((X2017-X2011)+abs(X2011))/abs(X2011)^(1/6)-1)

#identify companies with 5 or more years of increasing retained earnings
dca.re$trend <- ifelse(dca.re$plustotal>=5,1,0)

#rename gvkey column
names(dca.re)[1] <- "gvkey"

#pull out gvkey and isin for companies that meet criteria
re.trend <- subset(dca.re, trend=="1",select=c("gvkey","isin","re.cagr"))

#sort companies by growth in retained earnings
re.cagr <- re.trend[
  with(re.trend, order(re.cagr, decreasing=TRUE)),
  ]

#merge dca.re list with sortx list
re.x <- merge(sortx,re.trend,by=c("gvkey", "isin"))

re.x <- unique(re.x, by=c("gvkey","isin","fyear"))

#calculate industry medians for derived metrics
ind <- summarise(group_by_at(new, vars(gind,fyear)),sum(ni,na.rm=TRUE),median(gpmargin,na.rm=TRUE)
                 ,median(sga.gp,na.rm=TRUE),median(adj.de,na.rm=TRUE),median(adj.roe,na.rm=TRUE))

#calculate subindustry medians for derived metrics
subind <- summarise(group_by_at(new, vars(gsubind,fyear)), sum(ni,na.rm=TRUE),median(gpmargin,na.rm=TRUE)
                    ,median(sga.gp,na.rm=TRUE),median(adj.de,na.rm=TRUE),median(adj.roe,na.rm=TRUE))

#merge ind dataset with re.x dataset
dataind <- merge(re.x,ind,by=c("gind", "fyear"))

#merge subind dataset with re.x dataset
datasubind <- merge(re.x,subind,by=c("gsubind", "fyear"))

#change column names to remove parentheses
datasubind=rename(datasubind, median.gpmargin="median(gpmargin, na.rm = TRUE)"
                  , median.sgagp="median(sga.gp, na.rm = TRUE)"
                  , median.adjde="median(adj.de, na.rm = TRUE)"
                  , sum.ni="sum(ni, na.rm = TRUE)"
                  , median.adjroe="median(adj.roe, na.rm = TRUE)")


dataind=rename(dataind, median.gpmargin="median(gpmargin, na.rm = TRUE)"
               , median.sgagp="median(sga.gp, na.rm = TRUE)"
               , median.adjde="median(adj.de, na.rm = TRUE)"
               , sum.ni="sum(ni, na.rm = TRUE)"
               , median.adjroe="median(adj.roe, na.rm = TRUE)")

#create new columns that compare company values to industry average
dataind$gpmargin.dif <- with(dataind, round(((gpmargin-median.gpmargin+abs(median.gpmargin))/abs(median.gpmargin))-1, digits=3))
dataind$sgagp.dif <- with(dataind, round(((sga.gp-median.sgagp+abs(median.sgagp))/abs(median.sgagp))-1, digits=3))
dataind$adjde.dif <- with(dataind, round(((adj.de-median.adjde+abs(median.adjde))/abs(median.adjde))-1, digits=3))
dataind$adjroe.dif <- with(dataind, round(((adj.roe-median.adjroe+abs(median.adjroe))/abs(median.adjroe))-1, digits=3))
dataind$ni.share <- with(dataind, round(ni/sum.ni,digits=3))

datasubind$gpmargin.dif <- with(datasubind, round(((gpmargin-median.gpmargin+abs(median.gpmargin))/abs(median.gpmargin))-1, digits=3))
datasubind$sgagp.dif <- with(datasubind, round(((sga.gp-median.sgagp+abs(median.sgagp))/abs(median.sgagp))-1, digits=3))
datasubind$adjde.dif <- with(datasubind, round(((adj.de-median.adjde+abs(median.adjde))/abs(median.adjde))-1, digits=3))
datasubind$adjroe.dif <- with(datasubind, round(((adj.roe-median.adjroe+abs(median.adjroe))/abs(median.adjroe))-1, digits=3))
datasubind$ni.share <- with(datasubind, round(ni/sum.ni,digits=3))

#create new columns that indicate whether company passed gpmargin, sga, ltdpay, adj.roe, capx.ni, and adj.de filters
dataind$dca.gpmargin<-ifelse(dataind$gpmargin>=0.4,1,0)
dataind$dca.sgagp<-ifelse(dataind$sga.gp<=0.3,1,0)
dataind$dca.ltdpay<-ifelse(dataind$ltdpay<=4 & dataind$ltdpay>=0,1,0)
dataind$dca.adjroe<-ifelse(dataind$adj.roe>=0.2,1,0)
dataind$dca.capxni<-ifelse(dataind$capx.ni<=0.35,1,0)
dataind$dca.adjde<-ifelse(dataind$adj.de<=1.5,1,0)
dataind$dca.nishare<-ifelse(dataind$ni.share>=.5,1,0)

datasubind$dca.gpmargin<-ifelse(datasubind$gpmargin>=0.4,1,0)
datasubind$dca.sgagp<-ifelse(datasubind$sga.gp<=0.3,1,0)
datasubind$dca.ltdpay<-ifelse(datasubind$ltdpay<=4 & datasubind$ltdpay>=0,1,0)
datasubind$dca.adjroe<-ifelse(datasubind$adj.roe>=0.2,1,0)
datasubind$dca.capxni<-ifelse(datasubind$capx.ni<=0.35,1,0)
datasubind$dca.adjde<-ifelse(datasubind$adj.de<=1.5,1,0)
datasubind$dca.nishare<-ifelse(datasubind$ni.share>=.5,1,0)

#create variable that adds up scores on dca filters for derived metrics
dataind$dca.total <- with(dataind, dca.gpmargin+dca.sgagp+dca.ltdpay
                          +dca.adjroe+dca.capxni+dca.adjde+dca.nishare, digits=3)

datasubind$dca.total <- with(datasubind, dca.gpmargin+dca.sgagp+dca.ltdpay
                             +dca.adjroe+dca.capxni+dca.adjde+dca.nishare, digits=3)

#create variable that identifies companies which pass majority of filters
dataind$dca.pre <- ifelse(dataind$dca.total>=5,1,0)
datasubind$dca.pre <- ifelse(datasubind$dca.total>=5,1,0)

#create new table "dca" comprised of only companies that passed initial filter
dcaind <- subset(dataind, dca.pre == "1")

dcasubind <- subset(datasubind, dca.pre == "1")

#sort dca by the gvkey, isin, and year
dcaind <- dcaind[
  with(dcaind, order(gvkey, isin, fyear)),
  ]
dcasubind <- dcasubind[
  with(dcasubind, order(gvkey, isin, fyear)),
  ]

#indicate how many times a isin symbol occurs in dca datasets
dcaind$occurrences <- sequence(rle(as.character(dcaind$isin))$lengths)

dcasubind$occurrences <- sequence(rle(as.character(dcasubind$isin))$lengths)

#indicate when occurrences vector has value of 4 or greater
dcaind$trend <- ifelse(dcaind$occurrences>=4,1,0)

dcasubind$trend <- ifelse(dcasubind$occurrences>=4,1,0)

#create list of isin symbols that occur 4x or more
indtrend <- unique(subset(dcaind, trend == "1", select=c(gvkey,isin)))

subindtrend <- unique(subset(dcasubind, trend == "1", select=c(gvkey,isin)))

#merge "trend" dataset to only include companies with a probable durable competitive advantage
dca2ind <- merge(dcaind,indtrend,by=c("gvkey", "isin"))

dca2subind <- merge(dcasubind,subindtrend,by=c("gvkey", "isin"))

#sort dca2 datasets by gvkey isin and fyear
dca2ind <- dca2ind[
  with(dca2ind, order(gvkey, isin, fyear)),
  ]

dca2subind <- dca2subind[
  with(dca2subind, order(gvkey, isin, fyear)),
  ]

#pull out unique isin numbers
indpreisin <- unique(subset(dca2ind, select=c(isin)))
subindpreisin <- unique(subset(dca2subind, select=c(isin)))


#remove unnecessary columns and rearrange
data2ind <- subset(dca2ind, select=c(gvkey,isin,fyear,gind,ni,gpmargin,sga.gp,adj.de:sum.ni,gpmargin.dif:ni.share))
data2subind <- subset(dca2subind, select=c(gvkey,isin,fyear,gsubind,ni,gpmargin,sga.gp,adj.de:sum.ni,gpmargin.dif:ni.share))

#classify performance on metrics based on large advantages compared to industry mean
data2ind$adjde.adv <- ifelse(data2ind$adjde.dif<=-0.3,1,0)
data2ind$adjroe.adv <- ifelse(data2ind$adjroe.dif>=0.3,1,0)
data2ind$sgagp.adv <- ifelse(data2ind$sgagp.dif<=-0.3,1,0)
data2ind$gpmargin.adv <- ifelse(data2ind$gpmargin.dif>=0.3,1,0)
data2ind$nishare.adv <- ifelse(data2ind$ni.share>=0.5,1,
                               ifelse(data2ind$sum.ni<0 & data2ind$ni>0,1, 0))

data2subind$adjde.adv <- ifelse(data2subind$adjde.dif<=-0.3,1,0)
data2subind$adjroe.adv <- ifelse(data2subind$adjroe.dif>=0.3,1,0)
data2subind$sgagp.adv <- ifelse(data2subind$sgagp.dif<=-0.3,1,0)
data2subind$gpmargin.adv <- ifelse(data2subind$gpmargin.dif>=0.3,1,0)
data2subind$nishare.adv <- ifelse(data2subind$ni.share>=0.5,1,
                                  ifelse(data2subind$sum.ni<0 & data2subind$ni>0,1,0))


#create total performance classification scale that accounts for metrics
data2ind$totadv <- with(data2ind, adjde.adv+adjroe.adv+sgagp.adv+gpmargin.adv+nishare.adv)
data2subind$totadv <- with(data2subind, adjde.adv+adjroe.adv+sgagp.adv+gpmargin.adv+nishare.adv)

#indicate when totadv vector has value of 4 or greater
data2ind$adv <- ifelse(data2ind$totadv>=3,1,0)
data2subind$adv <- ifelse(data2subind$totadv>=3,1,0)

#keep data from years where totadv is 4 or greater
indadv <- unique(subset(data2ind, adv == "1"))
subindadv <- unique(subset(data2subind, adv == "1"))

#indicate how many times an isin symbol occurs in dca dataset
indadv$occurrences <- sequence(rle(as.character(indadv$isin))$lengths)
subindadv$occurrences <- sequence(rle(as.character(subindadv$isin))$lengths)

#indicate when occurrences vector has value of 4 or greater
indadv$trend <- ifelse(indadv$occurrences>=4,1,0)
subindadv$trend <- ifelse(subindadv$occurrences>=4,1,0)

#create list of isin symbols that occur 4x or more
indadvtrend <- unique(subset(indadv, trend == "1" , select=c(gvkey,isin,fyear)))
subindadvtrend <- unique(subset(subindadv, trend == "1", select=c(gvkey,isin,fyear)))

#only include isin for companies that met criteria in recent years
indfinalisin <- unique(subset(indadvtrend, subset = fyear %in% c("2016","2017","2018","2019"), select=c(isin)))
subindfinalisin <- unique(subset(subindadvtrend, subset = fyear %in% c("2016","2017","2018","2019"), select=c(isin)))


#bring in isin-country key
isin_country=read.csv(file="isin_country.csv", header=TRUE)

#bring in cur-usd conversion table
cur_usd=read.csv(file="cur-usd.csv", header=TRUE)

#rename curcd column
names(cur_usd)[1] <- "curcd"

#include only relevant information
indni <- subset(dataind, select=c("gvkey","isin","fyear","gind","ni","curcd"))
subindni <- subset(datasubind, select=c("gvkey","isin","fyear","gsubind","ni","curcd"))

#merge in currency-to-USD conversion multiples
indni <- merge(indni,cur_usd,by=c("curcd"))
subindni <- merge(subindni,cur_usd,by=c("curcd"))

#create new column that converts ni to USD
indni$ni.usd <- with(indni,ni*cur.usd)
subindni$ni.usd <- with(subindni,ni*cur.usd)

#create separate column that isolates first two characters of isin
indni$code <- substr(indni$isin,1,2)
subindni$code <- substr(subindni$isin,1,2)

#merge on ccode column to bring in countries
indni <- merge(indni,isin_country,by=c("code"))
subindni<- merge(subindni,isin_country,by=c("code"))

#rename country column
names(indni)[10] <- "country"
names(subindni)[10] <- "country"

#find leaders for net income within each industry and subindustry
indbestni <- indni %>%
  group_by(gind,fyear) %>%
  top_n(n = 5, wt = ni.usd)

indbestni <- indbestni %>%
  group_by(gind,fyear) %>%
  mutate(good_ranks = order(order(ni.usd, decreasing=TRUE)))

indbestni <- indbestni[
  with(indbestni, order(gind, fyear, good_ranks)),
  ]

subindbestni <- subindni %>%
  group_by(gsubind,fyear) %>%
  top_n(n = 3, wt = ni.usd)

subindbestni <- subindbestni %>%
  group_by(gsubind,fyear) %>%
  mutate(good_ranks = order(order(ni.usd, decreasing=TRUE)))

subindbestni <- subindbestni[
  with(subindbestni, order(gsubind,fyear, good_ranks)),
  ]

#merge finalisin and bestni lists to only include companies among the highest net income
#for each industry and subindustry
dcaindbestni <- merge(indbestni,indfinalisin,by=c("isin"))
dcasubindbestni <- merge(subindbestni,subindfinalisin,by=c("isin"))

#sort lists to see yearly ni progress for each company
dcaindbestni <- dcaindbestni[
  with(dcaindbestni, order(gvkey, isin, fyear)),
  ]

dcasubindbestni <- dcasubindbestni[
  with(dcasubindbestni, order(gvkey, isin, fyear)),
  ]

#indicate how many times an isin symbol occurs in dca dataset
dcaindbestni$occurrences <- sequence(rle(as.character(dcaindbestni$isin))$lengths)
dcasubindbestni$occurrences <- sequence(rle(as.character(dcasubindbestni$isin))$lengths)

#indicate when occurrences vector has value of 3 or greater
dcaindbestni$trend <- ifelse(dcaindbestni$occurrences>=3,1,0)
dcasubindbestni$trend <- ifelse(dcasubindbestni$occurrences>=3,1,0)

#create list of isin symbols that occur 3x or more
dcaindbestni <- unique(subset(dcaindbestni, trend == "1" , select=c(gvkey,isin,fyear,country,good_ranks)))
dcasubindbestni <- unique(subset(dcasubindbestni, trend == "1", select=c(gvkey,isin,fyear,country,good_ranks)))

#only include isin for companies that met criteria in recent years
indfinalisin <- unique(subset(dcaindbestni, subset = fyear %in% c("2016","2017","2018","2019"), select=c(isin)))
subindfinalisin <- unique(subset(dcasubindbestni, subset = fyear %in% c("2016","2017","2018","2019"), select=c(isin)))


#get list of isin symbols from each list
indfinalisin <- unique(subset(indfinalisin, select=c(isin)))
subindfinalisin <- unique(subset(subindfinalisin, select=c(isin)))

#combine lists
mergefinalisin <- rbind(indfinalisin, subindfinalisin)

#include only unique isin symbols
allfinalisin <- unique(mergefinalisin)

#merge allfinalisin list with sortx list to bring in company names
globalfinalisin <- merge(allfinalisin,sortx,by=c("isin"))

#keep only isin and company name, remove duplicates
globalfinalisin <- unique(subset(globalfinalisin,select=c(isin,conm)))

#save final isin list as .csv file
write.table(globalfinalisin,file="globalallfinalisin.csv",quote=FALSE,append=FALSE,sep=",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,col.names = TRUE)



#------------------------END OF SCRIPT-------------------------------------------------

