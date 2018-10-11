
### Problem Set 5 by Nikhil Gupta ###

#Question 1

library(lubridate)
library(data.table)
library(tidyr)
library(sandwich)
library(lmtest)

library(xts)
library(moments)
library(qpcR)
library(dplyr)
library(StatMeasures)  # To calculate decile
library(purrr)
library(bindrcpp)
library(Hmisc)


Mkt_Rf = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW5/FF_Factors.csv", header=TRUE, 
                  stringsAsFactors = FALSE)

Mkt_Rf_mon = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW5/FF_Factors_mon.csv", header=TRUE, 
               stringsAsFactors = FALSE)

CRSP = suppressWarnings(fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW5/CRSP.csv", header=TRUE, 
                                stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") ))
CRSP_mon = suppressWarnings(fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW5/CRSP_mon.csv", header=TRUE, 
                              stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") ))


#### Daily Clean 

data_1 <- CRSP[((CRSP$EXCHCD == 1) | (CRSP$EXCHCD == 2) | (CRSP$EXCHCD == 3))  ,]
#data_2 <- data_1[((data_1$SHRCD == 10) | (data_1$SHRCD == 11))  ,]
data_2 <- data_1[order(as.Date(data_1$date, format="%m/%d/%Y")),]
#data_2[is.na(data_2$SHRCD)]

data_2$RET[is.na(as.numeric(data_2$RET))] <- 0
data_2$DLRET[is.na(as.numeric(data_2$DLRET))] <- 0
data_2<- data_2[,fin_ret:= ((1+as.numeric(data_2$DLRET))*(1+as.numeric(data_2$RET)) - 1)]
#data_2 <- data_2[which(mdy(date)>="2012-12-31")]

## Daily vol calculation  Start 

data_2$year = year(mdy(data_2$date))
data_2$month = month(mdy(data_2$date))
data_2$day = day(mdy(data_2$date))
data_2$day <- formatC(data_2$day,width=0,format="d",flag=0)
data_2$month1 <- formatC(data_2$month,width=0,format="d",flag=0)
#a <- sprintf("%2d",data_2$day)
data_2$date1 <- as.numeric(paste(data_2$year,data_2$month1,data_2$day,sep=""))

data_2 <- data_2[,vol:= sd(fin_ret), by=c("year","month","PERMNO")]
data_2 <- data_2[,count:= .N,by=c("year","month","PERMNO")]
data_2 <- data_2[order(PERMNO,year,month),]

## Take data from Dec 2012 to Dec 2015 Start 
data_2013 <- data_2[which(year>=2013)]
data_2012_dec <- data_2[which(year==2012 & month==12)]
data_2 <- rbind(data_2013,data_2012_dec)
data_2 <- data_2[order(PERMNO,year,month),]
## Take data from Dec 2012 to Dec 2015 End 

data_2_sub <- data_2[,c("year","month","PERMNO","vol","count","month1")]
data_2_sub <- unique(data_2_sub)
data_2_sub <- data_2_sub[order(PERMNO,year,month),]

# Lagged Taken 
data_2_sub <- data_2_sub[,lagged_vol:= shift(vol,1)]
data_2_sub <- data_2_sub[,lagged_count:= shift(count,1)]

data_2_sub <- data_2_sub[which(year>=2013)]
data_2_sub <- data_2_sub[which(lagged_count>17)]

#### Daily Clean 

#### Monthly Clean 

data_1_m <- CRSP_mon[((CRSP_mon$EXCHCD == 1) | (CRSP_mon$EXCHCD == 2) | (CRSP_mon$EXCHCD == 3))  ,]
#data_2_m <- data_1_m[((data_1_m$SHRCD == 10) | (data_1_m$SHRCD == 11))  ,]
data_2_m <- data_1_m[order(as.Date(data_1_m$date, format="%m/%d/%Y")),]

#data_2[is.na(data_2$SHRCD)]

data_2_m$RET[is.na(as.numeric(data_2_m$RET))] <- 0
data_2_m$DLRET[is.na(as.numeric(data_2_m$DLRET))] <- 0

#data_2 <- data_2[,lag_price:=shift(PRC,13),by=c("PERMNO")]
data_2_m <- data_2_m[,fin_ret:= ((1+as.numeric(data_2_m$DLRET))*(1+as.numeric(data_2_m$RET)) - 1)]
data_2_m <- data_2_m[,mkt_cap:= abs(( PRC *SHROUT )/ 1000)]    # Mkt_Cap in millions 
data_2_m <- data_2_m[, Lagged_mkt_cap := shift(mkt_cap,1), by=c("PERMNO")]
data_2_m <- data_2_m[,logged_mkt_cap:= log(mkt_cap)]
data_2_m <- data_2_m[!is.na(Lagged_mkt_cap)]

data_2_m <- data_2_m[which(mdy(date)>"2012-12-31")]  
data_2_m$year = year(mdy(data_2_m$date))
data_2_m$month = month(mdy(data_2_m$date))
data_2_m <- data_2_m[order(PERMNO,year,month),]

data_tovol_merge <- merge(data_2_m,data_2_sub,by=c("year","month","PERMNO"))
#data_tovol_merge <- data_tovol_merge[!is.na(lagged_vol)]

GetPortNums <- function(x, numports) {
    as.integer(cut(
        x,
        quantile(x,  probs= 0:numports / numports, na.rm = TRUE),
        include.lowest = TRUE
    ))
}

data_tovol_merge1 <- data_tovol_merge[, tovol_quint := GetPortNums(lagged_vol,5), by=.(year,month)]

data_tovol_merge1 <- data_tovol_merge1[,mkt_cap_sum:= sum(Lagged_mkt_cap),by=c("year","month","tovol_quint")]
data_tovol_merge1 <- data_tovol_merge1[,weights_tovol:= Lagged_mkt_cap/mkt_cap_sum]
data_tovol_merge1 <- data_tovol_merge1[,tovol_ret:= sum((weights_tovol*fin_ret),na.rm=TRUE),by=c("year","month","tovol_quint")]

#data_tovol_merge1 <- data_tovol_merge1[is.na(mkt_cap)]
data_tovol_merge1 <- data_tovol_merge1[,mkt_cap_sum_curr:= sum(mkt_cap,na.rm=TRUE),by=c("year","month","tovol_quint")]
data_tovol_merge1 <- data_tovol_merge1[,mkt_cap_sum_curr_log:= mean(logged_mkt_cap,na.rm=TRUE),by=c("year","month","tovol_quint")]


sub <- data_tovol_merge1[,c("year","month","tovol_quint","tovol_ret")]
sub <- unique(sub)

sub1 <- data_tovol_merge1[,c("year","month","tovol_quint","mkt_cap_sum")]
sub1 <- unique(sub1)

sub2 <- data_tovol_merge1[,c("year","month","tovol_quint","mkt_cap_sum_curr_log")]
sub2 <- unique(sub2)

#mean(sub[which(tovol_quint==5)]$tovol_ret)
#sub <- sub[order(tovol_quint,year,month),]
#mean(sub[which(tovol_quint==5)]$tovol_ret)
#sd(sub[which(tovol_quint==4)]$tovol_ret)



final_1 <- matrix(ncol=9,nrow=5)
final_1[,1] <- seq(1:5)
#ff_me <- data.frame(ff_me)
#total_mkt_cap <- mean(sub[which(tovol_quint==1)]$tovol_ret)

## Check Newey West t-stat 

for (i in 1:5){
    #port_dm_ret1[i] <- mean(data_4[which(Decile==i)]$DM_Ret)*12
    final_1[i,2] <- round(mean(sub[which(tovol_quint==i)]$tovol_ret)*100,2)
    final_1[i,3] <- round(sd(sub[which(tovol_quint==i)]$tovol_ret)*100,2)
    final_1[i,4] <- round(sum(sub1[which(tovol_quint==i)]$mkt_cap_sum)/sum(sub1$mkt_cap_sum)*100,1)
    final_1[i,5] <- round(mean(sub2[which(tovol_quint==i)]$mkt_cap_sum_curr_log),2)
    final_1[i,6] <- round(coef(lm(sub[which(tovol_quint==i)]$tovol_ret ~ Mkt_Rf_mon$`Mkt-RF`))[1]*100,2)
    #final_1[i,7] <-  round(coeftest((lm(sub[which(tovol_quint==i)]$tovol_ret ~  Mkt_Rf_mon$`Mkt-RF`)),vcov=NeweyWest)[1,3],2)
    final_1[i,7] <-  round(coef(summary(lm(sub[which(tovol_quint==i)]$tovol_ret ~  Mkt_Rf_mon$`Mkt-RF`)))[1,3],2)
    
    final_1[i,8] <- round(coef(lm( sub[which(tovol_quint==i)]$tovol_ret  ~ Mkt_Rf_mon$`Mkt-RF` + Mkt_Rf_mon$SMB + Mkt_Rf_mon$HML))[1]*100,2)
    #final_1[i,9] <- round(coeftest((lm(sub[which(tovol_quint==i)]$tovol_ret ~  Mkt_Rf_mon$`Mkt-RF`+Mkt_Rf_mon$SMB + Mkt_Rf_mon$HML)),vcov=NeweyWest)[1,3],2)
    final_1[i,9] <-  round(coef(summary(lm(sub[which(tovol_quint==i)]$tovol_ret ~  Mkt_Rf_mon$`Mkt-RF`+Mkt_Rf_mon$SMB+ Mkt_Rf_mon$HML)))[1,3],2)
    
}

colnames(final_1) <- c("Rank","Mean", "Std_dev","%Mkt_Share","Size","CAPM_Alpha","CAPM_Alpha_t_test","FF3_Alpha","FF3_Alpha_t_test")
View(final_1)

#coeftest((lm(sub[which(tovol_quint==1)]$tovol_ret ~  Mkt_Rf_mon$`Mkt-RF`+Mkt_Rf_mon$SMB + Mkt_Rf_mon$HML)),vcov=NeweyWest)

# Table 2 

## Monthly Clean 

Mkt_Rf$year = as.numeric(substr(Mkt_Rf$Date,1,4))
Mkt_Rf$month = as.numeric(substr(Mkt_Rf$Date,5,6))
Mkt_Rf$date1 = Mkt_Rf$Date

data_2 <- data_2[which(count>17)]

data_2_reg_merge <- merge(data_2,Mkt_Rf,by=c("date1"),all.x=TRUE)
data_2_reg_merge <- data_2_reg_merge[order(PERMNO,date1),]
data_2_reg_merge <- data_2_reg_merge[!is.na(SMB)]
#data_2_reg_merge <- data_2_reg_merge[!is.na(HML)]
#data_2_reg_merge <- data_2_reg_merge[!is.na(`Mkt-RF`)]

#data_2_reg_merge$Mkt_RF1 <- data_2_reg_merge$`Mkt-RF`

#data_3_reg_merge <- data_3_reg_merge[which(mdy(date)>"2012-12-31")]  
#data_3_reg_merge <- data_2_reg_merge[which(year.y>=2013)]
data_3_reg_merge <- data_2_reg_merge[order(PERMNO,date1),]

## Major Step, Regression 
data_check <- data_3_reg_merge[,c(idio_risk1 = as.list(sd(lm(fin_ret ~ `Mkt-RF` + SMB + HML)$residuals))), by=c("year.y","month.y","PERMNO")]
## Major Step, Regression 

data_check <- data_check[order(PERMNO,year.y,month.y),]
data_check <- data_check[,idio_risk1_lagged:= shift(idio_risk1)] 
data_check$year = data_check$year.y
data_check$month = data_check$month.y

data_idvol_merge <- merge(data_2_m,data_check,by=c("year","month","PERMNO"))
data_idvol_merge <- data_idvol_merge[!is.na(idio_risk1_lagged)]

data_idvol_merge<- data_idvol_merge[order(PERMNO,year,month),]


GetPortNums <- function(x, numports) {
    as.integer(cut(
        x,
        quantile(x,  probs= 0:numports / numports, na.rm = TRUE),
        include.lowest = TRUE
    ))
}

data_idvol_merge1 <- data_idvol_merge[, idvol_quint := GetPortNums(idio_risk1_lagged,5), by=.(year,month)]
#data_idvol_merge1 <- data_idvol_merge1[!is.na(Lagged_mkt_cap)]

data_idvol_merge1 <- data_idvol_merge1[,mkt_cap_sum:= sum(Lagged_mkt_cap),by=c("year","month","idvol_quint")]
data_idvol_merge1 <- data_idvol_merge1[,weights_idvol:= Lagged_mkt_cap/mkt_cap_sum]
data_idvol_merge1 <- data_idvol_merge1[,idvol_ret:= sum((weights_idvol*fin_ret),na.rm=TRUE),by=c("year","month","idvol_quint")]

data_idvol_merge1 <- data_idvol_merge1[,mkt_cap_sum_curr:= sum(mkt_cap,na.rm=TRUE),by=c("year","month","idvol_quint")]
data_idvol_merge1 <- data_idvol_merge1[,mkt_cap_sum_curr_log:= mean(logged_mkt_cap,na.rm=TRUE),by=c("year","month","idvol_quint")]


sub3 <- data_idvol_merge1[,c("year","month","idvol_quint","idvol_ret")]
sub3 <- sub3[order(year,month),]
sub3 <- sub3[!duplicated(idvol_ret),]

#mean(sub3[which(idvol_quint==5)]$idvol_ret)

#sub3 <- unique(sub3)
#sub3 <- sub3[order(idvol_quint,year,month),]

sub4 <- data_idvol_merge1[,c("year","month","idvol_quint","mkt_cap_sum")]
sub4 <- unique(sub4)

sub5 <- data_idvol_merge1[,c("year","month","idvol_quint","mkt_cap_sum_curr_log")]
sub5 <- unique(sub5)

#sub1 <- sub1[order(idvol_quint,year,month),]

#mean(sub1[which(idvol_quint==1)]$idvol_ret)

final_2 <- matrix(ncol=9,nrow=5)
final_2[,1] <- seq(1:5)
#ff_me <- data.frame(ff_me)
#total_mkt_cap <- mean(sub[which(tovol_quint==1)]$tovol_ret)

for (i in 1:5){
    #port_dm_ret1[i] <- mean(data_4[which(Decile==i)]$DM_Ret)*12
    final_2[i,2] <- round(mean(sub3[which(idvol_quint==i)]$idvol_ret)*100,2)
    final_2[i,3] <- round(sd(sub3[which(idvol_quint==i)]$idvol_ret)*100,2)
    final_2[i,4] <- round(sum(sub4[which(idvol_quint==i)]$mkt_cap_sum)/sum(sub4$mkt_cap_sum)*100,1)
    final_2[i,5] <- round(mean(sub5[which(idvol_quint==i)]$mkt_cap_sum_curr_log),2)
    
    final_2[i,6] <- round(coef(lm(sub3[which(idvol_quint==i)]$idvol_ret ~ Mkt_Rf_mon$`Mkt-RF`))[1]*100,2)
    #final_2[i,7] <-  round(coeftest((lm(sub3[which(idvol_quint==i)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`)),vcov=NeweyWest)[1,3],2)
    final_2[i,7] <-  round(coef(summary(lm(sub3[which(idvol_quint==i)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`)))[1,3],2)
    final_2[i,8] <- round(coef(lm( sub3[which(idvol_quint==i)]$idvol_ret  ~ Mkt_Rf_mon$`Mkt-RF` + Mkt_Rf_mon$SMB + Mkt_Rf_mon$HML))[1]*100,2)
    #final_2[i,9] <- round(coeftest((lm(sub3[which(idvol_quint==i)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`+Mkt_Rf_mon$SMB + Mkt_Rf_mon$HML)),vcov=NeweyWest)[1,3],2)
    final_2[i,9] <-  round(coef(summary(lm(sub3[which(idvol_quint==i)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`+Mkt_Rf_mon$SMB+ Mkt_Rf_mon$HML)))[1,3],2)
    
}

colnames(final_2) <- c("Rank","Mean", "Std_dev","%Mkt_Share","Size","CAPM_Alpha","CAPM_Alpha_t_test","FF3_Alpha","FF3_Alpha_t_test")
View(final_2)

#coeftest(lm(sub3[which(idvol_quint==5)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`), vcov=NeweyWest)

#round(coef(lm(sub3[which(idvol_quint==3)]$idvol_ret ~ Mkt_Rf_mon$`Mkt-RF`))[1]*100,2)
#coeftest((lm(sub3[which(idvol_quint==2)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`)),vcov=NeweyWest((lm(sub3[which(idvol_quint==2)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`)),lag=1,prewhite = FALSE))
#coef(summary(lm(sub3[which(idvol_quint==2)]$idvol_ret ~  Mkt_Rf_mon$`Mkt-RF`)))

#### END ####################






















