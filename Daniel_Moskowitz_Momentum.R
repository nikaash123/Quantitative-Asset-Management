
### Problem Set 3 by Nikhil Gupta ###

#Question 1

library(lubridate)
library(data.table)
library(tidyr)

library(xts)
library(moments)
library(qpcR)
library(dplyr)
library(StatMeasures)  # To calculate decile
library(purrr)
library(bindrcpp)
library(Hmisc)

data_1 = suppressWarnings(fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/Full_Data.csv", header=TRUE, 
                  stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") ))

Mkt_Rf = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/FF.csv", header=TRUE, 
              stringsAsFactors = FALSE )

KRF_mom = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/KRF_Momentum.csv", header=TRUE, 
                stringsAsFactors = FALSE )

# Read file from Daniel Website#
dir.create("dmport")
untar('C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/DM_data_2017_03.tar.gz', exdir = "dmport") 
list.files("dmport")
dm_port_mon <- fread("dmport/m_m_pt_tot.txt")

PS3_Q1 <- function(data_1){

# Take only exchange codes and share codes 
data_2 <- data_1[((data_1$EXCHCD == 1) | (data_1$EXCHCD == 2) | (data_1$EXCHCD == 3))  ,]
data_2 <- data_2[((data_2$SHRCD == 10) | (data_2$SHRCD == 11))  ,]

#Making Returns = 0

#data_2[is.na(data_2)] = 0

data_2 <- data_2[order(as.Date(data_2$date, format="%m/%d/%Y")),]
#data_2[is.na(data_2$SHRCD)]

data_2$RET[is.na(as.numeric(data_2$RET))] <- 0
data_2$DLRET[is.na(as.numeric(data_2$DLRET))] <- 0

data_2 <- data_2[,lag_price:=shift(PRC,13),by=c("PERMNO")]
data_2 <- data_2[,fin_ret:= ((1+as.numeric(data_2$DLRET))*(1+as.numeric(data_2$RET)) - 1)]
data_2 <- data_2[,mkt_cap:= abs(( PRC *SHROUT )/ 1000)]    # Mkt_Cap in millions 
data_2 <- data_2[,log_stockreturn := log(1+fin_ret)]
data_2 <- data_2[, Lagged_mkt_cap := shift(mkt_cap,1), by=c("PERMNO")]

data_2 <- data_2[, lagged_2_prc_ret := shift(fin_ret,2),by=c("PERMNO")]


#data_2 <- data_2[,cumret:= sum(shift(log_stockreturn,(2:12)))]

data_2 <- data_2[,cumret := (shift(log_stockreturn,2)+shift(log_stockreturn,3)+shift(log_stockreturn,4)+shift(log_stockreturn,5)+shift(log_stockreturn,6)
                           +shift(log_stockreturn,7)+shift(log_stockreturn,8)+shift(log_stockreturn,9)+shift(log_stockreturn,10)+shift(log_stockreturn,11)+shift(log_stockreturn,12)),by=c("PERMNO")]

#### Removing min 8 returns
cols = c("v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12")
anscols = paste("lag", cols, sep="_")
data_2 <- suppressWarnings(data_2[, (anscols) := shift(log_stockreturn, 2:12, "lag"), by=c("PERMNO")])
#check4 <- check4[complete.cases(check4)]
#data_2[is.na(data_2)] <- 0


data_2 <- suppressWarnings(data_2[,count1:= sum(abs(lag_v2)>0, abs(lag_v3)>0,abs(lag_v4)>0,abs(lag_v5)>0,abs(lag_v6)>0,abs(lag_v7)>0,
                               abs(lag_v8)>0,abs(lag_v9)>0,abs(lag_v10)>0,abs(lag_v11)>0,abs(lag_v12)>0), by=.(date,PERMNO)])


data_2 <- data_2[which(data_2$count1>=8)]
data_2 <- data_2[,ranking_ret := ifelse( ((data_2$lag_price ==0 )| (data_2$lagged_2_prc_ret==0) ),0,data_2$cumret)]

a <- unique(data_2$date)

#data_2 <- data_2[,ranking_ret := ifelse( ((data_2$PRC==0) |(data_2$lag_price ==0 )| (data_2$lagged_2_prc_ret==0) ),0,data_2$cumret)]
data_2 <- data_2[which(data_2$date %in% a[2:1093])]
data_2[is.na(data_2)] <- 0
data_2 <- data_2[!which(data_2$ranking_ret == 0)]
data_2 <- data_2[!which(data_2$Lagged_mkt_cap==0)]

year = year(mdy(data_2$date))
month = month(mdy(data_2$date))

data_final <- cbind(year,month,data_2[,c("date","PERMNO","EXCHCD","Lagged_mkt_cap","fin_ret","ranking_ret")])
return(data_final)

}

data_2 <- PS3_Q1(data_1)


#write.csv(test,file="C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/test.csv")


# Question 2

PS3_Q2 <- function(data_2){
    

data_3 <- data_2[!which(data_2$ranking_ret == 0)]
data_3 <- data_3[!which(data_3$Lagged_mkt_cap==0)]

### DM Deciles ####

data_4 <- data.table(data_3 %>% split(.$date) %>% map_df(~ mutate(., DM_decile = decile(ranking_ret,decreasing=FALSE))))
data_4 <- data_4[order(as.Date(data_4$date, format="%m/%d/%Y")),]

data_5 <- data.table(data_4 %>% split(.$date) %>% map_df(~mutate(.,nTileNYSE = ifelse(EXCHCD == 1, decile(ranking_ret,decreasing=FALSE), NA))))
data_5 <- data_5[order(as.Date(data_5$date, format="%m/%d/%Y")),]

### KRF Deciles ####
GetPortNumsKRF <- function(x,y) {
    breakpoints <- as.numeric(unique(quantile(x, probs=0:10/10, na.rm = TRUE)))
    
    as.integer(cut(y,
                   breakpoints,
                   include.lowest=TRUE))
}

data_5 <- data_5[,KRF_decile:=mapply(GetPortNumsKRF,.(ranking_ret[which(EXCHCD==1)]),.(ranking_ret)),by=(date)]
data_5 <- data_5[,KRF_decile1:= ifelse(is.na(data_5$KRF_decile),DM_decile,KRF_decile)]

data_final <- data_5[,c("year","month","date","PERMNO","Lagged_mkt_cap","fin_ret","DM_decile","KRF_decile1")]
return(data_final)


}

data_3 <- PS3_Q2(data_2)
#data_5[is.na(data_5$KRF_decile)]

#### KRF Deciles 

# Question 3

# DM Return

PS3_Q3 <- function(data_3){

#data_6 <- data_3    
data_6 <- data_3[,mkt_cap_sum:= sum(Lagged_mkt_cap),by=c("date","DM_decile")]
data_6 <- data_6[,weights:= Lagged_mkt_cap/mkt_cap_sum]
data_6 <- data_6[,DM_ret:= sum((weights*fin_ret)),by=c("date","DM_decile")]

# KRF Return

data_7 <- data_6[,mkt_cap_sum_krf:= sum(Lagged_mkt_cap),by=c("date","KRF_decile1")]
data_7 <- data_7[,weights_krf:= Lagged_mkt_cap/mkt_cap_sum_krf]
data_7 <- data_7[,KRF_ret:= sum((weights_krf*fin_ret),na.rm=TRUE),by=c("date","KRF_decile1")]

year = unique(data_7$year)
month = unique(data_7$month)
decile = seq(1,10,1)
data_8 <- matrix(nrow=10920,ncol=6)

p =1 
for (i in 1:91){
    for (j in 1:12){
        for (k in 1:10){
        data_8[p,(1:3)] <- c(year[i],month[j],decile[k])
        p=p+1
      }
    }
}
data_8 <- data.table(data_8)

sub <- data_7[,c("date","DM_decile","DM_ret")]
sub <- unique(sub)

DM_ret <- sub[with(sub, order(mdy(date), DM_decile)),]

sub1 <- data_7[,c("date","KRF_decile1","KRF_ret")]
sub1 <- unique(sub1)
KRF_ret <- sub1[with(sub1, order(mdy(date), KRF_decile1)),]

data_8[,4] = DM_ret[,3]
data_8[,5] = KRF_ret[,3]

dt <- Mkt_Rf$RF[7:1098]
dt1 <- data.table(rep(dt,each=10))

data_8[,6] = dt1$V1/100

colnames(data_8) <- c("Year","Month","Decile","DM_Ret", "KRF_Ret","Rf")
return(data_8)

}

data_4 <- PS3_Q3(data_3)

# Question 4

PS3_Q4 <- function(data_4){
#data_8[,avg_dm:= sum(DM_ret),by=c('Decile')]


final <- matrix(ncol=11,nrow=4)

#data_4 <- data.frame(data_4)



for (i in 1:10){
    #port_dm_ret1[i] <- mean(data_4[which(Decile==i)]$DM_Ret)*12
    final[1,i] <- mean(data_4[which(data_4$Decile==i)]$DM_Ret[1:1035]- data_4[which(data_4$Decile==i)]$Rf[1:1035])*12
    final[2,i] <- sd(data_4[which(Decile==i)]$DM_Ret[1:1035]- data_4[which(Decile==i)]$Rf[1:1035])*sqrt(12)
    final[3,i] <- final[1,i]/final[2,i]
    final[4,i] <- skewness(log(1+data_4[which(Decile==i)]$DM_Ret[1:1035]))
   
    #port_krf[i] <- mean(data_4[which(Decile==i)]$KRF_Ret- data_4[which(Decile==i)]$Rf)
}

check <- data_4[(unique(data_4$Rf))]

final[1,11] = mean(data_4[which(Decile==10)]$DM_Ret[1:1035]- data_4[which(Decile==1)]$DM_Ret[1:1035])*12
final[2,11] = sd(data_4[which(Decile==10)]$DM_Ret[1:1035] - data_4[which(Decile==1)]$DM_Ret[1:1035])*sqrt(12)
final[3,11] = final[1,11]/final[2,11]
final[4,11] =  skewness(log(1+(data_4[which(Decile==10)]$DM_Ret[1:1035] - data_4[which(Decile==1)]$DM_Ret[1:1035])+data_4[which(Decile==10)]$Rf[1:1035]))

colnames(final) <- c("1","2","3","4","5","6","7","8","9","10","WML")
rownames(final) <- c("Annualised Excess Mean", "Annualised Excess Volatility", "Annualised SR", "Skewness(Monthly)")
final
return(final)

}

data_5 <- PS3_Q4(data_4)

#write.csv(check_this,file="C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/check_this.csv")

#plot(loser$DM_Ret,type='l',ylim=c(-3,10))
#lines(winner$DM_Ret,col='red')

# Question 5

PS3_Q5 <- function(data_4,KRF_mom,dm_port_mon){


final <- matrix(ncol=11, nrow=2)    
# Correlation between DM Estimated and DM Actual 

for (i in 1:10){
final[1,i] <- round(cor(dm_port_mon[which(V2==i)]$V3,data_4[which(Decile==i)]$DM_Ret[1:1080]),4)
}


KRF_mom <- data.frame(KRF_mom)

for (i in 1:10){
    final[2,i] <- round(cor(data_4[which(Decile==i)]$KRF_Ret,KRF_mom[,(i+1)]),4)
}

# Winners- Losers 

final[1,11] <- round(cor( (dm_port_mon[which(V2==10)]$V3-dm_port_mon[which(V2==1)]$V3),(data_4[which(Decile==10)]$DM_Ret[1:1080]- data_4[which(Decile==1)]$DM_Ret[1:1080])),4)
final[2,11] <- round(cor( (data_4[which(Decile==10)]$KRF_Ret - data_4[which(Decile==1)]$KRF_Ret) , (KRF_mom[,(11)] - KRF_mom[,(2)]) ),4)

colnames(final) <- c("1","2","3","4","5","6","7","8","9","10","WML") 
rownames(final) <- c("DM_Correlation", "KRF_Correlation")

return(final)
}

data_6 <- PS3_Q5(data_4,KRF_mom,dm_port_mon)

# Question 6

data_10 <- data_1[order(as.Date(data_1$date, format="%m/%d/%Y")),]
a <- unique(mdy(data_10$date))[986:1105]
plot(a,cumsum(log(1+data_4[which(Decile==10)]$DM_Ret[973:1092])),type='l',ylim=c(-2,2),ylab="Cum Log Returns",xlab="Year",main="Winners and Losers 2008-2017")
lines(a,cumsum(log(1+data_4[which(Decile==1)]$DM_Ret[973:1092])),col='red')

legend("topleft", 
       legend = c("Past Winners", "Past Losers"), 
       col = c("black","red"), 
       lty = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       #horiz = F , 
       inset = c(0.01, 0.05))


