
### Problem Set 2 by Nikhil Gupta ###

#Question 1

library(lubridate)
library(data.table)
library(tidyr)
library(xts)
library(moments)
library(qpcR)

data_1 = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW2/Bonds_Data.csv", header=TRUE, 
                  stringsAsFactors = FALSE )

Rf = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW2/Rf_Data.csv", header=TRUE, 
              stringsAsFactors = FALSE )

######### For the 1st Assignment 

data_s = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/Full_Data.csv", header=TRUE, 
                  stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") )
dec1925 = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW2/Dec1925.csv", header=TRUE, 
                  stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") )
FF = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/FF.csv", header=TRUE, 
              stringsAsFactors = FALSE )
#################




# Default values of Delisting Returns nd Returns converted to NA above

PS2_Q1 <- function(data_1){

data_1$MCALDT <- mdy(data_1$MCALDT)
test <- as.Date(data_1$MCALDT)

#data_1[complete.cases(data_1),]
data_1[is.na(data_1)] = 0
data_1$TMRETNUA[which(data_1$TMRETNUA==-99)] = 0

a <- data_1[order(as.Date(data_1$MCALDT, format="%m/%d/%Y")),]


xtsdata1=xts(a,order.by=as.Date((data_1$MCALDT),"%m/%d/%Y")) ## Took 3 
epm1=endpoints(xtsdata1,"months")


#sum <-0
mkt_cap<-0
mkt_cap1<-0


#for (i in (2:length(epm1)-1)){
n <- length(epm1)-1

for (i in (1:n)){
    end = epm1[i+1] 
    start = epm1[i]+1
    mkt_cap <- a$TMTOTOUT[start:end]
    mkt_cap1[i] <- sum(mkt_cap)
}

# Equal Weighted Returns 
ewretd <-0 
for (i in (1:n)){
    end = epm1[i+1] 
    start = epm1[i]+1
    ret <- (as.numeric(as.character(data_1$TMRETNUA[start:end])))
    ewretd[i] <- mean(ret)
}

uniq_dates <- unique(data_1$MCALDT)
uniq_bonds <- unique(data_1$KYCRSPID)

# Value Weighted Returns 
vwretd <-0
Lag_Market_Cap <-0 
    for (i in 2:length(uniq_dates)){
    
    #for (i in 2:100){
        #i = 1105
       
        months <- data_1[which(data_1$MCALDT == uniq_dates[i]),1:3]
        market_cap <- data.table(cbind(data_1$KYCRSPID[which(data_1$MCALDT == uniq_dates[i-1])], data_1$TMTOTOUT[which(data_1$MCALDT == uniq_dates[i-1])])) 
        #market_cap$V2 <- as.numeric(market_cap$V2)
        #months$TMRETNUA*market_cap$V2
        
        colnames(market_cap) <- c("KYCRSPID", "MarketCap")
        merged_data <- merge(months, market_cap, by = "KYCRSPID")
        merged_data1 <- merged_data[!duplicated(merged_data[c(1,3)]),]
        Lag_Market_Cap[i-1] <- sum(as.numeric(as.character(merged_data1$MarketCap)))
        
        vwretd[i-1] <- suppressWarnings(sum(merged_data1$TMRETNUA * as.numeric(as.character(merged_data1$MarketCap))) / Lag_Market_Cap[i-1])
        
    }

#n1 = length(uniq_dates)
mkt_cap_lag<-0
year = year(uniq_dates)[2:n]
month = month(uniq_dates)[2:n]
#mkt_cap_lag[1] <- 0
mkt_cap_lag <- mkt_cap1[2:n]
#mkt_cap1/1000000
  
final_data <- as.data.table(cbind(year,month,mkt_cap_lag,ewretd[2:n],vwretd))
#final_data <- as.data.table(cbind(year,month,Lag_Market_Cap,ewretd[2:n],vwretd))
colnames(final_data)[3] = "Bond_Lag_MV" 
colnames(final_data)[4] = "Bond_Ew_Return" 
colnames(final_data)[5] = "Bond_Vw_Return" 
return(final_data)    

}    
   
final_data_bonds <- PS2_Q1(data_1)
final_data_bonds

#############################
# Function to read from the previous assignment -- New method to incorporate Dec 1925
 
PS1_Q1 <- function(data_s){
    
    data_2 <- rbind(data_s,dec1925)
    Date <- mdy(data_2$date)
    
    crsp_stocks <- as.data.table(cbind(data_2[,1],Date,data_2[,3:8]))
    #crsp_stocks <- as.data.table(data_2)
    colnames(crsp_stocks)[1] <- "PERMNO"
    #head(crsp_stocks)
    
    # Data Cleaning 
    # Make RET and DLRET 0 in case where they are NA 
    
    crsp_stocks[is.na(crsp_stocks)] <- 0
    #final <- crsp_stocks[!(is.na(crsp_stocks$DLRET)) | !(is.na(crsp_stocks$RET)),]
    
    # Remove Rows with missing PRC 
    # final1 <- final[!(is.na(final$PRC)) ,]   # No Need 
    
    # 
    crsp_stocks$ret_final <-0
    crsp_stocks$mktcap <- abs(crsp_stocks$PRC*crsp_stocks$SHROUT)
    #crsp_stocks$ret_final <- ifelse(is.na(crsp_stocks$DLRET) , crsp_stocks$RET, crsp_stocks$DLRET)
    
    crsp_stocks$ret_final <- suppressWarnings(((1+as.numeric(crsp_stocks$RET))*(1+as.numeric(crsp_stocks$DLRET)) - 1))
    crsp_stocks$ret_final[is.na(crsp_stocks$ret_final)] <- 0
    
    #crsp_stocks$ret_final <- crsp_stocks$ret_final[!is.na(crsp_stocks$ret_final)]
    
    #final3 <- final1[!(is.na(final1$DLRET)) & !(is.na(final1$RET)),]
    #head(final1)
    
    a <- crsp_stocks[order(as.Date(crsp_stocks$Date, format="%m/%d/%Y")),]
    
    
    a1 <- a[((a$EXCHCD == 1) | (a$EXCHCD == 2) | (a$EXCHCD == 3))  ,]
    a2 <- a1[((a1$SHRCD == 10) | (a1$SHRCD == 11))  ,]
    
    #Date1 <- mdy(a2$Date)
    a3 <- cbind.data.frame(a2$PERMNO, a2$Date, a2$mktcap, a2$ret_final)
    colnames(a3)[1] <- "PERMNO"
    colnames(a3)[2] <- "Date"
    colnames(a3)[3] <- "Mkt_Cap"
    colnames(a3)[4] <- "Return"
    
    #a4 <- a3[is.na(a3$Date),]
    
    str(a3)
    #mdy(a3$Date)
    xtsdata1=xts(a3,order.by=as.Date((a3$Date),"%m/%d/%Y")) ## Took 3 
    epm1=endpoints(xtsdata1,"months")
    
    #sum <-0
    mkt_cap<-0
    mkt_cap1<-0
    
    #for (i in (2:length(epm1)-1)){
    n <- length(epm1)-1
    
    for (i in (1:n)){
        end = epm1[i+1] 
        start = epm1[i]+1
        mkt_cap <- a3$Mkt_Cap[start:end]
        mkt_cap1[i] <- sum(mkt_cap)
    }
    
    # Equal Weighted Returns 
    ewretd <-0 
    for (i in (1:n)){
        end = epm1[i+1] 
        start = epm1[i]+1
        ret <- (as.numeric(as.character(a3$Return[start:end])))
        ewretd[i] <- mean(ret)
    }
    
    # Calculating the final market returns Better Approach
    
    uniq_dates <- unique(a3$Date)
    uniq_stocks <- unique(a3$PERMNO)
    
    
    vwretd <-0
    for (i in 2:length(uniq_dates)){
        
        #for (i in 2:100){
        #i = 3
        Lag_Market_Cap <-0 
        months <- a3[which(a3$Date == uniq_dates[i]),]
        market_cap <- cbind(a3$PERMNO[which(a3$Date == uniq_dates[i-1])], a3$Mkt_Cap[which(a3$Date == uniq_dates[i-1])]) 
        colnames(market_cap) <- c("PERMNO", "MarketCap")
        merged_data <- merge(months, market_cap, by = "PERMNO")
        
        #merged_data1 <- merged_data[!duplicated(merged_data),]
        
        Lag_Market_Cap[i-1] <- sum(as.numeric(as.character(merged_data$MarketCap)))
        vwretd[i-1] <- sum(as.numeric(as.character(merged_data$Return)) * as.numeric(as.character(merged_data$MarketCap))) / Lag_Market_Cap[i-1]
        #equal_weighted_return[i-1] <- sum(return) / length(return)
    }
    
    
    FF_Mkt <- FF$Mkt.RF + FF$RF
    vwretd_f <- vwretd[7:1104]*100
    err <- abs(FF_Mkt-vwretd_f)
    cbind(FF$Date,FF_Mkt,vwretd_f,err)
    cor(FF_Mkt,vwretd_f)
    
    index_test <- which.max(err)
    
    #n1 = length(uniq_dates)
    mkt_cap_lag<-0
    year = year(uniq_dates)
    month = month(uniq_dates)
    #mkt_cap_lag[1] <- 0
    mkt_cap_lag <- mkt_cap1[2:n]/1000       # Number of shares are in thousands and price in millions 
    #mkt_cap1/1000000
    
    final_data <- as.data.table(cbind(year[2:n],month[2:n],mkt_cap_lag,ewretd[2:n],vwretd))
    colnames(final_data)[3] = "Stock_Lag_MV" 
    colnames(final_data)[4] = "Stock_Ew_Return" 
    colnames(final_data)[5] = "Stock_Vw_Return" 
    return(final_data)
    
}

final_data_s <- PS1_Q1(data_s)
final_data_s
############################################

#Question 2

ps2_q2 <- function(final_data_s,final_data_bonds,Rf){
    
    Stock_Excess_Vw_Ret <- final_data_s$Stock_Vw_Return - Rf$t30ret
    Bond_Excess_Vw_Ret <- final_data_bonds$Bond_Vw_Return - Rf$t30ret
    final_output <- cbind(final_data_bonds$year,final_data_bonds$month, final_data_s$Stock_Lag_MV, Stock_Excess_Vw_Ret,final_data_bonds$Bond_Lag_MV,Bond_Excess_Vw_Ret)
    colnames(final_output)[1] = "year"
    colnames(final_output)[2] = "month"
    colnames(final_output)[3] = "Stock_Lag_MV"
    colnames(final_output)[5] = "Bond_Lag_MV"
    
    return(final_output)
}

final_data_ps2_q2 <- ps2_q2(final_data_s,final_data_bonds,Rf)
final_data_ps2_q2

# Question 3

ps2_q3 <- function(final_data_ps2_q2){

Monthly_CRSP_Universe <- as.data.table(final_data_ps2_q2)

# Excess Return

stocks_weight <- Monthly_CRSP_Universe$Stock_Lag_MV / (Monthly_CRSP_Universe$Stock_Lag_MV + Monthly_CRSP_Universe$Bond_Lag_MV)
bonds_weight <- Monthly_CRSP_Universe$Bond_Lag_MV / (Monthly_CRSP_Universe$Stock_Lag_MV + Monthly_CRSP_Universe$Bond_Lag_MV)

Excess_Vw_Ret <- stocks_weight*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret  + bonds_weight*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret

#Excess_60_40_Ret <- (stocks_weight*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret + bonds_weight*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret)/(stocks_weight+bonds_weight)
Excess_60_40_Ret <- 0.6*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret + 0.4*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret
#plot(Excess_60_40_Ret,type='l')


#plot((cumsum(Excess_Vw_Ret[37:1014])),type='l')
#lines((cumsum(Excess_60_40_Ret[37:1014])),col='red')


# Unlevered k and Return
sigma_hat_unlev1<-NA
sigma_hat_unlev2<-NA
stock_wght_unl<-0
bond_wght_unl<-0
unlevered_k<-NA

for (i in 37:1104){
    sigma_hat_unlev1[i] <- sd(Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[(i-36):(i-1)])
    sigma_hat_unlev2[i] <- sd(Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[(i-36):(i-1)])
    unlevered_k[i] = 1/( (1/sigma_hat_unlev1[i]) +  (1/sigma_hat_unlev2[i]) )
    stock_wght_unl[i] <- unlevered_k[i]/sigma_hat_unlev1[i]
    bond_wght_unl[i] <- unlevered_k[i]/sigma_hat_unlev2[i]
}

unleverd_ret<-NA
for (i in 37:1104){
    unleverd_ret[i] <- stock_wght_unl[i]*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[i] + bond_wght_unl[i]*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[i]
}

# Levered k and Return
k<-NA
ret<- NA
ret[37:1104] <- Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[37:1104]/sigma_hat_unlev1[37:1104] + Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[37:1104]/sigma_hat_unlev2[37:1104]
k[37:1104] <- sd(Excess_Vw_Ret[37:1104])/sd(ret[37:1104])

#k[37:1104] <- sd(Excess_Vw_Ret[37:1104])/sqrt(2)


stock_wght_lev<-NA
bond_wght_lev<-NA

stock_wght_lev[37:1104] <- k[37:1104]/sigma_hat_unlev1[37:1104]
bond_wght_lev[37:1104] <- k[37:1104]/sigma_hat_unlev2[37:1104]

levered_ret<-NA
for (i in 37:1104){
    levered_ret[i] <- stock_wght_lev[i]*Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[i] + bond_wght_lev[i]*Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[i]
}

sd(levered_ret[37:1104])*sqrt(12)
sd(Excess_Vw_Ret[37:1104])*sqrt(12)


final_output <- as.data.table( cbind(year=Monthly_CRSP_Universe$year,month=Monthly_CRSP_Universe$month,Stock_Excess_Vw_Ret=Monthly_CRSP_Universe$Stock_Excess_Vw_Ret,
      Bond_Excess_Vw_Ret=Monthly_CRSP_Universe$Bond_Excess_Vw_Ret,Excess_Vw_Ret=Excess_Vw_Ret, Excess_60_40_Ret=Excess_60_40_Ret, 
      Stock_inverse_sigma_hat = (1/sigma_hat_unlev1), Bond_inverse_sigma_hat = (1/sigma_hat_unlev2), Unlevered_k = unlevered_k,
      Excess_Unlevered_RP_Ret = unleverd_ret, Levered_k = k, Excess_Levered_RP_Ret = levered_ret) )

return(final_output)

} # Function Ends 

Port_Rets <-  ps2_q3(final_data_ps2_q2)
Port_Rets
# Question 4


PS2_Q4 <- function(Port_Rets){

Port_Rets1 <- as.data.table(Port_Rets)
a <- matrix(NA,6,6)
n1 <- 37           # Start
n2 <- 1014          # End 


#Port_Rets1 <- as.data.table(Port_Rets)    


#x <- Port_Rets$Stock_Excess_Vw_Ret[49:1014]
a[1,1] <- mean(Port_Rets1$Stock_Excess_Vw_Ret[n1:n2])*12
a[1,2] <- as.numeric(t.test(as.vector(Port_Rets1$Stock_Excess_Vw_Ret[n1:n2]))[1])
a[1,3] <- sd(Port_Rets1$Stock_Excess_Vw_Ret[n1:n2])*sqrt(12)    
a[1,4] <- a[1,1]/a[1,3]
a[1,5] <- skewness(Port_Rets1$Stock_Excess_Vw_Ret[n1:n2])
a[1,6] <- kurtosis(Port_Rets1$Stock_Excess_Vw_Ret[n1:n2]) - 3

a[2,1] <- mean(Port_Rets1$Bond_Excess_Vw_Ret[n1:n2])*12
a[2,2] <- as.numeric(t.test(as.vector(Port_Rets1$Bond_Excess_Vw_Ret[n1:n2]))[1])
a[2,3] <- sd(Port_Rets1$Bond_Excess_Vw_Ret[n1:n2])*sqrt(12)    
a[2,4] <- a[2,1]/a[2,3]
a[2,5] <- skewness(Port_Rets1$Bond_Excess_Vw_Ret[n1:n2])
a[2,6] <- kurtosis(Port_Rets1$Bond_Excess_Vw_Ret[n1:n2]) - 3
    
a[3,1] <- mean(Port_Rets1$Excess_Vw_Ret[n1:n2])*12
a[3,2] <- as.numeric(t.test(as.vector(Port_Rets1$Excess_Vw_Ret[n1:n2]))[1])
a[3,3] <- sd(Port_Rets1$Excess_Vw_Ret[n1:n2])*sqrt(12)    
a[3,4] <- a[3,1]/a[3,3]
a[3,5] <- skewness(Port_Rets1$Excess_Vw_Ret[n1:n2])
a[3,6] <- kurtosis(Port_Rets1$Excess_Vw_Ret[n1:n2]) - 3

a[4,1] <- mean(Port_Rets1$Excess_60_40_Ret[n1:n2])*12
a[4,2] <- as.numeric(t.test(as.vector(Port_Rets1$Excess_60_40_Ret[n1:n2]))[1])
a[4,3] <- sd(Port_Rets1$Excess_60_40_Ret[n1:n2])*sqrt(12)    
a[4,4] <- a[4,1]/a[4,3]
a[4,5] <- skewness(Port_Rets1$Excess_60_40_Ret[n1:n2])
a[4,6] <- kurtosis(Port_Rets1$Excess_60_40_Ret[n1:n2]) - 3

a[5,1] <- mean(Port_Rets1$Excess_Unlevered_RP_Ret[(n1+1):n2])*12
a[5,2] <- as.numeric(t.test(as.vector(Port_Rets1$Excess_Unlevered_RP_Ret[(n1+1):n2]))[1])
a[5,3] <- sd(Port_Rets1$Excess_Unlevered_RP_Ret[(n1+1):n2])*sqrt(12)    
a[5,4] <- a[5,1]/a[5,3]
a[5,5] <- skewness(Port_Rets1$Excess_Unlevered_RP_Ret[(n1+1):n2])
a[5,6] <- kurtosis(Port_Rets1$Excess_Unlevered_RP_Ret[(n1+1):n2]) - 3

a[6,1] <- mean(Port_Rets1$Excess_Levered_RP_Ret[(n1+1):n2])*12
a[6,2] <- as.numeric(t.test(as.vector(Port_Rets1$Excess_Levered_RP_Ret[(n1+1):n2]))[1])
a[6,3] <- sd(Port_Rets1$Excess_Levered_RP_Ret[(n1+1):n2])*sqrt(12)    
a[6,4] <- a[6,1]/a[6,3]
a[6,5] <- skewness(Port_Rets1$Excess_Levered_RP_Ret[(n1+1):n2])
a[6,6] <- kurtosis(Port_Rets1$Excess_Levered_RP_Ret[(n1+1):n2]) - 3

colnames(a) = c("Excess Return", "t-Stat of Excess Return","Volatility", "SR", "Skewness","Excess Kurtosis")
rownames(a) = c("CRSP Stocks","CRSP Bonds", "Value Weighted Portfolio","60/40 Portfolio", "RP, unlevered","RP")
return(a)

}

final_q4 <- PS2_Q4(Port_Rets)
final_q4


 
