
### Problem Set 1 by Nikhil Gupta ###

#Question 1

library(lubridate)
library(data.table)
library(tidyr)
library(xts)
library(moments)

data_1 = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/Full_Data.csv", header=TRUE, 
                  stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") )

FF = read.csv("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/FF.csv", header=TRUE, 
              stringsAsFactors = FALSE )


# Default values of Delisting Returns nd Returns converted to NA above

PS1_Q1 <- function(data_1){
    
    #head(data_1)
    #data_1[data_1$date<= "12/31/1985",]
    Date <- mdy(data_1$date)
    #RET <- as.factor(data_1$RET)
    #DLRET <- as.factor(data_1$DLRET)
    
    crsp_stocks <- as.data.table(cbind(data_1[,1],Date,data_1[,3:8]))
    #crsp_stocks <- as.data.table(data_2)
    colnames(crsp_stocks)[1] <- "PERMNO"
    #head(crsp_stocks)
    
    # Data Cleaning 
    # Remove rows with missing both DLRET and RET
    final <- crsp_stocks[!(is.na(crsp_stocks$DLRET)) | !(is.na(crsp_stocks$RET)),]
    
    # Remove Rows with missing PRC 
    final1 <- final[!(is.na(final$PRC)) ,]
    
    # 
    final1$mktcap <- abs(final1$PRC*final1$SHROUT)
    final1$ret_final <- ifelse(is.na(final1$DLRET) , final1$RET, final1$DLRET)
    
    final1$ret_final <- ifelse((!(is.na(final1$DLRET)) & !(is.na(final1$RET))),((1+as.numeric(final1$RET))*(1+as.numeric(final1$DLRET)) - 1),final1$ret_final)
    #final3 <- final1[!(is.na(final1$DLRET)) & !(is.na(final1$RET)),]
    #head(final1)
    
    a <- final1[order(as.Date(final1$Date, format="%m/%d/%Y")),]
    
    
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
    vwretd_f <- vwretd[6:1103]*100
    err <- abs(FF_Mkt-vwretd_f)
    cbind(FF$Date,FF_Mkt,vwretd_f,err)
    cor(FF_Mkt,vwretd_f)
    
    index_test <- which.max(err)
    
    #n1 = length(uniq_dates)
    mkt_cap_lag<-0
    year = year(uniq_dates)
    month = month(uniq_dates)
    mkt_cap_lag[1] <- 0
    mkt_cap_lag[2:n] <- mkt_cap1[1:(n-1)]/1000000
    #mkt_cap1/1000000
    
    final_data <- as.data.table(cbind(year,month,mkt_cap_lag,ewretd,vwretd))
    colnames(final_data)[3] = "Stock_Lag_MV" 
    colnames(final_data)[4] = "Stock_Ew_Return" 
    colnames(final_data)[5] = "Stock_Vw_Return" 
    return(final_data)
    
}

final_data <- PS1_Q1(data_1)

# Question 2

ps1_q2 <- function(final_data,FF){
    n<-1104
    est_mkt_excess_ret <- final_data$Stock_Vw_Return[7:n] - FF$RF[1:(n-6)]/100
    actual_mkt_excess_ret <- FF$Mkt.RF[1:(n-6)]/100
    
    est_mkt_excess_mean1 <- (1+mean(est_mkt_excess_ret))^12 - 1
    est_mkt_excess_mean1
    
    est_mkt_excess_sd1 <- sqrt((var(est_mkt_excess_ret) + (1+mean(est_mkt_excess_ret))^2)^12 - (1+mean(est_mkt_excess_ret))^24)
    est_mkt_excess_sd1
    
    est_mkt_excess_sr1 <- est_mkt_excess_mean1/est_mkt_excess_sd1
    
    est_mkt_excess_mean <- mean(est_mkt_excess_ret)*12
    est_mkt_excess_sd <- sd(est_mkt_excess_ret)*sqrt(12)
    est_mkt_excess_sr <- est_mkt_excess_mean/est_mkt_excess_sd
    est_mkt_excess_skew <- skewness(est_mkt_excess_ret)
    est_mkt_excess_kurt <- kurtosis(est_mkt_excess_ret) - 3
    
    act_mkt_excess_mean1 <- (1+mean(actual_mkt_excess_ret))^12 - 1
    act_mkt_excess_mean1
    
    act_mkt_excess_sd1 <- sqrt((var(actual_mkt_excess_ret) + (1+mean(actual_mkt_excess_ret))^2)^12 - (1+mean(actual_mkt_excess_ret))^24)
    act_mkt_excess_sd1
    
    act_mkt_excess_sr1 <- act_mkt_excess_mean1/act_mkt_excess_sd1
    act_mkt_excess_sr1
    
    act_mkt_excess_mean <- mean(actual_mkt_excess_ret)*12
    act_mkt_excess_sd <- sd(actual_mkt_excess_ret)*sqrt(12)
    act_mkt_excess_sr <- act_mkt_excess_mean/act_mkt_excess_sd
    act_mkt_excess_skew <- skewness(actual_mkt_excess_ret)
    act_mkt_excess_kurt <- kurtosis(actual_mkt_excess_ret) - 3
    
    final_output_2 <- matrix(0,ncol=2,nrow=5)
    colnames(final_output_2) <- paste(c("Replication","Actual"),sep="")
    rownames(final_output_2) <- paste(c("Annualised Mean","Annualised Std Dev","Annualised Sharpe Ratio","Skewness","Excess Kurtosis"),sep="")
    
    final_output_2[1,1] <- est_mkt_excess_mean1
    final_output_2[2,1] <- est_mkt_excess_sd1
    final_output_2[3,1] <- est_mkt_excess_sr1
    final_output_2[4,1] <- est_mkt_excess_skew
    final_output_2[5,1] <- est_mkt_excess_kurt
    
    final_output_2[1,2] <- act_mkt_excess_mean1
    final_output_2[2,2] <- act_mkt_excess_sd1
    final_output_2[3,2] <- act_mkt_excess_sr1
    final_output_2[4,2] <- act_mkt_excess_skew
    final_output_2[5,2] <- act_mkt_excess_kurt
    return(final_output_2)    
    
}

final_output_2 <- ps1_q2(final_data,FF)

# Question 3

ps1_q3 <- function(final_data,FF){
    
    FF_Mkt <- FF$Mkt.RF + FF$RF  
    vwretd_f <- final_data$Stock_Vw_Return[7:1104]*100
    
    cor_3 <- cor(FF_Mkt,vwretd_f)
    cor_3a<- sprintf("%.8f",cor_3)
    max_abs_diff <- max(abs(FF_Mkt-vwretd_f))/100
    max_abs_diff_3a <- sprintf("%.8f",max_abs_diff)
    
    final_output_3 <- matrix(0,ncol=1,nrow=2)
    rownames(final_output_3) <- paste(c("Correlation","Maximum Absolute Difference"),sep="")
    final_output_3[1,1] <- cor_3a
    final_output_3[2,1] <- max_abs_diff_3a    
    return(final_output_3)
    
}

final_output_3 <- ps1_q3(final_data,FF)



