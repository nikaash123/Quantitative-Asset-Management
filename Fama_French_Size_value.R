
### Problem Set 4 by Nikhil Gupta ###

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

inp_comp1 = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Input_1_compustat.csv", header=TRUE, 
                  stringsAsFactors = FALSE )

inp_comp2 = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Input_2.csv", header=TRUE, 
               stringsAsFactors = FALSE )

inp_crsp1 = suppressWarnings(fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Input_3_1.csv", header=TRUE, 
                                stringsAsFactors = FALSE,na.strings=c("",".","NA","C","A","S","T","P") ))

link_table = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Linking_table.csv", header=TRUE, 
                  stringsAsFactors = FALSE )

ff_me <-  fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Portfolios_Formed_on_ME.csv", header=TRUE, 
              stringsAsFactors = FALSE )

ff_btm <-  fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Portfolios_Formed_on_BE-ME.csv", header=TRUE, 
                stringsAsFactors = FALSE )

Mkt_Rf = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/FF.csv", header=TRUE, 
               stringsAsFactors = FALSE )

#Mkt_Rf = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW1/FF.csv", header=TRUE, 
              #stringsAsFactors = FALSE )

#KRF_mom = fread("C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW3/KRF_Momentum.csv", header=TRUE, 
                #stringsAsFactors = FALSE )

inp_comp1_1 <- inp_comp1[order(as.Date(inp_comp1$datadate, format="%m/%d/%Y")),]
inp_comp1_1$datadate = mdy(inp_comp1_1$datadate)
inp_comp1_1 <- inp_comp1_1[which(inp_comp1_1$datadate >= "1972-01-31")]

# Final she3
inp_comp1_2 <- inp_comp1_1[,she1:= ifelse(is.na(seq),(ceq+pstk),seq)]
inp_comp1_2 <- inp_comp1_2[,she2:= ifelse(is.na(she1),(at-lt-mib),she1)]
inp_comp1_2 <- inp_comp1_2[,she3:= ifelse(is.na(she2),(at-lt),she2)]


#Compustat_BV[, ITCB_TXDB := ifelse(!is.na(itcb + txdb), (itcb + txdb), ifelse(!is.na(itcb), itcb, txdb))]
#Compustat_BV[, DT := ifelse(!is.na(txditc),txditc,ITCB_TXDB)]

# Final dt2 
inp_comp1_3 <- inp_comp1_2[,dt1:= ifelse(is.na(txditc),(itcb+txdb),txditc)]
inp_comp1_3 <- inp_comp1_3[,dt2:= ifelse(is.na(dt1),ifelse(!is.na(itcb), itcb, txdb),dt1)] ## Check , Sum what is not missing

# Final ps2 
inp_comp1_4 <- inp_comp1_3[,ps1:= ifelse(is.na(pstkrv),pstkl,pstkrv)]
inp_comp1_4 <- inp_comp1_4[,ps2:= ifelse(is.na(ps1),pstk,ps1)] 

# PRBA Merge 

inp_comp2_1 <- inp_comp2[order(as.Date(inp_comp2$datadate, format="%m/%d/%Y")),]
inp_comp2_1$datadate = mdy(inp_comp2_1$datadate)
colnames(inp_comp2_1)[1] <- "gvkey"

inp_comp_mer <- merge(inp_comp1_4,inp_comp2_1[,c("gvkey","prba","datadate")],by=c("gvkey","datadate"),all.x=TRUE)  # Left Join
inp_comp_mer <- inp_comp_mer[order(as.Date(inp_comp_mer$datadate, format="%m/%d/%Y")),]

inp_comp_mer$prba[is.na(as.numeric(inp_comp_mer$prba))] <- 0
inp_comp_mer$dt2[is.na(as.numeric(inp_comp_mer$dt2))] <- 0
inp_comp_mer$ps2[is.na(as.numeric(inp_comp_mer$ps2))] <- 0

# Final BE, Check few calculations 
inp_comp_mer1 <- inp_comp_mer[,be:= she3-ps2+dt2-prba]

#inp_comp_mer1 <- inp_comp_mer1[,Month1:= month(as.Date(datadate))]
inp_comp_mer1 <- inp_comp_mer1[,Year1:= year(as.Date(datadate))]

#suppressWarnings(inp_comp_mer1[, be_test := be[Month1==5], by=c("gvkey", "Year1")])
#inp_comp_mer1[, be_final := shift(be_test, n = 5), by=c("gvkey")]



## Merge gvkey, PERMNO and PERMCO using the link table

## Make gvkey unique, Cleaning here of link table, do that later 

link_table1 <- link_table[!which(duplicated(link_table, by="gvkey"))]

inp_comp_mer_perm <- merge(inp_comp_mer1,link_table1[,c("gvkey","LPERMNO","LPERMCO")],by=c("gvkey"),all.x = TRUE)  # Left Join 
#inp_comp_mer <- inp_comp_mer[order(as.Date(inp_comp_mer$datadate, format="%m/%d/%Y")),]
inp_comp_mer_perm <- inp_comp_mer_perm[order(as.Date(inp_comp_mer_perm$datadate, format="%m/%d/%Y")),]

length(inp_comp_mer_perm[!is.na(be)]$be)
#length(inp_comp_mer1[!is.na(be)]$be)
## 424231 values of BE 

# Calculate the Market Equity

inp_crsp2 <- inp_crsp1[order(as.Date(inp_crsp1$date, format="%m/%d/%Y")),]
inp_crsp2$date = mdy(inp_crsp2$date)

## Remove Financial Firms Here using SIC! 

# inp_crsp2 <- inp_crsp2[((inp_crsp2$SICCD == 1)  ,]

inp_crsp2 <- inp_crsp2[((inp_crsp2$EXCHCD == 1) | (inp_crsp2$EXCHCD == 2) | (inp_crsp2$EXCHCD == 3))  ,]
inp_crsp2 <- inp_crsp2[((inp_crsp2$SHRCD == 10) | (inp_crsp2$SHRCD == 11))  ,]
inp_crsp2 <- inp_crsp2[!which(SICCD==6172 | SICCD==6199)]

# Calculating Returns 
inp_crsp2$RET[is.na(as.numeric(inp_crsp2$RET))] <- 0
inp_crsp2$DLRET[is.na(as.numeric(inp_crsp2$DLRET))] <- 0
inp_crsp2 <- inp_crsp2[,fin_ret:= ((1+as.numeric(inp_crsp2$DLRET))*(1+as.numeric(inp_crsp2$RET)) - 1)]

# Unique PERMNO's , Merge to get LPERMCO's and add market cap's and returns

inp_crsp2 <- inp_crsp2[,mkt_cap:= abs(( PRC *SHROUT )/ 1000)]    # Mkt_Cap in millions 
inp_crsp2 <- inp_crsp2[ , Lagged_mkt_cap:= shift(mkt_cap,1), by = .(PERMCO)]    # Lagged Mkt_Cap in millions 

inp_crsp2 <- inp_crsp2[, PERMNO_weight := Lagged_mkt_cap/sum(Lagged_mkt_cap), by = c("PERMCO", "date")]
inp_crsp2 <- inp_crsp2[,Ret_permco := sum(PERMNO_weight*fin_ret), by = .(PERMCO, date)]
inp_crsp2 <- inp_crsp2[,Lagged_mkt_cap_permco := sum(Lagged_mkt_cap), by = .(PERMCO, date)]



#link_table2 <- link_table[!which(duplicated(link_table, by="LPERMNO"))]
#colnames(link_table2)[9] = "PERMNO"

#link_table2[!which(duplicated(link_table2, by="LPERMCO"))]
#inp_crs_merge_permco <- merge(inp_crsp2,link_table2[,c("gvkey","PERMNO","LPERMCO")],by=c("PERMNO"),all.x = TRUE)  # Left Join 

# Combine the subsidiaries, returns and mkt_cap

#ddply(inp_crs_merge_permco,~group,summarise,mean=mean(age),sd=sd(age))

#inp_crs_merge_permco <- inp_crs_merge_permco[,fin_ret1:= .(group_by(fin_ret,LPERMCO)) , by=c("date","LPERMCO")]


#inp_crsp2 <- inp_crsp2[,mkt_cap:= abs((PRC*SHROUT)/1000)]
inp_crsp2 <- inp_crsp2[,Month:= month(as.Date(date))]
inp_crsp2 <- inp_crsp2[,Year:= year(as.Date(date))]

#inp_comp_mer1


#Setting Market Cap For Size
suppressWarnings(inp_crsp2[, Stock_Market_Cap_Test := mkt_cap[Month==6], by=c("PERMNO", "Year")])
inp_crsp2[, Stock_Market_Cap_Size_c := shift(Stock_Market_Cap_Test, n = 6), by="PERMNO"]

inp_crsp2[, Stock_Market_Cap_Size := sum(Stock_Market_Cap_Size_c),by=c("PERMCO", "date")]

# Setting Market Cap for B/E 
#inp_crsp2 <- suppressWarnings(inp_crsp2[, Stock_Market_Cap_Size := (mkt_cap[Month==6]), by=.(PERMNO, Year)])

inp_crsp2 <- inp_crsp2[, Stock_Market_Cap_BM_permco := sum(mkt_cap), by=.(PERMCO, date)]


#inp_crsp2 <- inp_crsp2[, Stock_Market_Cap_BM_c := (Stock_Market_Cap_BM_Lag[1]), by=.(PERMCO, Year)]


inp_crsp2 <- inp_crsp2[order(as.Date(inp_crsp2$date, format="%m/%d/%Y")),]


inp_crsp2 <- inp_crsp2[!duplicated(inp_crsp2,by=c("PERMCO","date")),]
#inp_crsp2 <- inp_crsp2[, Stock_Market_Cap_Size := shift(mkt_cap,i), by=c("PERMNO","date")]

colnames(inp_comp_mer_perm)[2] = "date"
colnames(inp_comp_mer_perm)[35] = "PERMCO"     # Check this step, sometimes 34, 35 

inp_comp_mer_perm$Year = year(as.Date(inp_comp_mer_perm$date))
#inp_crsp2$year = year(as.Date(inp_crsp2$date))

# Unique PERMCO's
inp_comp_mer_perm <- inp_comp_mer_perm[!is.na(PERMCO)]
inp_comp_mer_perm1 <- inp_comp_mer_perm[!duplicated(inp_comp_mer_perm,by=c("PERMCO","date")),]

inp_comp_mer_perm1 <- inp_comp_mer_perm1[order(as.Date(inp_comp_mer_perm1$date, format="%m/%d/%Y")),]

inp_comp_crsp_mer <- merge(inp_crsp2,inp_comp_mer_perm1[,c("be","LPERMNO","Year","PERMCO")],by=c("Year","PERMCO")) 
inp_comp_crsp_mer <- inp_comp_crsp_mer[order(as.Date(inp_comp_crsp_mer$date, format="%m/%d/%Y")),]


# Taking Care of the Book/Market Equity 
inp_comp_crsp_mer <- inp_comp_crsp_mer[, Stock_Market_Cap_BM_Lag := lag(Stock_Market_Cap_BM_permco), by=.(PERMCO)]
inp_comp_crsp_mer <- inp_comp_crsp_mer[, Stock_Market_Cap_BM_c := (Stock_Market_Cap_BM_Lag[1]), by=.(PERMCO, Year)]

inp_comp_crsp_mer <- inp_comp_crsp_mer[, Book_Equity_Fiscal := shift(be,n=12), by=.(PERMCO)]
inp_comp_crsp_mer <- inp_comp_crsp_mer[, be_final := shift(Book_Equity_Fiscal,n=6), by=.(PERMCO)]
inp_comp_crsp_mer <- inp_comp_crsp_mer[, Stock_Market_Cap_BM := shift(Stock_Market_Cap_BM_c,n=6), by=.(PERMCO)]



# Adjust BE to take of Year t-1 
#inp_comp_crsp_mer <- inp_comp_crsp_mer[, be_lag := lag(be), by=.(PERMCO)]
#inp_comp_crsp_mer <- inp_comp_crsp_mer[, be_final := (be_lag[1]), by=.(PERMCO, Year)]


# Remove NA's 

inp_comp_crsp_mer_size <- inp_comp_crsp_mer[!is.na(Stock_Market_Cap_Size)]
inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[!is.na(Ret_permco)]

# For BtM, remove Stock_Market_Cap_Test == NA as well , June t missing ###

inp_comp_crsp_mer_bm <- inp_comp_crsp_mer[!is.na(Stock_Market_Cap_BM)]      # Dec t-1 not missing 
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[!is.na(be_final)]
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[!is.na(Stock_Market_Cap_Test)]  # June t not missing 
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[!is.na(be)]   # Optional to take historical be values
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[!is.na(Ret_permco)]

# For Decile portfolios , take only positive values of B/E 
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[which(be_final>0)]

inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[, BtM_ratio := be_final / Stock_Market_Cap_BM]
#summary(final_DT$BtM_ratio)
#inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[which(date>="1973-07-31")]

# Overall deciles 
GetPortNums <- function(x, numports) {
    as.integer(cut(
        x,
        quantile(unique(x, probs = 0:numports / numports, na.rm = TRUE)),
        include.lowest = TRUE
    ))
}

# Overall for Size Median
GetPortNums1 <- function(x) {
    as.integer(cut(
        x,
        quantile(x, probs=c(0,0.5,1) , na.rm = TRUE),
        include.lowest = TRUE
    ))
}

# Overall BM 30%, 70% 
GetPortNums2 <- function(x) {
    as.integer(cut(
        x,
        quantile(x, probs=c(0,0.3,0.7,1) , na.rm = TRUE),
        include.lowest = TRUE
    ))
}

inp_comp_crsp_mer_bm[, BTM_cut_overall := GetPortNums(BtM_ratio, 10), by=.(date)]
inp_comp_crsp_mer_bm[, BTM_cut_med_overall := GetPortNums2(BtM_ratio), by=.(date)]

inp_comp_crsp_mer_size[, Size_cut_overall := GetPortNums(Stock_Market_Cap_Size, 10), by=.(date)]
inp_comp_crsp_mer_size[, Size_cut_med_overall := GetPortNums1(Stock_Market_Cap_Size), by=.(date)]



## Size cut based on NYSE firms ## Start ###

GetPortNumsSize <- function(x,y) {
    breakpoints <- as.numeric(unique(quantile(x, probs=0:10/10, na.rm = TRUE)))
    
    as.integer(cut(y,
                   breakpoints,
                   include.lowest=TRUE))
}

## Size Cut Based on Median
GetPortNumsSize1 <- function(x,y) {
    breakpoints <- as.numeric(unique(quantile(x, probs=c(0,0.5,1), na.rm = TRUE)))
    
    as.integer(cut(y,
                   breakpoints,
                   include.lowest=TRUE))
}

inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[,Size_cut_nyse:=mapply(GetPortNumsSize,.(Stock_Market_Cap_Size[which(EXCHCD==1)]),.(Stock_Market_Cap_Size)),by=.(Year,Month==7)]
inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[,Size_cut_nyse1:= ifelse(is.na(inp_comp_crsp_mer_size$Size_cut_nyse),Size_cut_overall,Size_cut_nyse)]

inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[,Size_cut_nyse_med:=mapply(GetPortNumsSize1,.(Stock_Market_Cap_Size[which(EXCHCD==1)]),.(Stock_Market_Cap_Size)),by=.(Year,Month==7)]
inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[,Size_cut_nyse_med1:= ifelse(is.na(inp_comp_crsp_mer_size$Size_cut_nyse_med),Size_cut_med_overall,Size_cut_nyse_med)]

# Size cut based on NYSE firms #### END ###  


## B/M cut based on NYSE firms ## Start ###

GetPortNumsbtm <- function(x,y) {
    breakpoints <- as.numeric(quantile(x, probs=0:10/10, na.rm = TRUE))
    
    as.integer(cut(y,
                   breakpoints,
                   include.lowest=TRUE))
}

inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[,btm_cut_nyse:=mapply(GetPortNumsbtm,.(BtM_ratio[which(EXCHCD==1)]),.(BtM_ratio)),by=.(Year,Month==7)]
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[,btm_cut_nyse1:= ifelse(is.na(inp_comp_crsp_mer_bm$btm_cut_nyse),BTM_cut_overall,btm_cut_nyse)]

# B/M cut based on NYSE firms #### END ###  


## B/M cut based on NYSE firms for 30% and 70% ## Start ###

GetPortNumsbtm1 <- function(x,y) {
    breakpoints <- as.numeric(unique(quantile(x, probs=c(0,0.3,0.7,1), na.rm = TRUE)))
    
    as.integer(cut(y,
                   breakpoints,
                   include.lowest=TRUE))
}

inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[,btm_cut_nyse_med:=mapply(GetPortNumsbtm1,.(BtM_ratio[which(EXCHCD==1)]),.(BtM_ratio)),by=.(Year,Month==7)]
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[,btm_cut_nyse_med1:= ifelse(is.na(inp_comp_crsp_mer_bm$btm_cut_nyse_med),BTM_cut_med_overall,btm_cut_nyse_med)]


# Equal Weighted Returns for Size 

inp_comp_crsp_mer_size <- inp_comp_crsp_mer_size[,Size_ret:= mean(Ret_permco),by=c("date","Size_cut_nyse1")]

# Equal Weighted Returns for BM
inp_comp_crsp_mer_bm <- inp_comp_crsp_mer_bm[,btm_ret:= mean(Ret_permco),by=c("date","btm_cut_nyse1")]


# Value Weighted Returns for Size 
#inp_comp_crsp_mer <- inp_comp_crsp_mer[,Stock_Market_Cap_Size_sum:= sum(Stock_Market_Cap_Size),by=c("date","Size_cut")]
#inp_comp_crsp_mer <- inp_comp_crsp_mer[,size_weights:= Stock_Market_Cap_Size/Stock_Market_Cap_Size_sum]
#inp_comp_crsp_mer <- inp_comp_crsp_mer[,Size_ret:= sum(size_weights*Ret_permco),by=c("date","Size_cut")]


sub <- inp_comp_crsp_mer_size[,c("date","Size_cut_nyse1","Size_ret")]
sub <- unique(sub)
sub <- sub[which(date>="1973-01-31")]

final_size <- suppressWarnings(sub[with(sub, order(mdy(date), Size_cut_nyse1)),])

#plot(final_size$Size_ret[541:1080],type='l')
#lines(ff_me$`2-Dec`/100,col='red')


#inp_comp_crsp_mer[which(Size_cut==10)]
#inp_comp_mer <- inp_comp_mer[order(as.Date(inp_comp_mer$datadate, format="%m/%d/%Y")),]
#cor(DM_ret$Size_ret[1:540], ff_me$`Lo 10`)

### For BTM

sub1 <- inp_comp_crsp_mer_bm[,c("date","btm_cut_nyse1","btm_ret")]
sub1 <- unique(sub1)
sub1$date = as.Date(sub1$date)
sub1 <- sub1[which(date>="1973-07-31")]

#sub1 <- sub1[which(date>="1973-01-31")]

final_btm <- suppressWarnings(sub1[with(sub1, order(mdy(date), btm_cut_nyse1)),])
#write.csv(final_btm,"C:/Users/nikaa/Desktop/Lectures/Spring_2018/Quantitative_Asset_Management/Home_Works/HW4/Test.csv" )

#plot(final_btm$btm_ret[541:1080],type='l')
#lines(ff_btm/100,col='red')

#cor(final_btm$btm_ret[1:540], ff_btm$Lo.10/100)


# Answer 2

final_2 <- matrix(ncol=11,nrow=5)
ff_me <- data.frame(ff_me)

for (i in 1:10){
    #port_dm_ret1[i] <- mean(data_4[which(Decile==i)]$DM_Ret)*12
    final_2[1,i] <- mean(final_size[which(final_size$Size_cut_nyse1==i)]$Size_ret- Mkt_Rf[which(Date>=197301)]$RF/100)*12
    final_2[2,i] <- sd(final_size[which(final_size$Size_cut_nyse1==i)]$Size_ret- Mkt_Rf[which(Date>=197301)]$RF/100)*sqrt(12)
    final_2[3,i] <- final_2[1,i]/final_2[2,i]
    final_2[4,i] <- skewness(log(1+final_size[which(Size_cut_nyse1==i)]$Size_ret))
    final_2[5,i] <- cor(final_size[which(Size_cut_nyse1==i)]$Size_ret, ff_me[,(i+1)])
    #port_krf[i] <- mean(data_4[which(Decile==i)]$KRF_Ret- data_4[which(Decile==i)]$Rf)
}


final_2[1,11] = mean(final_size[which(Size_cut_nyse1==10)]$Size_ret- final_size[which(Size_cut_nyse1==1)]$Size_ret)*12
final_2[2,11] = sd(final_size[which(Size_cut_nyse1==10)]$Size_ret- final_size[which(Size_cut_nyse1==1)]$Size_ret)*sqrt(12)
final_2[3,11] = final_2[1,11]/final_2[2,11]
final_2[4,11] = skewness(log(1+(final_size[which(Size_cut_nyse1==10)]$Size_ret - final_size[which(Size_cut_nyse1==1)]$Size_ret) + Mkt_Rf[which(Date>=197301)]$RF/100))
final_2[5,11] = cor( (final_size[which(Size_cut_nyse1==10)]$Size_ret- final_size[which(Size_cut_nyse1==1)]$Size_ret), (ff_me[,11] - ff_me[,2]))

# Answer 3

final_3 <- matrix(ncol=11,nrow=5)
ff_btm <- data.frame(ff_btm)

for (i in 1:10){
    #port_dm_ret1[i] <- mean(data_4[which(Decile==i)]$DM_Ret)*12
    final_3[1,i] <- mean(final_btm[which(final_btm$btm_cut_nyse1==i)]$btm_ret- Mkt_Rf[which(Date>=197307)]$RF/100)*12
    final_3[2,i] <- sd(final_btm[which(final_btm$btm_cut_nyse1==i)]$btm_ret- Mkt_Rf[which(Date>=197307)]$RF/100)*sqrt(12)
    final_3[3,i] <- final_3[1,i]/final_3[2,i]
    final_3[4,i] <- skewness(log(1+final_btm[which(btm_cut_nyse1==i)]$btm_ret))
    final_3[5,i] <- cor(final_btm[which(btm_cut_nyse1==i)]$btm_ret, ff_btm[(7:540),(i+1)])
    #port_krf[i] <- mean(data_4[which(Decile==i)]$KRF_Ret- data_4[which(Decile==i)]$Rf)
}


final_3[1,11] = mean(final_btm[which(btm_cut_nyse1==10)]$btm_ret- final_btm[which(btm_cut_nyse1==1)]$btm_ret)*12
final_3[2,11] = sd(final_btm[which(btm_cut_nyse1==10)]$btm_ret- final_btm[which(btm_cut_nyse1==1)]$btm_ret)*sqrt(12)
final_3[3,11] = final_3[1,11]/final_3[2,11]
final_3[4,11] = skewness(log(1+(final_btm[which(btm_cut_nyse1==10)]$btm_ret - final_btm[which(btm_cut_nyse1==1)]$btm_ret) + Mkt_Rf[which(Date>=197307)]$RF/100))
final_3[5,11] = cor( (final_btm[which(btm_cut_nyse1==10)]$btm_ret- final_btm[which(btm_cut_nyse1==1)]$btm_ret), (ff_btm[(7:540),11] - ff_btm[(7:540),2])/100)

# Answer 4

a <- unique(as.Date(final_btm$date))[463:534]
plot(a,cumsum(log(1+final_btm[which(btm_cut_nyse1==10)]$btm_ret[463:534])),type='l',ylab="Cum Log Returns",xlab="Year",main="Value: Winners and Losers 2012-2017")
lines(a,cumsum(log(1+final_btm[which(btm_cut_nyse1==1)]$btm_ret[463:534])),col='red')

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

a1 <- unique(as.Date(final_size$date))[469:540]
plot(a1,cumsum(log(1+final_size[which(Size_cut_nyse1==10)]$Size_ret[469:540])),type='l',ylab="Cum Log Returns",xlab="Year",main="Size: Winners and Losers 2012-2017")
lines(a1,cumsum(log(1+final_size[which(Size_cut_nyse1==1)]$Size_ret[469:540])),col='red')

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


# Answer 5

# HML Return and SMB Return 

# Merge inp_comp_crsp_mer_bm and inp_comp_crsp_mer_size 

inp_comp_crsp_mer_bm_5 <- merge(inp_comp_crsp_mer_bm, inp_comp_crsp_mer_size[,c("Size_cut_nyse_med1","Size_ret","date","PERMCO")],by=c("date","PERMCO") )
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[!duplicated(inp_comp_crsp_mer_bm_5,by=c("PERMCO","date")),]

# Start Forming Portfolio

# Port 6 is the final
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port1:= ifelse((btm_cut_nyse_med1==3 & Size_cut_nyse_med1==1),"SV","Rest"),by=c("date")]
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port2:= ifelse((btm_cut_nyse_med1==2 & Size_cut_nyse_med1==1),"SN",port1),by=c("date")]
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port3:= ifelse((btm_cut_nyse_med1==1 & Size_cut_nyse_med1==1),"SG",port2),by=c("date")]

inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port4:= ifelse((btm_cut_nyse_med1==3 & Size_cut_nyse_med1==2),"BV",port3),by=c("date")]
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port5:= ifelse((btm_cut_nyse_med1==2 & Size_cut_nyse_med1==2),"BN",port4),by=c("date")]
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port6:= ifelse((btm_cut_nyse_med1==1 & Size_cut_nyse_med1==2),"BG",port5),by=c("date")]


inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[, port_weight := Lagged_mkt_cap_permco/sum(Lagged_mkt_cap_permco), by = c("port6", "date")]
inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,port_ret := sum(port_weight*Ret_permco), by = c("port6", "date")]
inp_comp_crsp_mer_bm_5$date <- as.Date(inp_comp_crsp_mer_bm_5$date)
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[which(date>="1973-07-31")]
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB_ret := sum(SMB_weight*Ret_permco), by = c("date")]


#inp_crsp2 <- inp_crsp2[,Lagged_mkt_cap_permco := sum(Lagged_mkt_cap), by = .(PERMCO, date)]

#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB_weights:= sum(Ret_permco[port6=="SV"], Ret_permco[port6=="SN"], Ret_permco[port6=="SG"]), by=c("date") ]
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB2:= sum(Ret_permco[port6=="BV"], Ret_permco[port6=="BN"], Ret_permco[port6=="BG"]), by=c("date") ]
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB:= (SMB1-SMB2)/3, by=c("date") ]



#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB1:= sum(Ret_permco[port6=="SV"], Ret_permco[port6=="SN"], Ret_permco[port6=="SG"]), by=c("date") ]
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB2:= sum(Ret_permco[port6=="BV"], Ret_permco[port6=="BN"], Ret_permco[port6=="BG"]), by=c("date") ]
#inp_comp_crsp_mer_bm_5 <- inp_comp_crsp_mer_bm_5[,SMB:= (SMB1-SMB2)/3, by=c("date") ]


#inp_comp_crsp_mer_bm_5[which(inp_comp_crsp_mer_bm_5$date=="1973-01-31")]

sub2 <- inp_comp_crsp_mer_bm_5[,c("date","port6","port_ret")]
sub2 <- unique(sub2)
small_smb <- sub2[which(port6=="SV"| port6=="SN"| port6=="SG")]
big_smb <- sub2[which(port6=="BV"| port6=="BN"| port6=="BG")]

high_hml <- sub2[which(port6=="SV" |  port6=="BV")]
low_hml <- sub2[which(port6=="SG"| port6=="BG")]


#sub3 <- sub2[,SMB1:= SMB_ret[port6=="SV"] + SMB_ret[port6=="SN"] + SMB_ret[port6=="SG"],by=c("date") ]
#sub3 <- sub3[,SMB2:= SMB_ret[port6==BV] + SMB_ret[port6==BN] + SMB_ret[port6==BG],by=c("date") ]
#sub3 <- sub3[, SMB:= (SMB1-SMB2)/3, by=c("date")]
big_smb <- big_smb[, SMB_big:= sum(port_ret)/3 , by=c("date")]
big_smb <- big_smb[,c("date","SMB_big")]
big_smb <- unique(big_smb)

small_smb <- small_smb[, SMB_small:= sum(port_ret)/3 , by=c("date")]
small_smb <- small_smb[,c("date","SMB_small")]
small_smb <- unique(small_smb)

#final_size_t <- sub2[with(sub2, order(mdy(date), port6)),]
#sub2 <- inp_comp_crsp_mer_bm_5[,c("date","port6","SMB_ret")]

high_hml <- high_hml[, HML_high:= sum(port_ret)/2 , by=c("date")]
high_hml <- high_hml[,c("date","HML_high")]
high_hml <- unique(high_hml)

low_hml <- low_hml[, HML_low:= sum(port_ret)/2 , by=c("date")]
low_hml <- low_hml[,c("date","HML_low")]
low_hml <- unique(low_hml)

#cor(Mkt_Rf[which(Date>=197307)]$SMB/100,(small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537]) )
#cor(Mkt_Rf[which(Date>=197307)]$HML/100,(high_hml$HML_high[4:537] - low_hml$HML_low[4:537]) )

a2 <- unique(as.Date(low_hml$date))[4:537]
plot(a2,cumsum(log(1+high_hml$HML_high[4:537])),type='l',ylab="Cum Log Returns",xlab="Year",main="HML Returns Over Time ")
lines(a2,cumsum(log(1+low_hml$HML_low[4:537])),col='red')

legend("topleft", 
       legend = c("High", "Low"), 
       col = c("black","red"), 
       lty = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       #horiz = F , 
       inset = c(0.01, 0.05))

plot(a2,cumsum(log(1+small_smb$SMB_small[4:537])),type='l',ylab="Cum Log Returns",xlab="Year",main="SMB Returns Over Time ")
lines(a2,cumsum(log(1+big_smb$SMB_big[4:537])),col='red')

legend("topleft", 
       legend = c("Small", "Big"), 
       col = c("black","red"), 
       lty = c(1,1), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       #horiz = F , 
       inset = c(0.01, 0.05))

final_4 <- matrix(ncol=2,nrow=5)
#ff_btm <- data.frame(ff_btm)

final_4[1,1] = mean(small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537])*12
final_4[2,1] = sd(small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537])*sqrt(12)
final_4[3,1] = final_4[1,1]/final_4[2,1]
final_4[4,1] = skewness(log(1+ (small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537]) + Mkt_Rf$RF[565:1098])  )
final_4[5,1] = cor(Mkt_Rf[which(Date>=197307)]$SMB/100,(small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537]) )


final_4[1,2] = mean(high_hml$HML_high[4:537] - low_hml$HML_low[4:537])*12
final_4[2,2] = sd(high_hml$HML_high[4:537] - low_hml$HML_low[4:537])*sqrt(12)
final_4[3,2] = final_4[1,2]/final_4[2,2]
final_4[4,2] = skewness(log(1+ (high_hml$HML_high[4:537] - low_hml$HML_low[4:537]) + Mkt_Rf$RF[565:1098])  )
final_4[5,2] = cor(Mkt_Rf[which(Date>=197307)]$HML/100,(high_hml$HML_high[4:537] - low_hml$HML_low[4:537]) )

colnames(final_4) <- c("SMB","HML")
rownames(final_4) <- c("Annualised Excess Mean", "Annualised Excess Volatility", "Annualised SR", "Skewness(Monthly)", "Correlation")


# Final Output table 

y1 <- year(as.Date(big_smb$date[4:537]))
m1 <- month(as.Date(big_smb$date[4:537]))

#year = unique(data_7$year)
#month = unique(data_7$month)
decile = seq(1,10,1)
final_out <- matrix(nrow=5340,ncol=7)

final_out[,1] <- y1
final_out[,2] <- m1
final_out[,3] <- rep(seq(1,10,1),each=534)
#final_out[,4] <- rep(seq(1,10,1),each=534)
final_out[,4] <- final_size$Size_ret[7:540]
final_out[,5] <- final_btm$btm_ret[7:540]
f1 <- small_smb$SMB_small[4:537] - big_smb$SMB_big[4:537]
f2 <- high_hml$HML_high[4:537] - low_hml$HML_low[4:537]

final_out[,6] <- rep(f1,10)
final_out[,7] <- rep(f2,10)

final_out <- data.table(final_out)

colnames(final_out) <- c("Year","Month", "Decile","Size_Ret","BtM_Ret","SMB_Ret","HML_Ret")




