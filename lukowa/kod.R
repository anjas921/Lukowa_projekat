for (i in nrow(TCEHY)) {
  TCEHY$LogReturn_Open_w<-"/"
  TCEHY$LogReturn_High_w<-"/"
  TCEHY$LogReturn_Low_w<-"/"
  TCEHY$LogReturn_Last_w<-"/"
}

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
TCEHY$LogReturn_Open[i]<-0
TCEHY$LogReturn_High[i]<-0
TCEHY$LogReturn_Low[i]<-0
TCEHY$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY)){
  TCEHY$LogReturn_Open[i]<-ln(TCEHY$Open[i]/TCEHY$Open[i-1])
  TCEHY$LogReturn_High[i]<-ln(TCEHY$High[i]/TCEHY$High[i-1])
  TCEHY$LogReturn_Low[i]<-ln(TCEHY$Low[i]/TCEHY$Low[i-1])
  TCEHY$LogReturn_Last[i]<-ln(TCEHY$Last[i]/TCEHY$Last[i-1])
}



# NEDELJNI LOG RETURN 
for(i in 9:nrow(TCEHY)){
            if(weekdays.POSIXt(TCEHY$`Date (GMT)`[i])=="Monday"){
                              for(j in 1:5){
                                if(weekdays.POSIXt(TCEHY$`Date (GMT)`[i-j])=="Monday"){
                                  TCEHY$LogReturn_Open_w[i]<-ln(TCEHY$Open[i]/TCEHY$Open[j])
                                  TCEHY$LogReturn_High_w[i]<-ln(TCEHY$High[i]/TCEHY$High[j])
                                  TCEHY$LogReturn_Low_w[i]<-ln(TCEHY$Low[i]/TCEHY$Low[j])
                                  TCEHY$LogReturn_Last_w[i]<-ln(TCEHY$Last[i]/TCEHY$Last[j])
                                }
                              }
                            }
}

# MESECNI LOG RETURN

monthly_returns_TCEHY_Open <- aggregate(TCEHY$LogReturn_Open, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_High <- aggregate(TCEHY$LogReturn_High, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_Low <- aggregate(TCEHY$LogReturn_Low, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_Last <- aggregate(TCEHY$LogReturn_Last, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_TCEHY_Open,monthly_returns_TCEHY_High,by="Group.1")
low_last<-merge(monthly_returns_TCEHY_Low,monthly_returns_TCEHY_Last,by="Group.1")
monthly_returns_TCEHY<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_TCEHY)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_TCEHY_Open)
remove(monthly_returns_TCEHY_High)
remove(monthly_returns_TCEHY_Low)
remove(monthly_returns_TCEHY_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_TCEHY_Open <- aggregate(TCEHY$LogReturn_Open, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_High <- aggregate(TCEHY$LogReturn_High, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_Low <- aggregate(TCEHY$LogReturn_Low, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_Last <- aggregate(TCEHY$LogReturn_Last, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_TCEHY_Open,yearly_returns_TCEHY_High,by="Group.1")
low_last<-merge(yearly_returns_TCEHY_Low,yearly_returns_TCEHY_Last,by="Group.1")
yearly_returns_TCEHY<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_TCEHY)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_TCEHY_Open)
remove(yearly_returns_TCEHY_High)
remove(yearly_returns_TCEHY_Low)
remove(yearly_returns_TCEHY_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_TCEHY$Open)
volatility_high <- sd(yearly_returns_TCEHY$High)
volatility_low <- sd(yearly_returns_TCEHY$Low)
volatility_last <- sd(yearly_returns_TCEHY$Last)

yearly_returns_TCEHY$Volatility_Open[1] <- "/"
yearly_returns_TCEHY$Volatility_Open[1] <- sd(yearly_returns_TCEHY$Open)
yearly_returns_TCEHY$Volatility_High[1] <- "/"
yearly_returns_TCEHY$Volatility_High[1] <- sd(yearly_returns_TCEHY$High)
yearly_returns_TCEHY$Volatility_Low[1] <- "/"
yearly_returns_TCEHY$Volatility_Low[1] <- sd(yearly_returns_TCEHY$Low)
yearly_returns_TCEHY$Volatility_Last[1] <- "/"
yearly_returns_TCEHY$Volatility_Last[1] <- sd(yearly_returns_TCEHY$Last)
