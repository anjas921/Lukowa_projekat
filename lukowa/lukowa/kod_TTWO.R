
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
TTWO <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="TTWO")


# DESKRIPTIVNA STATISTIKA

TTWO_descriptive_statistics <- data.frame(open=mean(TTWO$Open),high=mean(TTWO$High),low=mean(TTWO$Low),last=mean(TTWO$Last)) 
TTWO_descriptive_statistics[2,] <- c(median(TTWO$Open),median(TTWO$High),median(TTWO$Low),median(TTWO$Last))
library(moments)
TTWO_descriptive_statistics[3,] <- c(skewness(TTWO$Open),skewness(TTWO$High),skewness(TTWO$Low),skewness(TTWO$Last))
TTWO_descriptive_statistics[4,] <- c(kurtosis(TTWO$Open),kurtosis(TTWO$High),kurtosis(TTWO$Low),kurtosis(TTWO$Last))

rownames(TTWO_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
TTWO$LogReturn_Open<-0
TTWO$LogReturn_High<-0
TTWO$LogReturn_Low<-0
TTWO$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO)){
  TTWO$LogReturn_Open[i]<-ln(TTWO$Open[i]/TTWO$Open[i-1])
  TTWO$LogReturn_High[i]<-ln(TTWO$High[i]/TTWO$High[i-1])
  TTWO$LogReturn_Low[i]<-ln(TTWO$Low[i]/TTWO$Low[i-1])
  TTWO$LogReturn_Last[i]<-ln(TTWO$Last[i]/TTWO$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(TTWO)) {
  TTWO$LogReturn_Open_w<-"/"
  TTWO$LogReturn_High_w<-"/"
  TTWO$LogReturn_Low_w<-"/"
  TTWO$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(TTWO)){
  if(weekdays.POSIXt(TTWO$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(TTWO$`Date (GMT)`[i-j])=="Monday"){
        TTWO$LogReturn_Open_w[i]<-ln(TTWO$Open[i]/TTWO$Open[j])
        TTWO$LogReturn_High_w[i]<-ln(TTWO$High[i]/TTWO$High[j])
        TTWO$LogReturn_Low_w[i]<-ln(TTWO$Low[i]/TTWO$Low[j])
        TTWO$LogReturn_Last_w[i]<-ln(TTWO$Last[i]/TTWO$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_TTWO_Open,monthly_returns_TTWO_High,by="Group.1")
low_last<-merge(monthly_returns_TTWO_Low,monthly_returns_TTWO_Last,by="Group.1")
monthly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_TTWO)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_TTWO_Open)
remove(monthly_returns_TTWO_High)
remove(monthly_returns_TTWO_Low)
remove(monthly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

# GOTTWONJI LOG RETURN

yearly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_TTWO_Open,yearly_returns_TTWO_High,by="Group.1")
low_last<-merge(yearly_returns_TTWO_Low,yearly_returns_TTWO_Last,by="Group.1")
yearly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_TTWO)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_TTWO_Open)
remove(yearly_returns_TTWO_High)
remove(yearly_returns_TTWO_Low)
remove(yearly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_TTWO$Open)
volatility_high <- sd(yearly_returns_TTWO$High)
volatility_low <- sd(yearly_returns_TTWO$Low)
volatility_last <- sd(yearly_returns_TTWO$Last)

yearly_returns_TTWO$Volatility_Open[1] <- "/"
yearly_returns_TTWO$Volatility_Open[1] <- sd(yearly_returns_TTWO$Open)
yearly_returns_TTWO$Volatility_High[1] <- "/"
yearly_returns_TTWO$Volatility_High[1] <- sd(yearly_returns_TTWO$High)
yearly_returns_TTWO$Volatility_Low[1] <- "/"
yearly_returns_TTWO$Volatility_Low[1] <- sd(yearly_returns_TTWO$Low)
yearly_returns_TTWO$Volatility_Last[1] <- "/"
yearly_returns_TTWO$Volatility_Last[1] <- sd(yearly_returns_TTWO$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(TTWO)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO <- plot_ly(data = TTWO, type = "candlestick",
                   x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                   increasing = list(fillcolor = "green", line = list(color = "green")),
                   decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO <- fig_TTWO %>% layout(title = "TTWO Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_d <- plot_ly(data = TTWO, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_d <- fig_TTWO_lr_d %>% layout(title = "TTWO Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_w <- plot_ly(data = TTWO, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_w <- fig_TTWO_lr_w %>% layout(title = "TTWO Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_TTWO$Month<- as.character(monthly_returns_TTWO$Month)

# Create a candlestick chart
fig_TTWO_lr_m <- plot_ly(data = monthly_returns_TTWO, type = "candlestick",
                        x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_m <- fig_TTWO_lr_m %>% layout(title = "TTWO Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_TTWO$Year<- as.character(yearly_returns_TTWO$Year)

# Create a candlestick chart
fig_TTWO_lr_y <- plot_ly(data = yearly_returns_TTWO, type = "candlestick",
                        x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_y <- fig_TTWO_lr_y %>% layout(title = "TTWO Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
TTWO$MA5 <- rollmean(TTWO$Last, k = 5, fill = NA)
TTWO$MA21 <- rollmean(TTWO$Last, k = 21, fill = NA)
TTWO$MA63 <- rollmean(TTWO$Last, k = 63, fill = NA)

ggplot(TTWO, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(TTWO, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "dashed") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "dashed") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "dashed") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------













