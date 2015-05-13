#  单因子测试
setwd("D:\\working\\OneFactorTest")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)  
library(reshape2)
library(TTR)
library(rCharts)

source('D:/working/Timing/MyFunction.R')
########################################################################################################

channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")

########################################################################################################
# 指数数据
data <- list()
data$SecuMainStock <- tbl(channel, "SecuMain") %>%
  filter(SecuMarket %in% c(83, 90), SecuCategory == 1) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr) %>%
  collect 

data$SecuMainIndex <- tbl(channel, "SecuMain") %>%
  filter(InnerCode %in% c(1, 3145, 4978, 4982, 4088)) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr) %>%
  collect 

data$IndexQuote <- tbl(channel, "QT_IndexQuote") %>%
  semi_join(tbl(channel, "SecuMain") %>% filter(InnerCode %in% c(1, 3145, 4978, 4982, 4088)),
            by = "InnerCode") %>%
  select(InnerCode, TradingDay, PrevClosePrice, OpenPrice, HighPrice, LowPrice, ClosePrice) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay), Return = ClosePrice/PrevClosePrice-1)

data$TradingDay <- tbl(channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83L, IfTradingDay==1L) %>%
  select(TradingDate, IfWeekEnd, IfMonthEnd, IfQuarterEnd, IfYearEnd) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate))
###########################################################################################################

date <- data$TradingDay %>%
  filter(IfWeekEnd == 1) %>%
  arrange(TradingDate) %>%
  mutate(Date = lag(TradingDate, 1), ForecastDate = TradingDate) %>%
  filter(!is.na(Date)) %>%
  select(Date, ForecastDate) 
  

#########################################################################################################
# 信号
signal <- read.csv("Signal.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)
signal_week <- signal %>% 
  semi_join(data$TradingDay %>% filter(IfWeekEnd == 1), by = c("Date" = "TradingDate")) %>%
  arrange(Date) %>%
  mutate(Score = Score(Signal, 8, IsMinusMean = 1)) %>%
  filter(!is.na(Score))
 
return_week <- data$IndexQuote %>%
  filter(InnerCode == 3145) %>%
  semi_join(data$TradingDay %>% filter(IfWeekEnd == 1), by = c("TradingDay" = "TradingDate")) %>%
  arrange(TradingDay) %>%
  mutate(LagClosePrice = lag(ClosePrice, 1)) %>%
  mutate(Return = ClosePrice/LagClosePrice -1)

momentums <- data$IndexQuote %>%
  filter(InnerCode == 3145) %>%
  arrange(TradingDay) %>%
  mutate(Momentum3d = Momentum(Return, 3), Momentum5d = Momentum(Return, 5),
         Momentum10d = Momentum(Return, 10), Momentum15d = Momentum(Return, 15)) %>%
  semi_join(data$TradingDay %>% filter(IfWeekEnd == 1), by = c("TradingDay" = "TradingDate")) %>%
  arrange(TradingDay) %>%
  mutate(Momentum3dScore = Score(Momentum3d, 8, IsMinusMean = 0),
         Momentum5dScore = Score(Momentum5d, 8, IsMinusMean = 0),
         Momentum10dScore = Score(Momentum10d, 8, IsMinusMean = 0),
         Momentum15dScore = Score(Momentum15d, 8, IsMinusMean = 0)) %>%
  select(TradingDay, Momentum3dScore, Momentum5dScore, Momentum10dScore, Momentum15dScore)

  
final_signal <- signal_week %>% 
  left_join(date, by = "Date") %>%
  left_join(return_week %>% select(TradingDay, Return), by = c("ForecastDate" = "TradingDay")) %>%
  filter(!is.na(Return)) %>%
  left_join(momentums, by = c("Date" = "TradingDay"))
  
final_return <- final_signal %>%
  mutate(Rsquare = ifelse(Score > 0, Return, ifelse(Score < 0, -Return, 0)),
         RsquareMomentum3d = ifelse(Score > 0, ifelse(Momentum3dScore > 0, Return, ifelse(Momentum3dScore < 0, -Return, 0)), 0),
         RsquareMomentum5d = ifelse(Score > 0, ifelse(Momentum5dScore > 0, Return, ifelse(Momentum5dScore < 0, -Return, 0)), 0),
         RsquareMomentum10d = ifelse(Score > 0, ifelse(Momentum10dScore > 0, Return, ifelse(Momentum10dScore < 0, -Return, 0)), 0),
         RsquareMomentum15d = ifelse(Score > 0, ifelse(Momentum15dScore > 0, Return, ifelse(Momentum15dScore < 0, -Return, 0)), 0)) %>%
  select(Date, Rsquare, RsquareMomentum3d, RsquareMomentum5d, RsquareMomentum10d, RsquareMomentum15d)
         
PlotCumlateReturn(final_return)
TotalPerformance(final_return)

