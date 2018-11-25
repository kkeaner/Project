Sept30b <- read.csv(file.choose( ))
library (dplyr)
library(caret)
library(ggplot2)
library(mosaic)
# data description
alpa_Sept30b <- Sept30b %>% filter(grepl("[a-z]", Source))
num_Sept30b <- Sept30b %>% filter(!grepl("[a-z]", Source))
Stateless<- subset(num_Sept30b, is.na(Source.Port))
Stateful<- subset(num_Sept30b, !is.na(Source.Port))
Stateful$state <- 1
Stateless$state <- 0
N_Sept30b <<- rbind(Stateful, Stateless)
nSept30b_go1 <- filter(N_Sept30b, Source != "::" )
nSept30b_go2 <- filter(nSept30b_go1, Source != "0.0.0.0" )
nSept30b_go3 <- filter(nSept30b_go2, Source != "8.8.8.8" )
Mattrix_2 <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
Mattrix_3 <- subset(Mattrix_2, select = -c(Source.Port, Destination.Port, Window.size.value))
Mattrix_4 <- subset(Mattrix_3, Protocol %in% c("CLASSIC-STUN", "DNS", "HTTP", "HTTP/XML", "ICMP" ,"Omni-Path" ,"SSDP","STUN","TCP","TLSv1","TLSv1.2","UDP","UDT") )
Mattrix_5 = subset(Mattrix_4, select = -c(No.,Info ))
#Sept30b had 802226 objects and 13 variables after being converted to a .CSV and imported
#Mattrix_2 was an implementation of only the 6 IP addresses for the 6 devices that produced the most traffic
#Mattrix_3 was the removal of the columns Source.Port, Destination.Port, Window.size.value in order to help processing on lab and home computer and the addition of another column of type factor in order for ease of viewing and understandability the names of the 6 IOT devices corresponding the MAC addresses of those devices elsewhere in the data frame
#Mattrix_4 is the inclusion of Protocols that are picked up as being used in with the IO devices in the subset Mattrix_3
#Mattrix_5 was the removal of the columns Info and No. in order to help processing on lab and home computer
#Mattrix_5 had 311499 objects and 10 variables (large reduction)
tally(~Source.1, data = Sept30b, format = "percent")
# from here we get the top talkers
barchart(tally(~Source.1, data = Sept30b, format = "proportion"))
#graphical representation
multi <- lm(Length ~ Source + Protocol + Time.to.live, data = Mattrix_5)
vif(multi)
hulti <- lm(Length ~ Type + Protocol + Time.to.live, data = Mattrix_5)
vif(hulti)
hulti <- lm(Length ~ Type + Protocol + Time.to.live, data = Mattrix_5)
vif(hulti)
# linear regressions and analysis
first <- lm(Length ~ Protocol, data = Mattrix_5)
summary(first)
second <- lm(Length ~ Type, data = Mattrix_5)
summary(second)
third <- lm(Length ~ Time.to.live, data = Mattrix_5)
summary(third)
anova(first)
anova(second)
anova(third)
# mulitple linear regression based on Multi-collinerity
first_multi <- lm(Length ~ Protocol + Source + Time.to.live, data = Mattrix_5)
summary(first_multi)
kruskal.test(x = Mattrix_5$Length, g = as.factor(Mattrix_5$Source))
aggregate(Length~Source.1, data = Mattrix_5,FUN=mean, na.rm=T)