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
nSept30b_go3 <- filter(nSept30b_go3, Destination != "8.8.8.8" )
Mattrix_2 <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
Mattrix_3 <- subset(Mattrix_2, select = -c(Source.Port, Destination.Port, Window.size.value))
Mattrix_3$Type <- NA
Mattrix_3$Type[Mattrix_3$Source.1 == "30:8c:fb:2f:e4:b2"] <- "Drop Camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:83:28:11"] <- "Wemo Motion Sensor"
Mattrix_3$Type[Mattrix_3$Source.1 == "f4:f2:6d:93:51:f1"] <- "TP Link camera"
Mattrix_3$Type[Mattrix_3$Source.1 == "00:16:6c:ab:6b:88"] <- "Samsung SmartCam"
Mattrix_3$Type[Mattrix_3$Source.1 == "ec:1a:59:79:f4:89"] <- "Belkin Wemo Switch"
Mattrix_3$Type[Mattrix_3$Source.1 == "44:65:0d:56:cc:d3"] <- "Amazon Echo"  
Mattrix_3$state <- as.factor(Mattrix_3$state)
Mattrix_3$Type <- as.factor(Mattrix_3$Type)
Mattrix_4 <- subset(Mattrix_3, Protocol %in% c("CLASSIC-STUN", "DNS", "HTTP", "HTTP/XML", "ICMP" ,"Omni-Path" ,"SSDP","STUN","TCP","TLSv1","TLSv1.2","UDP","UDT") )
Mattrix_5 = subset(Mattrix_4, select = -c(No.,Info ))

# Subset Variable Selection
library(leaps)
Mat <- subset(Stateful, select = c(Time, Length, Source.Port, Destination.Port, Window.size.value ))
regfit <- regsubsets(Length ~ ., Mat)
summary(regfit)
regsum <- summary(regfit)
plot(regsum$rsq,xlab = "Number of Variables", ylab = "Rsquare", type = "l")
plot(regsum$rss,xlab = "Number of Variables", ylab = "RSS", type = "l")
which.max(regsum$adjr2)
plot(regsum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.min(regsum$cp)
which.min(regsum$bic)
plot(regfit, scale = "r2")
plot(regfit, scale = "adjr2")
plot(regfit, scale = "Cp")
plot(regfit, scale = "bic")
#When Destination Port is the Response
regfit <- regsubsets(Destination.Port ~ ., Mat)
summary(regfit)
regsum <- summary(regfit)
plot(regsum$rsq,xlab = "Number of Variables", ylab = "Rsquare", type = "l")
plot(regsum$rss,xlab = "Number of Variables", ylab = "RSS", type = "l")
which.max(regsum$adjr2)
plot(regsum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.min(regsum$cp)
which.min(regsum$bic)
plot(regfit, scale = "r2")
plot(regfit, scale = "adjr2")
#when Source Port is the Response
regfit <- regsubsets(Source.Port ~ ., Mat)
summary(regfit)
regsum <- summary(regfit)
plot(regsum$rsq,xlab = "Number of Variables", ylab = "Rsquare", type = "l")
plot(regsum$rss,xlab = "Number of Variables", ylab = "RSS", type = "l")
which.max(regsum$adjr2)
plot(regsum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.min(regsum$cp)
which.min(regsum$bic)
plot(regfit, scale = "r2")
plot(regfit, scale = "Cp")
plot(regfit, scale = "bic")
# when time is the response variable
regfit <- regsubsets(Time ~ ., Mat)
summary(regfit)
regsum <- summary(regfit)
plot(regsum$rsq,xlab = "Number of Variables", ylab = "Rsquare", type = "l")
plot(regsum$rss,xlab = "Number of Variables", ylab = "RSS", type = "l")
which.max(regsum$adjr2)
plot(regsum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.min(regsum$cp)
which.min(regsum$bic)
plot(regfit, scale = "r2")
plot(regfit, scale = "Cp")
plot(regfit, scale = "bic")
# this indicates that Length is the best single predictor if other variables that cannot 
# predict on behalf of stateless protocols are taken out (numeric variables).  Length is not 
# as much of a factor when the response is Destination but that is the only time (as we are not
# able to record it as recieved) but still Time drops off before Length and length is predominant 
# every high BIC and residual squared result as well as next to lowest CP except when Length itself # is the response variable
# Linear Regression and Multicolinearity
second_multi <- lm(Length~ Time + Source + Protocol + Time.to.live, data = Mattrix_5)
vif(second_multi)
first <- lm(Length ~ Protocol, data = Mattrix_5)
summary(first)
second <- lm(Length ~ Type, data = Mattrix_5)
summary(second)
third <- lm(Length ~ Time.to.live, data = Mattrix_5)
summary(third)
anova(first)
anova(second)
anova(third)
# Stepwise Regression - this code did not work on my local machine but worked on the AWS instance of Rstudio??
library(FSA)
library(psych)
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Length ~ 1, 
                 data=Data.num)
model.full = lm(Length ~ Time + Source.Port + Destination.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=6607768
#Length ~ Source.Port + Time + Window.size.value + Destination.Port
#
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Source.Port ~ 1, 
                 data=Data.num)
model.full = lm(Source.Port ~ Time + Length + Destination.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10710554
#Source.Port ~ Destination.Port + Length + Window.size.value + 
#    Time
#
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Destination.Port ~ 1, 
                 data=Data.num)
model.full = lm(Destination.Port ~ Time + Length + Source.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10724696
#Destination.Port ~ Source.Port + Window.size.value + Length + 
#    Time
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Time ~ 1, 
                 data=Data.num)
model.full = lm(Time ~ Destination.Port + Length + Source.Port + Window.size.value, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=11260302
#Time ~ Length + Destination.Port + Window.size.value + Source.Port
#
Data.num = select(Stateful,
           Time,
           Length,
           Source.Port,
           Destination.Port,           
           Window.size.value)
corr.test(Data.num, 
           use = "pairwise",
           method="pearson",
           adjust="none",     # Can adjust p-values; see ?p.adjust for options
           alpha=.05)
model.null = lm(Window.size.value ~ 1, 
                 data=Data.num)
model.full = lm(Window.size.value ~ Destination.Port + Length + Source.Port + Time, data = Data.num)
step(model.null,
      scope = list(upper=model.full),
              direction="both",
data=Data.num)
#
#Step:  AIC=10219203
# Window.size.value ~ Source.Port + Length + Destination.Port + Time
xtabs(~Source.1 + state, data = Mattrix_3)
#                          state
#Source.1                 0      1
#  00:16:6c:ab:6b:88  23127  15156
#  00:24:e4:11:18:a8      0      0
#  00:24:e4:1b:6f:96      0      0
#  00:24:e4:20:28:c6      0      0
#  00:62:6e:51:27:2e      0      0
#  08:21:ef:3b:fc:e3      0      0
#  14:cc:20:51:33:ea      0      0
#  18:b4:30:25:be:e4      0      0
#  18:b7:9e:02:20:44      0      0
#  30:8c:fb:2f:e4:b2      4 108822
#  44:65:0d:56:cc:d3  16103  13636
#  50:c7:bf:00:56:39      0      0
#  70:5a:0f:e4:9b:c0      0      0
#  70:ee:50:03:b8:ac      0      0
#  70:ee:50:18:34:43      0      0
#  74:c6:3b:29:d7:1d      0      0
#  d0:52:a8:00:67:5e      0      0
#  d0:73:d5:01:83:08      0      0
#  e0:76:d0:33:bb:85      0      0
#  ec:1a:59:79:f4:89   6774  23972
#  ec:1a:59:83:28:11   5381  33009
#  f4:f2:6d:93:51:f1   9371  59158
xtabs(~Destination.1 + state, data = Mattrix_3)
#                          state
#Destination.1            0      1
#  00:16:6c:ab:6b:88      0   6770
#  00:24:e4:11:18:a8      0      0
#  00:24:e4:1b:6f:96      0      0
#  00:24:e4:20:28:c6      0      0
#  00:62:6e:51:27:2e      0      0
#  01:00:5e:00:00:01      0      0
#  01:00:5e:00:00:16     97      0
#  01:00:5e:00:00:fb    675      0
#  01:00:5e:00:01:3c      0      0
#  01:00:5e:7f:ff:fa  21896      0
#  08:21:ef:3b:fc:e3     39    175
#  14:cc:20:51:33:e9      0      0
#  14:cc:20:51:33:ea  34285 193936
#  18:b4:30:25:be:e4      0      0
#  18:b7:9e:02:20:44      0      0
#  30:8c:fb:2f:e4:b2      0      0
#  33:33:00:00:00:01      0      0
#  33:33:00:00:00:02      0      0
#  33:33:00:00:00:0c      0      0
#  33:33:00:00:00:16      0      0
#  33:33:00:00:00:fb      0      0
#  33:33:00:01:00:02      0      0
#  33:33:00:01:00:03      0      0
#  33:33:ff:00:00:00      0      0
#  33:33:ff:00:00:01      0      0
#  33:33:ff:00:0e:e2      0      0
#  33:33:ff:01:83:08      0      0
#  33:33:ff:18:34:43      0      0
#  33:33:ff:25:be:e4      0      0
#  33:33:ff:33:bb:85      0      0
#  33:33:ff:3b:fc:e3      0      0
#  33:33:ff:51:33:ea      0      0
#  33:33:ff:62:42:ec      0      0
#  33:33:ff:79:f4:89      0      0
#  33:33:ff:83:28:11      0      0
#  33:33:ff:ab:6b:88      0      0
#  33:33:ff:c0:e9:84      0      0
#  33:33:ff:e4:9b:c0      0      0
#  33:33:ff:e5:66:2d      0      0
#  44:65:0d:56:cc:d3      0      0
#  50:c7:bf:00:56:39      0      0
#  70:5a:0f:e4:9b:c0      0      0
#  70:ee:50:03:b8:ac      0      0
#  70:ee:50:18:34:43      0      0
#  74:c6:3b:29:d7:1d      0      0
#  d0:52:a8:00:67:5e      0      0
#  d0:73:d5:01:83:08      0      0
#  e0:76:d0:33:bb:85      0      0
#  ec:1a:59:79:f4:89      0  23211
#  ec:1a:59:83:28:11   3768  29661
#  f4:f2:6d:93:51:f1      0      0
#  ff:ff:ff:ff:ff:ff      0      0
# 
  xtabs(~Protocol + state, data = Mattrix_3)
#                     state
#Protocol            0      1
#  ADwin Config      0      0
#  ARP               0      0
#  AX4000            0     14
#  BROWSER           0      0
#  CLASSIC-STUN   2417      0
#  DHCP             16      0
#  DHCPv6            0      0
#  DIAMETER          0      5
#  DISTCC            0     24
#  DNS            3080      0
#  EAPOL             0      0
# ESP               0      0
#  Gearman           0      3
#  GQUIC             0      0
#  HTTP              0   5520
#  HTTP/XML          0   4238
#  ICMP          13931      0
#  ICMPv6            0      0
#  IGMPv2         1113      0
#  IGMPv3           97      0
#  IPA               0     33
#  ISAKMP            0      0
#  MDNS              0      0
#  MIH               0     17
#  NBNS              0      0
#  NTP            1581      0
#  Omni-Path         0    204
#  RSIP              0     25
#  SABP              0     22
#  SIP               0      0
#  SSDP          22770      0
#  STUN              0    462
#  TCP               0 129234
#  TCPCL             0      7
#  TFP over TCP      0      4
#  TLSv1             0 109403
#  TLSv1.2           0   4485
#  UDP            8988      0
#  UDPENCAP          0      0
#  UDT            6767      0
#  ULP               0      0
#  WOW               0     12
#  XID               0      0
#  XMPP/XML          0     41
#

# Generalized Linear Regression
library(caTools)
glmtest_c <- glm(state~ Length + Source, family = "binomial", Mattrix_3)
pred <- predict(glmtest_c, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
table(p_class, test[["state"]])
confusionMatrix(table(p_class, test [["state"]]))
colAUC(pred, test[["state"]], plotROC = TRUE) 
# accuracy of approx. 85%
glmtest_d <- glm(state ~ Length + Destination.1, family = "binomial", Mattrix_3)
pred <- predict(glmtest_d, test, type = "response")
p_class <- ifelse(pred > .50, "1", "0")
table (p_class)
table(p_class, test[["state"]])
confusionMatrix(table(p_class, test [["state"]]))
colAUC(pred, test[["state"]], plotROC = TRUE) 
# accuracy of approx 88%
#
#
