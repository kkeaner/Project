Sept30b <- read.csv(file.choose( ))
library (dplyr)
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
Mattrix <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.1", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240"))
udp <- dplyr::filter(Mattrix,  is.na(Source.Port))
tcp <- dplyr::filter(Mattrix,  !is.na(Source.Port))
Mattrix <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.1", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
lin_reg_1 <- lm(Length ~ Source, data = Mattrix)
summary(lin_reg_1)
lin_reg_2 <- lm(Length ~ Protocol, data = Mattrix)
summary(lin_reg_2)
# second Matrix minus the gateway for protocol elimination due to internal broadcasts, giving us top 6 talkers
Mattrix_2 <- subset(nSept30b_go3, Source %in% c("192.168.1.249", "192.168.1.106", "192.168.1.223", "192.168.1.193", "192.168.1.143" ,"192.168.1.240") )
lin_reg_3 <- lm(Length ~ Protocol, data = Mattrix_2)
lin_reg_4 <- lm(Length ~ Source, data = Mattrix_2)
summary(lin_reg_3)
summary(lin_reg_4)
data.frame(summary(lin_reg_3)$coef[summary(lin_reg_3)$coef[,4] < 2e-8, 4])
# this gives the 6 most statistically significant protocols
lin_reg_5 <- lm(Length ~ Destination, data = Mattrix_2)
summary(lin_reg_5)
# this tells us which destinations that the internal devices are speaking to.....many!!!
# subMat <- subset(Mattrix_2, Protocol %in% c("CLASSIC-STUN", "DNS", "ICMP", "IGMPv2", "IGMPv3", "NTP") )
# when the above is executed to pick out statistically significant protocols only UDP was returned.....this cannot be used

