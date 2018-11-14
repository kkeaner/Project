Sept30b <- read.csv(file.choose( ))
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
nSept30b_go3a <- filter(nSept30b_go3, Source != "NA" )
nSept30b_go3b <- filter(nSept30b_go3, Source.Port != "NA" )
lost <- lm(formula = Length ~ Protocol, data = nSept30b_go2)
data.frame(summary(lost)$coef[summary(lost)$coef[,4] < 1e-4, 4])
frost <- lm(formula = Length ~ Source, data = nSept30b_go2)
data.frame(summary(frost)$coef[summary(frost)$coef[,4] < 1e-4, 4])
proto <- subset(nSept30b_go2, Protocol %in% c("DHCP", "DISTCC", "GQUIC", "HTTP", "HTTP/XML", "IPA" ,"MIH" , "RSIP" ,"SABP" ,"SIP", "SSDP", "UDT")) 
roto <- subset(nSept30b_go2, Protocol %in% c("13.107.3.128", "176.31.232.79", "192.168.1.143", "192.168.1.223", "192.168.1.241", "203.5.76.210" ,"203.5.76.213" , "23.23.116.35" ,"40.78.98.202" ,"52.62.218.228", "68.232.45.201", "74.125.34.101", "91.121.58.223"))
temp <- merge(proto, roto, all = TRUE)
toast <- lm(formula = Length ~ Source, data = temp)
roast <- lm(formula = Length ~ Protocol, data = temp)
data.frame(summary(toast)$coef[summary(toast)$coef[,4] < 1e-2, 4])
data.frame(summary(roast)$coef[summary(roast)$coef[,4] < 1e-2, 4])




