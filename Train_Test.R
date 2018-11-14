attach(nSept30b_go3)
inTrain <- createDataPartition(y=Protocol, p=0.75, list=FALSE)
summary(nSept30b_go3[inTrain,])
training <- nSept30b_go3[inTrain,]
test <- nSept30b_go3[-inTrain,]
library(dplyr)
tcp <- dplyr::filter(nSept30b_go3,  !is.na(Source.Port))
Train <- createDataPartition(y=Protocol, p=0.75, list=FALSE)
#Warning message:
#In createDataPartition(y = Protocol, p = 0.75, list = FALSE) :
#Some classes have no records ( ADwin Config, ARP, BROWSER, CLASSIC-STUN, DHCP, DHCPv6, DNS, EAPOL, ESP, 
#GQUIC, ICMP, ICMPv6, IGMPv2, IGMPv3, ISAKMP, MDNS, NBNS, NTP, SSDP, UDP, UDPENCAP, UDT, XID ) and these will be ignored
summary(tcp[Train,])
trainin <- tcp[Train,]
tester <- tcp[-Train,]
knnFit <- train(Protocol ~ ., 
+                                 data = trainin, 
+                                 method="knn",  
+                                 tuneGrid=data.frame(k=1:20))
# Error: cannot allocate vector of size 1517.6 Gb