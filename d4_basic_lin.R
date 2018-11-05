Sept30b <- read.csv(file.choose( ))
# had done investigative work on day 4 previous, however this is a re-do 
# import of csv of data from PCAP file that has had name resolution 
# removed from source IP's.  Slightly different than previous data 
View(Sept30b)
glimpse(Sept30b)
alpa_Sept30b <- Sept30b %>% filter(grepl("[a-z]", Source))
num_Sept30b <- Sept30b %>% filter(!grepl("[a-z]", Source))
# creating data frames for traffic that have name resolution still
# and is traffic from physical device to physical device using ARP
# and another for traffic that is from IP addresses in Source
Stateless<- subset(num_Sept30b, is.na(Source.Port))
Stateful<- subset(num_Sept30b, !is.na(Source.Port))
Stateful$state <- 1
Stateless$state <- 0
N_Sept30b <<- rbind(Stateful, Stateless)
# creating a dummy column in order to be able to distinguish between 
# a 1 for TCP traffic and a 0 for UDP traffic
str(N_Sept30b)
summary(N_Sept30b)
nSept30b_go1 <- filter(N_Sept30b, Source != "::" )
nSept30b_go2 <- filter(N_Sept30b, Source != "0.0.0.0" )
nSept30b_go3 <- filter(N_Sept30b, Source != "8.8.8.8" )
#removing all clutter and traffic that has no meaning
glimpse(nSept30b_go3)
fair <- lm(Window.size.value ~ Source, data = nSept30b_go3)
summary(fair)
fairer <- lm(Window.size.value ~ Protocol, data = nSept30b_go3)
summary(fairer)
fairest <- lm(Window.size.value ~ Protocol + Source, data = nSept30b_go3)
summary(fairest)
fairly <- lm(Length ~ state, data = nSept30b_go3)
summary(fairly)
fairlly <- lm(Window.size.value ~ state, data = nSept30b_go3)
summary(fairlly)
fairly <- lm(Length ~ Protocol + state, data = nSept30b_go3)
summary(fairly)
fairrly <- lm(Length ~ state + Protocol, data = nSept30b_go3)
summary(fairrly)
ffairrlly <- lm(state ~ Source, data = nSept30b_go3)
summary(ffairrlly)
nextfairest <- lm(Length ~ Source, data = nSept30b_go3)
# testing of different models to see which works best.  
rparter <- rpart(Window.size.value ~ Protocol, data = nSept30b_go3)
summary(rparter)
# having a look at recursive partitioning
# best linear regression model so far
fairest <- lm(Window.size.value ~ Protocol + Source, data = nSept30b_go3)
summary(fairest)
coef(summary(fairest))
anova(fairest)












