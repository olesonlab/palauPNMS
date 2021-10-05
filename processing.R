#### Resident Data Exploratory Analysis

setwd("C:/Users/Rachel/Google Drive/Palau sanctuary/Surveys/resident/Final data")
data <- read.csv("C:/Users/Rachel/Google Drive/Palau sanctuary/Surveys/resident/Final data/for_R.csv")

#omit Doramae and Pam's surveys
data <- data[ which(data$Q20 !='Doramae'),]
data <- data[ which(data$Q20 !='Doramae '),]
data <- data[ which(data$Q20 !='Pam'),]

#exclude surveys based on QA/QC timing criteria
data <- data[ which(data$valid_8_3min == 1),]

#rename/reorder
data$Q2<-gsub(3,"male",data$Q2)
data$Q2<-gsub(4,"female",data$Q2)

#subsets of data based on urban/rural
data_U<-data[which(data$urban == "urban"),]
data_R<-data[which(data$urban == "rural"),]
data_P<-data[which(data$urban == "peri"),]

#######################################################################################################
#number of respondents, male/female
aggregate(data$Q1523,by=list(Area=data$urban),FUN=length)
table(data$urban)
table1<-table(data$urban,data$Q2)
prop.table(table1)
barplot(table1, beside=T,legend = c("peri","rural","urban"))

#income levels
data$Q12 <- factor(data$Q12, levels = c("<$4,999","$5,000-$9,999","$10,000-$29,999","$30,000-$49,999","$50,000-$74,999",">$75,000"))
income_table<-table(data$urban,data$Q12)
income_table<-prop.table(income_table, margin=1)
barplot(income_table, beside=TRUE,legend = c("peri","rural","urban"), ylab="proportion", xlab="income level")

#religions
data$Q14 <- factor(data$Q14, levels = c("Catholic","Protestant","SDA","Modekngei","JW","LDS","Muslim","None","Other"))
table_rel<-table(data$urban,data$Q14)
table_rel<-prop.table(table_rel, margin=1)
write.csv(table_rel, "table_rel.csv")


##In the last week, what percentage of the fish your household ate was: [based on respondent proportion estimates, not consumption history]
par(mfrow=c(3,3), mar=c(5,2,2,2))
hist(data_U$Q1081_1_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% caught by household, urban")
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_R$Q1081_1_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% caught by household, rural",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_P$Q1081_1_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% caught by household, peri",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))

hist(data_U$Q1081_2_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% purchased, urban", freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_R$Q1081_2_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% purchased, rural",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_P$Q1081_2_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% purchased, peri",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))


hist(data_U$Q1081_3_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% gifted, urban", freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_R$Q1081_3_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% gifted, rural",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
hist(data_P$Q1081_3_1, breaks=c(0,25,50,75,100),xaxt='n',xlab="%", ylab="frequency",main="% gifted, peri",freq=TRUE)
axis(side=1, at=c(0,25,50,75,100), labels=c(0,25,50,75,100))
#########################################################################

##In what form do you usually buy fresh non-tuna offshore fish (mahi mahi, wahoo, marlin)? 
table_non_tuna<-table(data$urban, data$Q1076)
barplot(table_non_tuna,legend= c("peri","rural","urban"))
##What is the price per pound in dollars you pay for fresh non-tuna offshore fish (mahi mahi, wahoo, marlin)? 
hist(data$Q1091[which(data$Q1076=="piece")], xlab="price ($)",main="Price/lb paid for fresh non-tuna pelagics")
##What form of fresh tuna do you like best?
table_tuna_like<-table(data$urban, data$Q1080)
table_non_tuna_like<-table(data$urban, data$Q1073)

##Q1077 In what form do you usually buy fresh tuna? (1-whole,2-loin,3-belly,4-steaks)
table(data$urban, data$Q1077)
##What is the price per pound in dollars you pay for fresh tuna? 
hist(data$Q1091[which(data$Q1076=="whole")&data$Q1091<50], xlab="price ($)",main="Price/lb paid for fresh tuna") #there is one outlier that is $54, because of long tail, perhaps respondents were giving price for whole fish and not per pound

##What is the price per pound in dollars you pay for fresh whole reef fish? 
hist(data$Q1078[which(data$Q1078<10)], xlab="price ($)",main="Price/lb paid for fresh whole reef fish")
# several outliers where price is >$10, can be found with the following code: data$ID[which(data$Q1078>10)]


#############################################################################
#Now think about what you personally ate in the past month. How often did you eat the following odoim?
#for data in Q1069 series
dev.off()
data <- read.csv("C:/Users/Rachel/Google Drive/Palau sanctuary/Surveys/resident/Final data/for_R.csv")
data <- data[ which(data$Q20 !='Doramae'),]
data <- data[ which(data$Q20 !='Doramae '),]
data <- data[ which(data$Q20 !='Pam'),]
data <- data[ which(data$valid_8_3min == 1),]
#subsets of data based on urban/rural
data_U<-data[which(data$urban == "urban"),]
data_R<-data[which(data$urban == "rural"),]
data_P<-data[which(data$urban == "peri"),]
par(mfrow=c(1,3), mar=c(5,4,6,2))
data_U<-data_U[,23:31] #need to check if these still correspond to the correct columns
data_R<-data_R[,23:31]
data_P<-data_P[,23:31]
for (i in 1:9){
  data_U[,i] <- factor(data_U[,i], levels = c("daily","triweekly","weekly","bi_monthly","rarely_never"))
  table_i_U<-table(data_U[,i])
  table_i_U<-prop.table(table_i_U)
  plot_i_U<-barplot(table_i_U, main="urban",, ylim=c(0,1))
  
  data_R[,i] <- factor(data_R[,i], levels = c("daily","triweekly","weekly","bi_monthly","rarely_never"))
  table_i_R<-table(data_R[,i])
  table_i_R<-prop.table(table_i_R)
  plot_i_R<-barplot(table_i_R, main="rural", ylim=c(0,1))
  
  data_P[,i] <- factor(data_P[,i], levels = c("daily","triweekly","weekly","bi_monthly","rarely_never"))
  table_i_P<-table(data_P[,i])
  table_i_P<-prop.table(table_i_P)
  plot_i_P<-barplot(table_i_P, main="peri", ylim=c(0,1))
  
  food<-c("pork","beef","fresh_tuna","pelagic","spam","can_tuna","chicken","reef","sanma")
  mtext(food[i], outer=TRUE,  cex=1, line=-1)
}

#################################################################################################
#Q1026 Over the past month, how often did you buy the following odoim?
dev.off()
par(mfrow=c(1,3))
data <- read.csv("C:/Users/Rachel/Google Drive/Palau sanctuary/Surveys/resident/Final data/for_R.csv")
data <- data[ which(data$Q20 !='Doramae'),]
data <- data[ which(data$Q20 !='Doramae '),]
data <- data[ which(data$Q20 !='Pam'),]
data <- data[ which(data$valid_8_3min == 1),]
#subsets of data based on urban/rural
data_U<-data[which(data$urban == "urban"),]
data_R<-data[which(data$urban == "rural"),]
data_P<-data[which(data$urban == "peri"),]

data_U<-data_U[,32:40]
data_R<-data_R[,32:40]
data_P<-data_P[,32:40]
for (i in 1:9){
  data_U[,i] <- factor(data_U[,i], levels = c("thrice_plus","twice","once","never"))
  table_i_U<-table(data_U[,i])
  table_i_U<-prop.table(table_i_U)
  plot_i_U<-barplot(table_i_U, main="urban", ylim=c(0,1))
  
  data_R[,i] <- factor(data_R[,i], levels = c("thrice_plus","twice","once","never"))
  table_i_R<-table(data_R[,i])
  table_i_R<-prop.table(table_i_R)
  plot_i_R<-barplot(table_i_R, main="rural", ylim=c(0,1))
  
  data_P[,i] <- factor(data_P[,i], levels = c("thrice_plus","twice","once","never"))
  table_i_P<-table(data_P[,i])
  table_i_P<-prop.table(table_i_P)
  plot_i_P<-barplot(table_i_P, main="peri", ylim=c(0,1))
  
  food<-c("pork","beef","fresh_tuna","pelagic","spam","can_tuna","chicken","reef","sanma")
  mtext(food[i], outer=TRUE,  cex=1, line=-1)
}
#################################################################################################
###CONSUMPTION QUESTIONS

table(data$Q1070)
table(data$Q1082)
nrow(data[which(data$Q1070==1 & data$Q1082==1),]) #number of respondents who ate fish both days
nrow(data[which(data$Q1070==2 & data$Q1082==1),])+nrow(data[which(data$Q1070==1 & data$Q1082==2),]) #number of respondents who ate fish one day

