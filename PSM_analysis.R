############################################################
#
#  This performs PSM analysis (Table 3 & 4 with matched samples)
#  Use package 'MatchIt'
#  Follows Randolph et al. (2014) 
############################################################

install.packages('MatchIt')
install.packages('tableone')

library(MatchIt)
library(data.table)

## (0) Read the data:
setwd("C:\\Users\\Yuriy\\Documents\\_Research\\  !  Project_ Payout Revolution\\Data\\capitaliq")
data = fread("capital_iq_work_full.csv")
data[1:5,]


summary(data$dco)
sd(data$dco, na.rm = T)
length(data$dco) - sum(is.na(data$dco))


## (1) Mach samples:
# Matching variables: size(t-1), payouts(t-1), 
#   cash_ta(t-1), past ownership(t-1)
colnames(data)

data = data[!is.na(data$dco_high),] 
data = data[!is.na(data$size_1),] 
data = data[!is.na(data$own1_1),] 
data = data[!is.na(data$netpo_ta_1),] 
data = data[!is.na(data$cash_ta_1),]
# data = data[!is.na(data$crossdummy1),]
data = data[!is.na(data$ebit_ta_1),]
data = data[!is.na(data$hhi_1),]

data[1:7,]
data$cash_ta_1 = (data$cash_ta_1 - mean(data$cash_ta_1))/sd(data$cash_ta_1)
data$ebit_ta_1 = (data$ebit_ta_1 - mean(data$ebit_ta_1))/sd(data$ebit_ta_1)
data$hhi_1 = (data$hhi_1 - mean(data$hhi_1))/sd(data$hhi_1)

summary(data$cash_ta_1)
summary(data$hhi_1)
summary(data$ebit_ta_1)

mydata = data[,c('dco_high', 'crossdummy1' ,'size_1', 'own1_1', 'netpo_ta_1', 
                 'cash_ta_1', 'ebit_ta_1', 'hhi_1', 
                 'time', 'gvkey', 'cusip', 'sic3')]

mydata[1:7,]

m.out1 = matchit(crossdummy1 ~ size_1 + own1_1 + netpo_ta_1 + 
                   cash_ta_1 + ebit_ta_1 + hhi_1, 
                data = mydata, method = "nearest", ratio = 1) 


## (2) Save matched data:
m.data1 = match.data(m.out1)
write.csv(m.data1, file = "capital_iq_work_matched.csv")

######################################################
######## Matching for each year separately ###########

data = fread("capital_iq_work_full.csv")

count = 1
for (tt in 2001:2017) {
  print(tt)
  data1 = data[data$time == tt, ]
  data1 = data1[!is.na(data1$dco_high),] 
  data1 = data1[!is.na(data1$size_1),] 
  data1 = data1[!is.na(data1$own1_1),] 
  data1 = data1[!is.na(data1$netpo_ta_1),] 
  data1 = data1[!is.na(data1$cash_ta_1),]
  data1 = data1[!is.na(data1$ebit_ta_1),]
  data1 = data1[!is.na(data1$hhi_1),]
  data1 = data1[!is.na(data1$crossdummy1_1),]
  
  cash_m = mean(data1$cash_ta_1)
  cash_sd = sd(data1$cash_ta_1)
  ebit_m = mean(data1$ebit_ta_1)
  ebit_sd = sd(data1$ebit_ta_1)
  hhi_m = mean(data1$hhi_1)
  hhi_sd = sd(data1$hhi_1)
  
  data1$cash_ta_1 = (data1$cash_ta_1 - cash_m)/cash_sd
  data1$ebit_ta_1 = (data1$ebit_ta_1 - ebit_m)/ebit_sd
  data1$hhi_1 = (data1$hhi_1 - hhi_m)/hhi_sd
  
  mydata = data1[,c('dco_high', 'crossdummy1_1' ,'size_1', 'own1_1', 'netpo_ta_1', 
                   'cash_ta_1', 'ebit_ta_1', 'hhi_1', 
                   'time', 'gvkey', 'cusip', 'sic3')]
  
  m.out1 = matchit(dco_high ~ size_1 + own1_1 + netpo_ta_1 + 
                     cash_ta_1 + ebit_ta_1 + hhi_1 + crossdummy1_1, 
                   data = mydata, method = "nearest", ratio = 1) 

  data_new = match.data(m.out1)
  data_new$cash_ta_1 = data_new$cash_ta_1*cash_sd + cash_m 
  data_new$ebit_ta_1 = data_new$ebit_ta_1*ebit_sd + ebit_m 
  data_new$hhi_1 = data_new$hhi_1*hhi_sd + hhi_m 
  
  if (count == 1) {
    m.data1 = data_new
  } else {
    m.data1 = rbind(m.data1, data_new)
  }
  count = count + 1
}

write.csv(m.data1, file = "capital_iq_work_matched_yby.csv")

# -------------------------------
colnames(m.data1)
m.data1[1:7,]
a1 <- summary(m.out1)
summary(m.out1$model)
plot(m.out1, type = "hist")
