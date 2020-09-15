

## Model estimation
#----------------------

##Data for model

df <-read.csv("final data/final_data.csv", header = T)

library(car)
library(tidyverse)
library(stargazer)
library(jtools)
library(ggstance)
library(broom.mixed)
library(margins)

## (Table 1)Base model (distance-Pat) (l_Real_price) 
#--------------------------------------------------------
xs <- c("Secchi_depth", "Total_phosphorus")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_1 <- lm(as.formula(f), data=df)

xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus", "city_distance")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_2 <- lm(as.formula(f), data=df)

xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_base_3 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_base_1,reg_base_2,reg_base_3, type = "text")


## (Table 2) Model 1: Base model (distance-Pat) (l_Real_price) with TP dummy (tp_dummy) 
#----------------------------------------------------------------------------------------

xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "tp_dummy", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_1 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_model_1, type = "text")

## Table 3= Model 2, Model 3, Model 4 (only single WQ variable)
#--------------------------------------------------------------------
## Model 2:	Only SD as WQ variable
xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_2<- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_model_2, type = "text")

## Model 3:	Only TP as WQ variable
xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area", "Total_phosphorus", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_3 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_model_3, type = "text")

## Model 4:	Only TP (dummy: 10-20 microgram per liter = 1, rest of the values = 0) as WQ variable
xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area", "tp_dummy", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_4 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_model_2, reg_model_3, reg_model_4, type = "text")

## Alternative specific models

## Table 4: model 5(SD*l_distance),model 6(TP*l_distance),model 7(TP*l_distance),model 8 (TP*lake distance dummy)
#-------------------------------------------------------------

## Model 5: SD, TP, SD*l_distance

xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus","Distance_SD", 
        "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_5 <- lm(as.formula(f), data=df)

stargazer(reg_model_5, type = "text")

## Marginal implicit price of secchi depth
#-----------------------------------------

average_price <- mean(df$Real_price)
deltaMethod(reg_model_5, "Secchi_depth + (Distance_SD*average_l_Distance)")
## For lake distance=1meter [ln(1)=0]
deltaMethod(reg_model_5, "Secchi_depth + (Distance_SD*0)")

#-----------------------------------------------------------------------------------------------
## Interpretation: with an average lake distance, house buyers are are willing to pay 0.57% less for a 1 meter more clear water. 
## However, the MWTP increases by 9.56% for water clarity for waterfront houses situated 1 meter distant to lake
#-----------------------------------------------------------------------------------------------

## MWTP for SD
#------------------------------
## For reg_model_5:
## with avg lake distance, MWTP for SD = (-0.0057592*average_price) = (-0.0057592*238893) = -$1376
## with 1 meter lake distance, MWTP for SD = (0.095566*average_price) = (0.095566*238893) = $22830
#----------------------------------

##MWTP for SD
#----------
## For lake distance=1meter [ln(1)=0]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*0))*average_price")
## For lake distance=50meter [ln(50)=3.912023005428146]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*3.912023005428146))*average_price")
## For lake distance=100meter [ln(100)=4.605170185988092]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*4.605170185988092))*average_price")
## For lake distance=250meter [ln(250)=5.521460917862246]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*5.521460917862246))*average_price")
## For lake distance=500meter [ln(500)=6.214608098422191]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.214608098422191))*average_price")
## For lake distance=750meter [ln(750)=6.620073206530356]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.620073206530356))*average_price")
## For lake distance=1000meter [ln(1000)=6.907755278982137]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.907755278982137))*average_price")
## For lake distance=2000meter [ln(2000)=7.600902459542082]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*7.600902459542082))*average_price")
## For lake distance=3000meter [ln(3000)=8.006367567650246]
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*8.006367567650246))*average_price")
## For lake distance=avg_l_dist
deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*average_l_Distance))*average_price")

## Plot of MWTP
#----------------------------------------
plot.data <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*0))*average_price") %>%
  mutate(MWTP="1m") 
plot.data1 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*3.912023005428146))*average_price") %>%
  mutate(MWTP="50m") 
plot.data2 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*4.605170185988092))*average_price") %>%
  mutate(MWTP="100m") 
plot.data3 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*5.521460917862246))*average_price") %>%
  mutate(MWTP="250m") 
plot.data4 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.214608098422191))*average_price") %>%
  mutate(MWTP="500m") 
plot.data5 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.620073206530356))*average_price") %>%
  mutate(MWTP="750m") 
plot.data6 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*6.907755278982137))*average_price") %>%
  mutate(MWTP="1000m") 
plot.data7 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*7.600902459542082))*average_price") %>%
  mutate(MWTP="2000m") 
plot.data8 <- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*8.006367567650246))*average_price") %>%
  mutate(MWTP="3000m") 
plot.data9<- deltaMethod(reg_model_5, "(Secchi_depth + (Distance_SD*average_l_Distance))*average_price") %>%
  mutate(MWTP="Average distance")

u<- data.frame(rbind(plot.data,plot.data1,plot.data2,plot.data3,plot.data4,plot.data5,plot.data6,plot.data7,plot.data8,plot.data9))

colnames(u)[3] <- "2.5 %"
colnames(u)[4] <- "97.5 %"

# Make a plot
level_order <- c('1m', '50m', '100m','250m','500m','750m','1000m','2000m','3000m', 'Average distance')

ggplot(data = u, aes(x = factor(MWTP,level = level_order),  y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_boxplot()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for TP with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme

    
## Model 6: SD, TP, TP*l_distance
xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus","Distance_TP",
        "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_6 <- lm(as.formula(f), data=df)

## Marginal implicit price of TP
#-----------------------------------

average_price <- mean(df$Real_price)
deltaMethod(reg_model_6, "Total_phosphorus + (Distance_TP*average_l_Distance)")
## For lake distance=1meter [ln(1)=0]
deltaMethod(reg_model_6, "Total_phosphorus + (Distance_TP*0)")

#-----------------------------------------------------------------------------------------------
## Interpretation: with an average lake distance, house buyers are are willing to pay 0.466% more for a marginal increase in TP. 
## However, the MWTP decreases by 0.71% for waterfront houses situated 1 meter distant to lake for a marginal increase in TP 
#-----------------------------------------------------------------------------------------------

## MWTP for TP
#------------------------------
## For reg_model_6:
## with avg lake distance, MWTP for TP = (0.00465894*average_price) = (0.00465894*238893) = $1113
## with 1 meter lake distance, MWTP for TP = (0.0014023*average_price) = (-0.0071011*238893) = $335

##MWTP for TP
#----------
## For lake distance=1meter [ln(1)=0]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*0))*average_price")
## For lake distance=50meter [ln(50)=3.912023005428146]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*3.912023005428146))*average_price")
## For lake distance=100meter [ln(100)=4.605170185988092]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*4.605170185988092))*average_price")
## For lake distance=250meter [ln(250)=5.521460917862246]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*5.521460917862246))*average_price")
## For lake distance=500meter [ln(500)=6.214608098422191]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.214608098422191))*average_price")
## For lake distance=750meter [ln(750)=6.620073206530356]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.620073206530356))*average_price")
## For lake distance=1000meter [ln(1000)=6.907755278982137]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.907755278982137))*average_price")
## For lake distance=2000meter [ln(2000)=7.600902459542082]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*7.600902459542082))*average_price")
## For lake distance=3000meter [ln(3000)=8.006367567650246]
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*8.006367567650246))*average_price")
## For lake distance=avg_l_dist
deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*average_l_Distance))*average_price")

## Plot of MWTP
#----------------------------------------
plot.data11 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*0))*average_price") %>%
  mutate(MWTP="1m") 
plot.data12 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*3.912023005428146))*average_price") %>%
  mutate(MWTP="50m") 
plot.data13 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*4.605170185988092))*average_price") %>%
  mutate(MWTP="100m") 
plot.data14 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*5.521460917862246))*average_price") %>%
  mutate(MWTP="250m") 
plot.data15 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.214608098422191))*average_price") %>%
  mutate(MWTP="500m") 
plot.data16 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.620073206530356))*average_price") %>%
  mutate(MWTP="750m") 
plot.data17 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*6.907755278982137))*average_price") %>%
  mutate(MWTP="1000m") 
plot.data18 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*7.600902459542082))*average_price") %>%
  mutate(MWTP="2000m") 
plot.data19 <- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*8.006367567650246))*average_price") %>%
  mutate(MWTP="3000m") 
plot.data20<- deltaMethod(reg_model_6, "(Total_phosphorus + (Distance_TP*average_l_Distance))*average_price") %>%
  mutate(MWTP="Average distance")

x<- data.frame(rbind(plot.data11,plot.data12,plot.data13,plot.data14,plot.data15,plot.data16,plot.data17,plot.data18,plot.data19,plot.data20))

colnames(x)[3] <- "2.5 %"
colnames(x)[4] <- "97.5 %"

# Make a plot

level_order <- c('1m', '50m', '100m','250m','500m','750m','1000m','2000m','3000m', 'Average distance')

ggplot(data = x, aes(x = factor(MWTP,level = level_order), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_boxplot()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for TP with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme
#---------------------------------------------------

## Model 7: SD, TP, SD*l_distance dummy

xs <- c("l_Lot_size", "l_Lake.Area","Secchi_depth", "Total_phosphorus",
        "lake_dist_dummy_250m_SD","lake_dist_dummy_500m_SD","lake_dist_dummy_750m_SD","lake_dist_dummy_1000m_SD","lake_dist_dummy_2000m_SD",
        "lake_dist_dummy_3000m_SD","c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", 
        "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_7 <- lm(as.formula(f), data=df)

## Marginal implicit price of SD for different lake distance
#----------------------------------------------------------------------
average_price <- mean(df$Real_price)
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_250m_SD")
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_500m_SD")
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_750m_SD")
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_1000m_SD")
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_2000m_SD")
deltaMethod(reg_model_7, "Secchi_depth + lake_dist_dummy_3000m_SD")

## MWTP for SD for different lake distance
#-------------------------------------------
# MWTP (250m) = (0.041546*average_price) = (0.041546*238893) = $9925
# MWTP (500m) = (0.0173559*average_price) = (0.0173559*238893) = $4146
# MWTP (750m) = (0.0188676*average_price) = (0.0188676*238893) = $4507
# MWTP (1000m) = (0.0096472*average_price) = (0.0096472*238893) = $2305
# MWTP (2000m) = (0.0123395*average_price) = (0.0123395*238893) = $2948
# MWTP (3000m) = (-0.0066981*average_price) = (-0.0066981*238893) = -$1600

##MWTP for SD
#----------
## For lake distance=250meter 
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_250m_SD)*average_price")
## For lake distance=500meter
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_500m_SD)*average_price")
## For lake distance=750meter 
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_750m_SD)*average_price")
## For lake distance=1000meter
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_1000m_SD)*average_price")
## For lake distance=2000meter 
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_2000m_SD)*average_price")
## For lake distance=3000meter 
deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_3000m_SD)*average_price")

## Plot of MWTP
#----------------------------------------
plot.data21 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_250m_SD)*average_price") %>%
  mutate(MWTP="250m") 
plot.data22 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_500m_SD)*average_price") %>%
  mutate(MWTP="500m") 
plot.data23 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_750m_SD)*average_price") %>%
  mutate(MWTP="750m") 
plot.data24 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_1000m_SD)*average_price") %>%
  mutate(MWTP="1000m") 
plot.data25 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_2000m_SD)*average_price") %>%
  mutate(MWTP="2000m") 
plot.data26 <- deltaMethod(reg_model_7, "(Secchi_depth + lake_dist_dummy_3000m_SD)*average_price") %>%
  mutate(MWTP="3000m") 

y<- data.frame(rbind(plot.data21,plot.data22,plot.data23,plot.data24,plot.data25,plot.data26))

colnames(y)[3] <- "2.5 %"
colnames(y)[4] <- "97.5 %"

# Make a plot
level_order <- c('250m','500m','750m','1000m','2000m','3000m')
ggplot(data = y, aes(x = factor(MWTP,level = level_order), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for SD with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme

## Model 8: SD, TP, TP*lake distance dummy

xs <- c("l_Lot_size", "l_Lake.Area","Secchi_depth", "Total_phosphorus",
        
        "lake_dist_dummy_250m_TP","lake_dist_dummy_500m_TP","lake_dist_dummy_750m_TP","lake_dist_dummy_1000m_TP","lake_dist_dummy_2000m_TP",
        "lake_dist_dummy_3000m_TP","c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", 
        "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_8 <- lm(as.formula(f), data=df)

## Marginal implicit price of TP for different lake distance
#----------------------------------------------------------------------
average_price <- mean(df$Real_price)
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_250m_TP")
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_500m_TP")
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_750m_TP")
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_1000m_TP")
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_2000m_TP")
deltaMethod(reg_model_8, "Total_phosphorus + lake_dist_dummy_3000m_TP")

## MWTP for TP for different lake distance
#-------------------------------------------
# MWTP (250m) = (0.0229628*average_price) = (0.0229628*238893) = $5485.65
# MWTP (500m) = (0.0105076 *average_price) = (0.0105076 *238893) = $2510.18
# MWTP (750m) = (0.0101787*average_price) = (0.0101787*238893) = $2431.6
# MWTP (1000m) = (0.0076219*average_price) = (0.0076219*238893) = $1820.82
# MWTP (2000m) = (0.00979671*average_price) = (0.00979671*238893) = $2340.36
# MWTP (3000m) = (0.00531544*average_price) = (0.00531544*238893) = $1269.82

##MWTP for TP dummy
#----------
## For lake distance=250meter 
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_250m_TP)*average_price")
## For lake distance=500meter
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_500m_TP)*average_price")
## For lake distance=750meter 
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_750m_TP)*average_price")
## For lake distance=1000meter
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_1000m_TP)*average_price")
## For lake distance=2000meter 
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_2000m_TP)*average_price")
## For lake distance=3000meter 
deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_3000m_TP)*average_price")

## Plot of MWTP
#----------------------------------------
plot.data31 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_250m_TP)*average_price") %>%
  mutate(MWTP="250m") 
plot.data32 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_500m_TP)*average_price") %>%
  mutate(MWTP="500m") 
plot.data33 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_750m_TP)*average_price") %>%
  mutate(MWTP="750m") 
plot.data34 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_1000m_TP)*average_price") %>%
  mutate(MWTP="1000m") 
plot.data35 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_2000m_TP)*average_price") %>%
  mutate(MWTP="2000m") 
plot.data36 <- deltaMethod(reg_model_8, "(Total_phosphorus + lake_dist_dummy_3000m_TP)*average_price") %>%
  mutate(MWTP="3000m") 

y<- data.frame(rbind(plot.data31,plot.data32,plot.data33,plot.data34,plot.data35,plot.data36))

colnames(y)[3] <- "2.5 %"
colnames(y)[4] <- "97.5 %"

# Make a plot
level_order <- c('250m','500m','750m','1000m','2000m','3000m')

ggplot(data = y, aes(x = factor(MWTP,level = level_order), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for SD with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme

stargazer(reg_model_5, reg_model_6, reg_model_7, reg_model_8,  type = "text")

## Table 5: model 10 (SD*TP, SD*l_distance), model 11(SD*TP, TP*l_distance)
#-------------------------------------------------------------

## Model 10:Interacted variables SD*TP, SD*l_distance,  
xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus", "interact_SD_TP", "Distance_SD",  "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_10 <- lm(as.formula(f), data=df)

## Marginal implicit price of secchi depth
#-----------------------------------------

average_TP <- mean(df$Total_phosphorus)
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*average_l_Distance) + (interact_SD_TP*average_TP)")

## For lake distance=1meter [ln(1)=0]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*average_TP)")
## For lake distance=250meter [ln(250)=5.521460917862246]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*average_TP)")
## For lake distance=500meter [ln(500)=6.214608098422191]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*average_TP)")
## For lake distance=750meter [ln(750)=6.620073206530356]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*average_TP)")
## For lake distance=1000meter [ln(1000)=6.907755278982137]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*average_TP)")
## For lake distance=2000meter [ln(2000)=7.600902459542082]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*average_TP)")
## For lake distance=3000meter [ln(3000)=8.006367567650246]
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*average_TP)")

## Plot of MWTP
#----------------------------------------
plot.data41 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*0) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="1m") 
plot.data42 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="250m") 
plot.data43 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="500m") 
plot.data44 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="750m") 
plot.data45 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="1000m") 
plot.data46 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="2000m") 
plot.data47 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*average_TP))*average_price") %>%
  mutate(MWTP="3000m") 

e<- data.frame(rbind(plot.data41,plot.data42,plot.data43,plot.data44,plot.data45,plot.data46,plot.data47 ))

colnames(e)[3] <- "2.5 %"
colnames(e)[4] <- "97.5 %"

# Make a plot
level_order <- c('1m','250m','500m','750m','1000m','2000m','3000m')
ggplot(data = e, aes(x = factor(MWTP,level = level_order), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for SD with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme

## For TP=9.43

## For lake distance=1meter [ln(1)=0] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*9.43)")
## For lake distance=250meter [ln(250)=5.521460917862246] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*9.43)")
## For lake distance=500meter [ln(500)=6.214608098422191] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*9.43)")
## For lake distance=750meter [ln(750)=6.620073206530356] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*9.43)")
## For lake distance=1000meter [ln(1000)=6.907755278982137] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*9.43)")
## For lake distance=2000meter [ln(2000)=7.600902459542082] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*9.43)")
## For lake distance=3000meter [ln(3000)=8.006367567650246] & TP=9.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*9.43)")

## For TP=8.43

## For lake distance=1meter [ln(1)=0] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*8.43)")
## For lake distance=250meter [ln(250)=5.521460917862246] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*8.43)")
## For lake distance=500meter [ln(500)=6.214608098422191] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*8.43)")
## For lake distance=750meter [ln(750)=6.620073206530356] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*8.43)")
## For lake distance=1000meter [ln(1000)=6.907755278982137] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*8.43)")
## For lake distance=2000meter [ln(2000)=7.600902459542082] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*8.43)")
## For lake distance=3000meter [ln(3000)=8.006367567650246] & TP=8.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*8.43)")

## For TP=7.43

## For lake distance=1meter [ln(1)=0] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*7.43)")
## For lake distance=250meter [ln(250)=5.521460917862246] & TP7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*7.43)")
## For lake distance=500meter [ln(500)=6.214608098422191] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*7.43)")
## For lake distance=750meter [ln(750)=6.620073206530356] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*7.43)")
## For lake distance=1000meter [ln(1000)=6.907755278982137] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*7.43)")
## For lake distance=2000meter [ln(2000)=7.600902459542082] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*7.43)")
## For lake distance=3000meter [ln(3000)=8.006367567650246] & TP=7.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*7.43)")

## For TP=6.43

## For lake distance=1meter [ln(1)=0] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*6.43)")
## For lake distance=250meter [ln(250)=5.521460917862246] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*6.43)")
## For lake distance=500meter [ln(500)=6.214608098422191] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*6.43)")
## For lake distance=750meter [ln(750)=6.620073206530356] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*6.43)")
## For lake distance=1000meter [ln(1000)=6.907755278982137] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*6.43)")
## For lake distance=2000meter [ln(2000)=7.600902459542082] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*6.43)")
## For lake distance=3000meter [ln(3000)=8.006367567650246] & TP=6.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*6.43)")

## For TP=5.43

## For lake distance=1meter [ln(1)=0] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*0) + (interact_SD_TP*5.43)")
## For lake distance=250meter [ln(250)=5.521460917862246] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*5.43)")
## For lake distance=500meter [ln(500)=6.214608098422191] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*5.43)")
## For lake distance=750meter [ln(750)=6.620073206530356] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*5.43)")
## For lake distance=1000meter [ln(1000)=6.907755278982137] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*5.43)")
## For lake distance=2000meter [ln(2000)=7.600902459542082] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*5.43)")
## For lake distance=3000meter [ln(3000)=8.006367567650246] & TP=5.43
deltaMethod(reg_model_10, "Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*5.43)")


## Plot of MWTP
#----------------------------------------
plot.data50 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*0) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="1m") 
plot.data51 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*5.521460917862246) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="250m") 
plot.data52 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.214608098422191) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="500m") 
plot.data53 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.620073206530356) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="750m") 
plot.data54 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*6.907755278982137) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="1000m") 
plot.data55 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*7.600902459542082) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="2000m") 
plot.data56 <- deltaMethod(reg_model_10, "(Secchi_depth + (Distance_SD*8.006367567650246) + (interact_SD_TP*5.43))*average_price") %>%
  mutate(MWTP="3000m") 

z<- data.frame(rbind(plot.data50,plot.data51,plot.data52,plot.data53,plot.data54,plot.data55,plot.data55 ))

colnames(z)[3] <- "2.5 %"
colnames(z)[4] <- "97.5 %"

# Make a plot
level_order <- c('1m','250m','500m','750m','1000m','2000m','3000m')
ggplot(data = z, aes(x = factor(MWTP,level = level_order), y = `Estimate`, ymin = `2.5 %`, ymax = `97.5 %`)) + 
  geom_pointrange() +
  geom_errorbar()+
  geom_hline(yintercept = 1, lty = 2) +
  xlab("Lake Distance") + ylab("MWTP for SD with 95% Confidence Interval")   + # Labels
  theme_bw()  # Nicer theme

# Regression results
stargazer(reg_model_10, type = "text")

## Model 11:	Interacted variables SD*TP, TP*l_distance

xs <- c("l_Lot_size", "l_Distance", "l_Lake.Area","Secchi_depth", "Total_phosphorus", "interact_SD_TP", "Distance_TP", "c_ID_dummy_P", "c_ID_dummy_O", "c_ID_dummy_H", "c_ID_dummy_Br",
        "year_dummy_2005", "year_dummy_2006", "year_dummy_2007", "year_dummy_2008", "year_dummy_2009", "year_dummy_2010", "year_dummy_2011", "year_dummy_2012", "year_dummy_2013")
f <- paste("l_Real_price ~",paste(xs, collapse=" + "))
reg_model_11 <- lm(as.formula(f), data=df)

# Regression results
stargazer(reg_model_11, type = "text")

stargazer(reg_model_10,reg_model_11, type = "text")

##------------
##------------


## Plot average marginal effect (AME) with upper & lower bound
#-------------------------------------------------------------
library(margins)
i <- margins(reg_base_2)
summary(i)
plot(i)

ggplot(data =i) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

# plot coefficients of multiple models
#--------------------------------------------------------------------------------

library(jtools)
library(ggstance)
library(broom.mixed)
plot_summs(reg_model_5,reg_model_7, scale = TRUE, inner_ci_level = .5)

plot_summs(reg_base_1)
plot_summs(reg_model_2, reg_model_3, scale = TRUE)
plot_summs(reg_model_2, reg_model_3, reg_model_3, scale = TRUE, inner_ci_level = .5)
plot_summs(reg_base_2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
#--------------------------------------------------------------------










