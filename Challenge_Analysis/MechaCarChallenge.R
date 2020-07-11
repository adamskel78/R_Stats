# Mecha Car Challenge Script
library(jsonlite)
library(tidyverse)

## MPG Regression
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',stringsAsFactors = F) # Import Data
View(MechaCar_table)
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) #generate multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table)) #generate summary statistics

## Suspension Coil Summary 
SuspensionCoil_table <- read.csv(file='Suspension_Coil.csv',stringsAsFactors = F) # Import Data
View(SuspensionCoil_table)
summarize_SuspensionCoil <- SuspensionCoil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI)) #create summary table with multiple columns

## Suspension T_Test
t.test(subset(SuspensionCoil_table,Manufacturing_Lot=="Lot1")$PSI,mu=1500) #compare Lot 1 versus population result
t.test(subset(SuspensionCoil_table,Manufacturing_Lot=="Lot2")$PSI,mu=1500) #compare Lot 2 versus population result
t.test(subset(SuspensionCoil_table,Manufacturing_Lot=="Lot3")$PSI,mu=1500) #compare Lot 3 versus population result
