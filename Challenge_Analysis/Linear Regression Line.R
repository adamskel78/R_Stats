# Mecha Car Challenge Script
library(jsonlite)
library(tidyverse)
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',stringsAsFactors = F)
View(MechaCar_table)
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) #generate multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table)) #generate summary statistics

# Since we have some variance we want to dive further into a couple of areas of intrest
model <- lm(mpg ~ vehicle.length,MechaCar_table) #create linear model
yvals <- model$coefficients['vehicle.length']*MechaCar_table$vehicle.length +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(MechaCar_table,aes(x=vehicle.length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model
