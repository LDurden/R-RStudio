###    24 Aug 2019                  ###     
###    Day 2 practical-morning      ### 
###    5 of 5 R Bootcamp exercises  ###
###    Data Modeling                ###
###    by Lekeah Durden             ###
#######################################



###########################################################
#Task 1: 
setwd("C:/Users/ldurden/Desktop/DATA/DATA")
#install packages
install.packages("tidyverse")
install.packages("magrittr")
install.packages('GGally')
#Acquire packages
library('magrittr')
library("ggplot2")
library("tidyverse")
library("GGally")
LdPrismPop <- read_csv("LdPrismPop.csv")

 
#############################################

#Task 2: ggpairs
 # FYI ggpairs good for exploritory data for pairings 
 #general format function ggpairs(df,columns=c("x","y","z")) where df is fata name and x,y,z equal column titles

ggpairs(LdPrismPop,columns=c("prcp","avtemp","size","cases"))

#Task 3: new columns for log10(size) and log10(cases+1) and substitute these for the original columns ()

LdPrismPop %<>% mutate(log10size =log10(size))     #in dataframe LdPrismPop 'mutated' created column of calculated log10size 
LdPrismPop %<>% mutate(log10cases =log10(cases+1)) #in dataframe LdPrismPop 'mutated' created column of calculated log10cases+1
ggpairs(LdPrismPop,columns=c("prcp","avtemp","log10size","log10cases"))
#### Working CODE ABOVE ###### 

############################################
###  A simple linear model ####
#Task 4: Using set.seed(222) create a new dataframe to be a random sample (n=100 rows) of the full df and plot prcp (x-axis) vs avtemp (y-axis).

set.seed(222) #Blue number is random selection and will be continued every time chosen for data, # itself doesnt matter)
New_df <- LdPrismPop %<>% sample_n(100) #created 100 rows in new dataframe (New_df)

myplot <- ggplot(New_df,aes(x=prcp,y=avtemp))+geom_point()
myplot

#Task 5: Add the best straight line to the plot using geom_smooth
ggplot(New_df,aes(x=prcp,y=avtemp))+
  geom_point()+
  geom_smooth(method = lm)

#Task 6: Create a linear model (lm) object
myModel <- lm(avtemp ~ prcp, data = New_df)

#Task 7:What is the slope of the line 
summary(myModel)   # produces summary table from dataframe
summary(myModel)$coefficients[2,c(1,4)] #[row 2, combines (columns 1,4)]

#Task 8: Write a single line of code to generate a ggplot of population size (total) by year.
Task8 <- 
  LdPrismPop %<>%
  ggplot(New_df,geom_point(aes(x=year,y=total)) +
                

