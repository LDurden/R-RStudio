###  24 Aug 2019                 ###     
###  Day 1 practical-afternoon   ###
###  2 of 5 R Bootcamp exercises ###
###                              ###
####################################

setwd("C:/Users/ldurden/Desktop/Rbootcamp_IU/DATA")
#install.packages("magrittr")
library('magrittr')
#install.packages("ggplot2")
library('ggplot2')


##SCRIPTS##
#Exercise. Write a script to load the West Nile virus data and use ggplot 
# to create a histogram for the total number of cases in each state in each year. 
# Follow the format of the prototypical script advocated in the presentation: 
# Header, Load Packages, Declare Functions, Load Data, Perform Analysis.

WNVdata <-read.csv('wnv.csv')
head(WNVdata)

ggplot(data=WNVdata) + 
  geom_bar(mapping=aes(x=Total)) +
  labs(x='State-level case burden', y='Case count', title='Histogram of West Nile state-level case count')

#Way 1_ Using scale_x_log10 to transform within the set
ggplot(data=WNVdata) + scale_x_log10() +
  geom_histogram(mapping=aes(x=Total)) +
  labs(x='State-level case burden', y='Case count', title='Histogram of West Nile state-level case count')

#Way 2_create a new column of transformed data logTotal
WNVdata$logTotal <- log10(WNVdata$Total)
ggplot(data=WNVdata) +
  geom_histogram(mapping=aes(x=logTotal),binwidth = 30) +
  labs(x='State-level case burden', y='Case count', title='Histogram of West Nile state-level case count')

ggplot(data=WNVdata) + 
  geom_histogram(mapping=aes(x=Total),binwidth=15) +
  labs(x='State-level case burden', y='Case count', title='Histogram of West Nile state-level case count')

#Exercise. Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state in each year. 
# Plot a histogram of the calcated CFRs.

CFR <- (WNVdata$Fatal)/(WNVdata$Total)
  
ggplot(data=WNVdata) + 
  geom_histogram(mapping=aes(x=CFR) +
  labs(x='State-level case burden', y='Case count', title='Case fatality rate in each state per year'))

WNVdata %>% 
  mutate(CFR = (Fatal)/(Total)) %>%
  na.omit() %>%
  ggplot() +
  geom_histogram(aes(x = CFR)) +
  labs(x='State-level case burden', y='CFR', title='Case fatality rate in each state per year')

##Try again
#WNVdata %>% 
  na.omit() %>% 
  ggplot() +
  geom_histogram(aes(x = (WNVdata$Fatal)/(WNVdata$Total)) +
  labs(x='State-level case burden', y='CFR', title='Case fatality rate in each state per year'))
  
#Exercise. Use arithmetic operators, logical operators, and the function sum to verify that the variable 
# Total is simply the sum of the number of cases (Fever, EncephMen, and Other)
 
Total2 <- WNVdata$EncephMen + WNVdata$Fever + WNVdata$Other
WNVdata$Total==Total2
sum(WNVdata$Total==Total2) #camparing numbers are equal or True

sum(WNVdata$Total!=Total2) #numbers that are false


## FUNCTIONS ##
#  Exercise. Write a function to calculate the mean and standard error (standard deviation divided by the 
#  square root of the sample size) of the neuroinvasive disease rate for all the states in a given list 
#  and given set of years. Follow the Google R style and remember to place the function near the top of your
#  script. Use your function to calculate the average severe disease rate in California, Colorado, and New York.

ndr <- function(state='California', years=1999:2007){
  x <- WNVdata[WNVdata$State %in% & WNVdata$Year %in% years,]
  y <- data.frame(state =x$State,ndr = x$EncephMen / x$Total)
  m <- aggregate(y$ndr, by=list(y$state), FUN = mean) sd <- aggregate(y$ndr, by=list(y$state), 
  FUN = function(x) se(x)/sqrt(length(x)) )
  out <- merge(m, se, by = 'Group.1') names(out) <- c('state','mean.ndr','se.ndr') return(out)
    }

disease_rate <- ndr(state = c('California','Colorado','New York'))


disease_mean <- sum(state*year)/sum(year)
return(m)
     
##PIPES##
#Exercise. Use pipes to produce the same plots without using your function.
# Looping practice
for(prefix in c('b','c','m','r')){
  word <- paste(prefix, 'at', sep='')
  print(word)}
#looping practice 2
for(prefix in c('t','l','b','r')){
  word2 <- paste(prefix, 'ow', sep ='')
  print(word2)}