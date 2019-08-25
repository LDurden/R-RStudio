###  24 Aug 2019               ###     
###  Day 1 practical-morning   ### 
###   by Lekeah Durden         ###
##################################


mers <- read.csv('cases.csv')
data <-read.csv('cases.csv')
head(mers)
class(mers$onset)
mers$hospitalized[890] <-c('2015-02-20')
mers <- mers[-471,]

install.packages("lubridate")
library(lubridate)
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

day0 <- min(na.omit(mers$onset2))
#ANSWER: function na.omit is used to signify to skip empty (na) data 

mers$epi.day <- as.numeric(mers$onset2 - day0)
#Answer: The purpose of command as.numeric to give the numeral value (i.e. number for the epidemic day)
# character values wont be understood in the logic

#Install packages if not in library
## Warnings messages can be ignored there just small FYI notifications
install.packages("ggplot2")
library(ggplot2)

ggplot(data = mers) + 
  geom_bar(mapping = aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title= 'Global count of MERS cases by data of symptom onset',
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# I can change the order as long as I specify the string value (x= or y=)
# leaving out string values uses default row name

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# geom_bar() basic bar plot
#aes used to add aesthetic properties to plot of geom_bar function 

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Plot shows colorful bars of each country
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + coord_flip() +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#To modify the coordinates functions coord_flip() flips access (moves y to x) 

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + coord_polar() +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#coord_polar() creates a circular/target graph

###Univariate plots-one variable plots###
mers$infectious.period <- mers$hospitalized2-mers$onset2    
# calculate "raw" infectious period

class(mers$infectious.period)
# these data are class "difftime"

mers$infectious.period <- as.numeric(mers$infectious.period, units = "days") 
# convert to days

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) + 
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency',
       title='Distribution of calculated MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#calculated positive and zero otherwise
##ifelse function specify which values to focus from entire data range

ggplot(data=mers) + 
  geom_density(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#density plot can be used for continuesly valued data

ggplot(data=mers) + 
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#area plot of data


### Bivariate plots- 2 variables ###
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')), mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

### EXCERCISE- Install extra ggplot package extensions ###
install.packages("ggplot2-exts")

#install 'plotly'
library(plotly)
epi.curve <-ggplot(data = mers) +
  geom_bar(mapping = aes(x=epi.day)) +
  labs(x='Epidemic day', y= 'Case count', title = 'Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplotly((epi.curve) +
           geom_point(alpha=0.7, colour = "#51A0D5"))

### Exercise- Make an interactive graphic ###
funplot <- ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) + 
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(funplot)

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) + 
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


