###    24 Aug 2019                  ###     
###    Day 2 practical-morning      ### 
###    3 of 5 R Bootcamp exercises  ###
###    Data Wrangling               ###
###    by Lekeah Durden             ###
#######################################

#setwd("C:/Users/ldurden/Desktop")
#install packages
install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("stringr")
install.packages('GGally')
install.packages('maptools')
install.packages('pacman')
install.packages("ggmaps")
install.packages("plotly")
#acquire packages from library
library('pacman')
library('dplyr')
library('magrittr')
library("ggplot2")
library("tidyverse")
library("magrittr")
library("maps")
library("plotly")
library("ggplot2")

##Task 1: Read in all three csv files as tibble data frames. For consistency with 
#these notes, we’ll assign their dataframes to be called “ld”, “pop” and “prism”, resectively.
read_csv("./DATA/lyme.csv")

ld <- read_csv('./DATA/lyme.csv')
pop <- read_csv('./DATA/pop.csv')
prism <- read_csv('./DATA/climate.csv')

#Task 2: By inspecting the ‘pop’ data, and talking with your neighbors and instructors,
# articulate in which way(s) these data fail to conform to the tidy data format?

#Answer: PRISM.data has all the same digit lengths within dataset, Ld/pop data has
# no fips and contains varying numeral lengths in the columns or multi info within cells

#FIPS- This is a number that uniquely defines a county in the US
#functions from the dplyr package: select, gather, and mutate

str_replace_all(x,y,z)
#X needs to be my pop data, Y needs to be my county codes=FIPS, Z is replacing

pop %<>% select(fips,starts_with("pop2"))
#found the columns that begin w 'pop2' these are all my years post 2000
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit 
#Aligned data w pop2 (the year value) and formed into single column of years omitted it from original data grouped it by FIPS
pop %<>% mutate(year=str_replace_all(str_year,"pop","")) 
#changing removed pop2 from the column titles and replaced as only numerals for year
pop %<>% mutate(year=as.integer(year)) 
#Changed the format from a character to an integer
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
#replacing county codes that begining w zer(^0 commands)
pop %<>% mutate(fips=as.integer(fips)) 
#replacing the fips from charaters to integers
# Everything is nicely formatted for 'tidy'

#Answer if we wanted to omit year, we could code for 
# pop %<>% select(fips, size, year)

#Task 4: Write a code chunk to convert the Lyme disease data to tidy data format
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
# found columns with starts with 'cases' and formed into single columns of 'str_year'

ld %<>% mutate(year=str_replace_all(str_year,"Cases","")) 
#created new column removing case from 'Caseyear' 

ld %<>% mutate(year=as.integer(year))
#Changed year into integer form instead of character

ld %<>% rename(state=STNAME,county=CTYNAME)
