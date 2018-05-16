### AUTHORS : Lea Briard
### DATE : 16 MAY 2018
### PURPOSE: project management
### data = MERS, cases.csv


### packages
library(ggplot2)
library(tidyverse)
library(magrittr)

##data
cases<-read_csv("cases.csv")
myplot<- cases%>% 
  ggplot(aes(x=country, y = death ))+geom_bar(stat="identity")
