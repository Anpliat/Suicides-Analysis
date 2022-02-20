###########################################################################################
#################################    Data Preparation     #################################  
###########################################################################################

#####     1st part of Data Preparation code:
#      => Same as in the main file (that's why all the comments were erased).

library(countrycode)
library(data.table)
library(dplyr)
library(ggplot2)
library(lattice) 
library(shiny)
library(tidyverse)

setwd("C:/..")

df1 <- read.csv(file='Suicide rates_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
temp5<-by(df1$TIME, df1$LOCATION, max)
temp5<-as.matrix(temp5)
colnames(temp5)<-"Max_year"
countries_kept<-rownames(temp5)[temp5>=2016]

df1_temp<-df1[(df1$LOCATION %in% countries_kept) & (df1$TIME %in% c(1980:2016)),]
df3<- read.csv(file='countries_codes_and_coordinates.csv', sep=",")
df3$Alpha.3.code <- trimws(df3$Alpha.3.code, which = c("both"))
country_names_df<-df3[,c(1,3)]

new_df1 <- inner_join(df1_temp, country_names_df, c("LOCATION" = "Alpha.3.code"))
final_data<-new_df1[,c(1,9,3,6,7)]
colnames(final_data)[5]<-"Suicide_rate"
final_data$TIME <- factor(final_data$TIME)
df6 <- read.csv(file='Employment rate_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df6)[7]<-'Employment_rate'
df6_filtered <- df6[df6$MEASURE=='PC_WKGPOP',]
final_data<-final_data[final_data$TIME %in% c(1980:2016),]

matches2 <- left_join(final_data, df6_filtered[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))
miss467<-matches2[is.na(matches2$Employment_rate),]
df_data_cleaned<-matches2[!(matches2$LOCATION %in% unique(miss467$LOCATION)),]
df_data_cleaned$TIME <- factor(df_data_cleaned$TIME)
df_relig <- read.csv(file='main_religion_per_country.csv', header=TRUE, sep=",")
data_rel <- inner_join(final_data, df_relig[,c(1,4)], c("Country" = "Entity"))
data_rel$Main.religion <- factor(data_rel$Main.religion)


data_totals<-final_data[final_data$SUBJECT=='TOT',]
df54 <- read.csv(file='Alcohol consumption_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df54)[7]<-'Alcohol_consumption'
df54$TIME <- as.factor(df54$TIME)
data_totals_alc <- left_join(data_totals, df54[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))
df8 <- read.csv(file='Hours worked_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df8)[7]<-'Hours_worked'
df8$TIME <- as.factor(df8$TIME)
matches44 <- left_join(data_totals_alc, df8[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))
miss4<-matches44[is.na(matches44$Hours_worked),]
df_totals_cleaned<-matches44[!(matches44$LOCATION %in% unique(miss4$LOCATION)),]



final_data$continent<-factor(countrycode(final_data[, "Country"], origin = 'country.name', destination = 'continent'))
data_rel$continent<-factor(countrycode(data_rel[, "Country"], origin = 'country.name', destination = 'continent'))
df_data_cleaned$continent<-factor(countrycode(df_data_cleaned[, "Country"], origin = 'country.name', destination = 'continent'))
data_totals$continent<-factor(countrycode(data_totals[, "Country"], origin = 'country.name', destination = 'continent'))
data_totals_alc$continent<-factor(countrycode(data_totals_alc[, "Country"], origin = 'country.name', destination = 'continent'))
df_totals_cleaned$continent<-factor(countrycode(df_totals_cleaned[, "Country"], origin = 'country.name', destination = 'continent'))


#######################################################################################
############################     Additions for shiny App    ###########################
#######################################################################################

### Libraries
library(ggalt)
library(scales)  # for wrap_format()
library(DT)

#### Data
data_rel98<-data_rel[data_rel$SUBJECT=='TOT',] 
data999 <- inner_join(data_totals_alc[,c(2:7)], data_rel98[,c(2:7)], c("Country", "SUBJECT","TIME",'continent',"Suicide_rate"))

colnames(data999)[3]<-"Year"
data999$Year<-as.numeric(as.character(data999$Year))

