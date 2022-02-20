
###########################################################################################
#################################    Data Preparation     #################################  
###########################################################################################

#  install.packages(c('broom', 'car', 'countrycode', 'data.table', 'dplyr', 'ggalt', 'ggimage', 'ggplot2', 
#                     'ggpubr' , 'ggrepel', 'ggtext', 'grid', 'gridExtra', 'Hmisc', 'hrbrthemes', 'lattice', 
#                     'rworldmap', 'rworldxtra', 'scales', 'shiny', 'tidyverse', 'viridis'))


# install.packages("https://cran.r-project.org/src/contrib/rlang_0.4.11.tar.gz", repos = NULL, type="source")

library(broom)
library(car)
library(countrycode)
library(data.table)
library(dplyr)
library(ggalt)
library(ggimage)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggtext)
library(grid)
library(gridExtra)  # for grid.arrange
library(Hmisc)
library(hrbrthemes)
library(lattice) 
library(rworldmap)
library(rworldxtra)
library(scales)     # for wrap_format()
library(shiny)
library(tidyverse)
library(viridis)

setwd("C:/..")  
getwd()

# The file read is using a UTF-16 or UTF-32 encoding (with a BOM), which is why the first bytes of the location column are the Byte Order Mark (BOM). 
# So we used the encoding "UTF-8-BOM" in order to handle them appropriately.

df1 <- read.csv(file='Suicide rates_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
head(df1)
str(df1)
unique(df1[,1])

## Greece has data until 2016.
## The analysis will be based on data from 2010 to 2016, so we keep all the countries that have data till 2016.
require(stats)
temp5<-by(df1$TIME, df1$LOCATION, max)
temp5<-as.matrix(temp5)
colnames(temp5)<-"Max_year"
countries_kept<-rownames(temp5)[temp5>=2016]
rownames(temp5)[temp5<2016]

df1_temp<-df1[(df1$LOCATION %in% countries_kept) & (df1$TIME %in% c(2010:2016)),]
str(df1_temp)
sort(unique(df1_temp$LOCATION))
countries_kept

# Country names
# Dataset found here: https://gist.github.com/tadast/8827699
df3<- read.csv(file='countries_codes_and_coordinates.csv', sep=",")
str(df3)
# Strip leading and trailing Space 
df3$Alpha.3.code <- trimws(df3$Alpha.3.code, which = c("both"))
str(df3)
head(df3)
country_names_df<-df3[,c(1,3)]
colnames(country_names_df)[2]

# The country_names_df had some duplicate rows, so we removed them before joining the data frames
new_df1 <- inner_join(df1_temp, country_names_df, c("LOCATION" = "Alpha.3.code"))
final_data<-new_df1[,c(1,9,3,6,7)]
colnames(final_data)[5]<-"Suicide_rate"

## Drop unused factor levels (after subsseting)
final_data$TIME <- factor(final_data$TIME)

# ----------------------------------------------------------------
df6 <- read.csv(file='Employment rate_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df6)[7]<-'Employment_rate'
head(df6)
str(df6)

## The employement rate is measured in MEASURE: THND_PER  kai PC_WKGPOP
## We will only keep the PC_WKGPOP (percentage of working age population!!)

df6_filtered <- df6[df6$MEASURE=='PC_WKGPOP',]
final_data<-final_data[final_data$TIME %in% c(2010:2016),]

matches2 <- left_join(final_data, df6_filtered[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))
## There are missing values for employemnt rate!!!!
any(is.na(matches2$Employment_rate))
matches2[is.na(matches2$Employment_rate),]

miss467<-matches2[is.na(matches2$Employment_rate),]
unique(miss467$LOCATION)

# We exclude Brazil and Costa Rica that have missing values for Employment_rate!!!
df_data_cleaned<-matches2[!(matches2$LOCATION %in% unique(miss467$LOCATION)),]
## Drop unused factor levels (after subsseting)
df_data_cleaned$TIME <- factor(df_data_cleaned$TIME)

# ----------------------------------------------------------------
## For religious
df_relig <- read.csv(file='main_religion_per_country.csv', header=TRUE, sep=",")
any(is.na(df_relig$Main.religion))

data_rel <- inner_join(final_data, df_relig[,c(1,4)], c("Country" = "Entity"))
data_rel$Main.religion <- factor(data_rel$Main.religion)

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------

######################    Totals only   ###################### 

data_totals<-final_data[final_data$SUBJECT=='TOT',]

###  Alcohol consumption_total.csv   => only Total (not for Male, Female)!!!
df54 <- read.csv(file='Alcohol consumption_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df54)[7]<-'Alcohol_consumption'
df54$TIME <- as.factor(df54$TIME)
data_totals_alc <- left_join(data_totals, df54[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))

## There are no missing values for Alcohol consumption
any(is.na(data_totals_alc$Alcohol_consumption))
# ----------------------------------------------------------------
# ----------------------------------------------------------------

###  Hours worked_total.csv   => only Total (not for Male, Female)!!!
df8 <- read.csv(file='Hours worked_total.csv', header=TRUE, sep=",", skipNul=T, fileEncoding="UTF-8-BOM")
colnames(df8)[7]<-'Hours_worked'
df8$TIME <- as.factor(df8$TIME)
matches44 <- left_join(data_totals_alc, df8[,c(1,3,6,7)], by = c("LOCATION", "SUBJECT", "TIME"))
head(matches44)
str(matches44)

## There are missing values for Hours worked!!!!
miss4<-matches44[is.na(matches44$Hours_worked),]
sum(sapply(matches44$Hours_worked,is.na))
unique(miss4$LOCATION)

# We exclude Brazil, Turkey and Colombia that have missing values for Hours_worked!!!
df_totals_cleaned<-matches44[!(matches44$LOCATION %in% unique(miss4$LOCATION)),]
str(df_totals_cleaned)
head(df_totals_cleaned)

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# ----------------------------------------------------------------

final_data$continent<-factor(countrycode(final_data[, "Country"], origin = 'country.name', destination = 'continent'))
data_rel$continent<-factor(countrycode(data_rel[, "Country"], origin = 'country.name', destination = 'continent'))
df_data_cleaned$continent<-factor(countrycode(df_data_cleaned[, "Country"], origin = 'country.name', destination = 'continent'))
data_totals$continent<-factor(countrycode(data_totals[, "Country"], origin = 'country.name', destination = 'continent'))
data_totals_alc$continent<-factor(countrycode(data_totals_alc[, "Country"], origin = 'country.name', destination = 'continent'))
df_totals_cleaned$continent<-factor(countrycode(df_totals_cleaned[, "Country"], origin = 'country.name', destination = 'continent'))

# -----------------------------------------------------------------------
########## For employment rate
# In general, a high ratio is considered to be above 70 percent of the working-age population whereas 
# a ratio below 50 percent is considered to be low. 

df_data_cleaned$Employment_rate_cut<-cut2(df_data_cleaned$Employment_rate,cuts = c(0,50,60,70,80,100))
levels(df_data_cleaned$Employment_rate_cut)<-c('Very low','Low','Medium','High','Very high')
df_data_cleaned$Employment_rate_cut <- factor(df_data_cleaned$Employment_rate_cut, ordered=T)

########## For Alcohol consumption
df_totals_cleaned$Alcohol_cut<-cut2(df_totals_cleaned$Alcohol_consumption,cuts = c(0,5,9,12,16))
levels(df_totals_cleaned$Alcohol_cut)<-c('Low','Medium','High','Very high')
df_totals_cleaned$Alcohol_cut <- factor(df_totals_cleaned$Alcohol_cut, ordered=T)
# ----------------------------------------------------------------
                      #######  Created Data  ####### 
head(final_data)       # general data
head(data_rel)         # general data + religion
head(df_data_cleaned)  # general data + employment rate (without Brazil, Costa Rica)
head(data_totals)          # Totals only
head(data_totals_alc)      # Totals only + Alcohol consumption
head(df_totals_cleaned)    # Totals only + Alcohol consumption + Hours worked (without Brazil, Turkey, Colombia)

# ----------------------------------------------------------------
################################################################################
#################################    Plots     #################################  
################################################################################

############################################   
###########  Suicide Trends Plot ########### 
############################################

data_totals$TIME<-as.numeric(as.character(data_totals$TIME))

## Avg_Suicide_rate_per_year
temp34<-by(data_totals$Suicide_rate, data_totals$TIME, mean)
temp34<-as.matrix(temp34)
colnames(temp34)<-"Avg_suicide_rate"
temp34 <- as.data.frame(cbind(Year=seq(2010,2016,1),temp34))
rownames(temp34)<-NULL

# Total average suicide rate for all years
total_average <- mean(data_totals$Suicide_rate)
greece_values<-data_totals[data_totals$Country=='Greece',c(4,5)]
image_gr<-"https://www.pngitem.com/pimgs/m/23-231125_greece-flag-round-png-transparent-png.png"
image_world<-"https://dl0.creation.com/articles/p115/c11547/globe.jpg"

temp34$image<-image_world
greece_values$image<-image_gr

plot1a<-ggplot() + 
  theme_bw()+
  scale_x_continuous(breaks = seq(2010, 2016, 1)) + 
  scale_y_continuous(breaks = seq(0, 16)) +
  geom_line(data = temp34, aes(x = Year, y = Avg_suicide_rate), color = "indianred3", size = 1) +
  geom_line(data = greece_values, aes(x = TIME, y = Suicide_rate), color = "deepskyblue3", size = 1) +
  geom_hline(aes(yintercept = total_average), linetype = 3, color = "black", size = 1) +
  #scale_colour_manual("", breaks = c("Globally", "Greece"),
  #                    values = c("indianred3", "deepskyblue3")) +
  #scale_linetype_manual(name = "Limit", values = 3, 
  #                     guide = guide_legend(override.aes = list(color = "black", linetype = 3))) +
  labs(title = "Suicides Trends",
       subtitle = "From 2010 to 2016",x = "Year",y = "Suicide Rate (per 100K)") +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13)) +
  annotate(geom="text", x=2015.9, y=10.8, label="Globally",color="indianred3", size=5) +
  annotate(geom="text", x=2015.9, y=3.4, label='Greece',color="deepskyblue3", size=5) +
  annotate(geom="text", x=2015.6, y=12.8, label='Global Average',color="black", size=5)

plot_first<-plot1a + geom_image(data=temp34,aes(x = Year, y = Avg_suicide_rate, image=image), size=.05) +
  geom_image(data=greece_values,aes(x = TIME, y = Suicide_rate,image=image), size=.05)+
  theme(legend.title=element_blank())

#win.graph()
plot_first
# ggsave(".jpg", plot = last_plot(), device = NULL, path = NULL,
#        scale = 1, dpi = 300, limitsize = TRUE)
# --------------------------------------------------------------------

#########################################################   
###########  Suicide Trends by Continent Plot ########### 
#########################################################    

trm65<-as.data.frame(setDT(data_totals)[,.(AVG = round(mean(Suicide_rate),2)), 
                                        by = c("continent", "TIME")])
colnames(trm65)<-c("Continent","Year","Avg_suicide_rate")

plot13a <- ggplot(data=trm65, aes(x = Year, y = Avg_suicide_rate, col = Continent)) + 
  theme_bw()+
  facet_grid(Continent ~ ., scales = "free_y") + 
  scale_x_continuous(breaks = seq(2010, 2016, 1), minor_breaks = F) +
  geom_line() + 
  geom_point() + 
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13)) +
  labs(title = "Suicides Trends by Continent",
       subtitle = "From 2010 to 2016",x = "Year",y = "Suicide Rate (per 100K)")

plot13a
# -------------------------------------------
# win.graph()
grid.arrange(plot_first, plot13a, ncol = 2)

# ===================================================================================
# ===================================================================================
# ===================================================================================

#######################################################    
###########  Suicides across Countries Plot ########### 
#######################################################    

wrap_8 <- wrap_format(8)
data15a<-as.data.frame(setDT(data_totals)[,.(AVG = round(mean(Suicide_rate),2)), 
                                          by = c("continent", "Country")])
colnames(data15a)<-c("Continent","Country","Avg_suicide_rate")
data15a$Country <- factor(data15a$Country, ordered = T)
data15b<-data15a[order(data15a$Country, decreasing = FALSE),]  
data15b$code<-tolower(data15b$Country)
data15b$Iso2<-countrycode(data15b$Country, "country.name", "iso2c")
levels(data15b$Country)[c(9,20,34,35)]<-c('Czechia','Korea','UK',"USA")

ggplot(data15b, aes(x = Country, y = Avg_suicide_rate, fill = Continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept = total_average), color = "black", size = 0.7, linetype = 2) +
  geom_flag(y =  -2, aes(image = Iso2),size=0.025)  +
  theme_bw()+
  labs(title = "Suicides across Countries",
       subtitle = "Average values for the period 2010 to 2016",
       x = "Country",y = "Suicide Rate (per 100K)") +
  scale_y_continuous(breaks = seq(0, 30, 10))+
  expand_limits(y=-2)+
  theme(axis.text.x=element_text(angle=30,hjust=0.55,vjust=0.6, size = 8, color = "grey8"),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18), plot.subtitle = element_text(size=13),
        legend.title.align=1,
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.position = c(0.94, 0.84),
        axis.ticks = element_line(size = 0.8, color="grey40"), axis.ticks.length = unit(.3, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", size = 0.2)) +
  scale_fill_manual(values = c("Americas" = "lightsalmon2", "Asia" = "lightseagreen", 
                               "Europe"="gray69","Oceania"="navajowhite2")) +
  geom_text(aes(label=ifelse(Country=="Greece", Avg_suicide_rate,"")),
            position=position_dodge(width=0.9), size = 3.2, vjust = -0.2, fontface = "bold") +
  annotate(geom="text", x=32.4, y=14.6, label=wrap_8("Global Average"),color="black",size=5)

# ===================================================================================
# ===================================================================================
# ===================================================================================

##########################################  
###########  Suicides heat map ########### 
##########################################

wrap_13 <- wrap_format(13)
data_totals$Country <- factor(data_totals$Country)
dat14d<-as.matrix(by(data_totals$Suicide_rate, data_totals$Country, mean))
dat14d <- data.frame(Country=levels(data_totals$Country),Avg_suicide_rate=dat14d)
rownames(dat14d)<-NULL

library(rworldmap)
# install.packages('rworldxtra')
library(rworldxtra)
countrydata <- joinCountryData2Map(dat14d, joinCode = "NAME", nameJoinColumn = "Country", 
                                   mapResolution='medium')   # medium, coarse
par(mai=c(0.03,0.03,0.6,0.03),xaxs="i",yaxs="i",bg='grey98', cex.main=1.5)  # margins
map14<-mapCountryData(countrydata, nameColumnToPlot="Avg_suicide_rate", 
                      mapTitle="Suicide map for 2010 to 2016", 
                      xlim=c(-10.6700,31.5500),
                      ylim = c(34.5000,71.0500),
                      colourPalette = "terrain",   # "heat", "terrain"
                      oceanCol="lightskyblue",     # azure2, "lightskyblue"
                      missingCountryCol="grey65",
                      addLegend = FALSE, 
                      catMethod = "pretty",
                      borderCol = "gray35")
do.call(addMapLegend, c(map14, legendLabels="all",horiz = FALSE,
                        legendWidth=1, legendIntervals="data", legendMar = 3, legendShrink=0.75))

mtext(wrap_13("Suicide Rate (per 100K)"),side=3,line=-2,adj = 1, cex=1.1)  # topright
# mtext("(zoomed in Europe)",side=3,line=-1, cex=1.3, font=2,outer=TRUE)
mtext("(zoomed in Europe)",side=3,line=-3, cex=1.3, font=2,outer=TRUE)

do.call(addMapLegendBoxes, list(colourVector="grey65", cutVector='No data', bg="white",horiz=FALSE,
                                x="bottomright", cex=0.8,title='Missing'))
# get coordinates for each country
country_coord<-data.frame(coordinates(countrydata),stringsAsFactors=F)
# filter countrys
country_coord$Countries<-rownames(country_coord)
country_coord <- country_coord[c('Greece',"Lithuania","Turkey",'Italy', 'Spain', 'United Kingdom'),]
## country_coord <- country_coord[c('Greece',"Lithuania","Turkey"),]

# insert labels in plot
text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))
# mtext("Note: Average suicide rate per 100K",side=1,line=-1, adj = 0)

# ===================================================================================
# ===================================================================================
# ===================================================================================

################################################    
###########  Suicides by Gender Plot ########### 
################################################

## Gender differences by Country

# library(ggalt)
temp_data<-final_data[final_data$SUBJECT=='TOT',] 
#temp_data$Country<-factor(temp_data$Country)
#levels(temp_data$Country)[c(9,20,34)]<-c('Czechia','Korea','UK')
temp_data$Country<-factor(temp_data$Country)
levels(temp_data$Country)[20]<-'Republic of Korea'

temp_data$SUBJECT<-factor(temp_data$SUBJECT)
country_long<-as.data.frame(setDT(temp_data)[,.(AVG = round(mean(Suicide_rate),2)), 
                                             by = c("Country", "continent")])
colnames(country_long)<-c("Country","Continent","Avg_suicide_rate")


### by Country, continent and sex
temp_data3<-final_data
#temp_data3$Country<-factor(temp_data3$Country)
#levels(temp_data3$Country)[c(9,20,34)]<-c('Czechia','Korea','UK')
temp_data3$Country<-factor(temp_data3$Country)
levels(temp_data3$Country)[20]<-'Republic of Korea'

temp_data3$SUBJECT<-factor(temp_data3$SUBJECT)
data_long<-as.data.frame(setDT(temp_data3)[,.(AVG = round(mean(Suicide_rate),2)), 
                                                  by = c("Country", "continent", "SUBJECT")])
colnames(data_long)<-c("Country","Continent", "Sex","Avg_suicide_rate")

data_wide<-data_long %>% spread(Sex, Avg_suicide_rate)
data_wide <-data_wide[with(data_wide, order(data_wide$MEN - data_wide$WOMEN)),]  # in ascending order
# for descending order: order(-data_wide$MEN, -data_wide$WOMEN))

### For right ordering
data_wide$Country <- factor(data_wide$Country, ordered = T, levels = data_wide$Country)
data_long$Country <- factor(data_long$Country, ordered = T, levels = data_wide$Country) # use the same order

levels(data_long$Sex)[2]<-"AVERAGE"

plot4a<-ggplot(data_wide, aes(y = Country)) + 
  
  geom_dumbbell(aes(x=WOMEN, xend=MEN), color = "black", size = 1) + ## Connect bullets together!!!
  
  geom_point(data = data_long[data_long$Sex!="AVERAGE",],
             aes(x = Avg_suicide_rate, color=Sex), shape = 15, size = 3) +  
  
  geom_point(data = data_long[data_long$Sex=="AVERAGE",],
             aes(x = Avg_suicide_rate, color=Sex), shape = 18, size = 3) +
  
  geom_vline(xintercept = total_average, linetype = 2, color = "grey35", size = 1) +
  
  scale_color_manual(values = c("MEN" = "deepskyblue3", "AVERAGE" = "green4", "WOMEN" = "darkred")) +
  theme(axis.text.y = element_text(size = 8), legend.position = c(0.85, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  scale_y_discrete(labels=c("Greece"=expression(bold(Greece)), parse=TRUE)) + 
  annotate("rect", xmin = 0.5, xmax = 12.2, ymin = 0.6, ymax = 8.5,alpha = .2, color="dodgerblue", fill="dodgerblue") + 
  labs(title = "Suicides from 2010 to 2016 by gender",
       subtitle = "Ordering: descending order of suicide rates' difference by Sex",
       x = "Suicide Rate (per 100K)",y = "Country") +
  theme_bw() +
  theme(legend.position = c(0.08, 0.88),legend.title = element_blank(),
        legend.background =element_rect(fill="white", color = "grey88",size=0.5),
        plot.background=element_rect(fill="#f7f7f7"),
        #panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        #axis.ticks=element_blank(),
        panel.border=element_blank(),
        axis.title.x=element_text(size=14),axis.title.y=element_text(size=14), 
        axis.text.x=element_text(angle=30,hjust=0.55,vjust=0.6, size = 8, color = "grey8"),
        axis.ticks = element_line(size = 0.8, color="grey40"), axis.ticks.length = unit(.3, "cm"))+
  coord_flip()+ 
  annotate(geom="text", x=15, y=4.1, label="Global Average Line",color="black",size=5)

plot4a

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

##############################################################    
###########  % distribution of suicdes by sex Plot ########### 
##############################################################    

data_longb2<-data_long
data_wideb2<-data_longb2 %>% spread(Sex, Avg_suicide_rate)
data_wideb2 <-data_wideb2[with(data_wideb2, order(data_wideb2$MEN)),]  # in ascending order
data_wideb2$Country <- factor(data_wideb2$Country, ordered = T, levels = data_wideb2$Country)
data_longb2$Country <- factor(data_longb2$Country, ordered = T, levels = data_wideb2$Country) # use the same order
levels(data_longb2$Sex)[2]<-"AVERAGE"

## Create column for men percentage in ascending order!!!
teamp54<-data_wide
teamp54$Men_Proportion<-teamp54$MEN/(teamp54$MEN+teamp54$WOMEN)
teamp54<-teamp54[order(teamp54$Men_Proportion, decreasing = FALSE),]  
teamp54_new<-teamp54
teamp54_new$Men_Proportion<-paste(round(100*teamp54_new$Men_Proportion,0),'%',sep="")
head(teamp54_new)

# Levels in the same order!!!
data_long2<-data_longb2
data_long2$Country <- factor(data_long2$Country, ordered = T,levels = teamp54$Country)
data_long2<-data_long2[data_long2$Sex!='AVERAGE',]
data_long2$Sex<-factor(data_long2$Sex)
levels(data_long2$Country)[c(7,31,17)]<-c('Czechia','Korea','UK')

# install.packages('grid')
# library(grid)
text_low <- textGrob("Less\nmen", gp=gpar(fontsize=13, fontface="bold"))
text_high <- textGrob("Most\nmen", gp=gpar(fontsize=13, fontface="bold"))


plot4b<-ggplot(data_long2, aes(x = Country, y = Avg_suicide_rate, fill = Sex)) + 
  geom_bar(position = "fill", stat = "identity", width = 0.85) +
  theme_bw() +
  scale_fill_manual(values = c("MEN" = "gray69", "WOMEN" = "tomato2")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=c("Greece"=expression(bold(Greece)), parse=TRUE)) + 
  labs(title = "Percent distribution of suicides by sex for various countries",
       subtitle = "Ordering: From the lowest men suicide rate to the highest",
       x = "Country",y = "Percentage of suicides") +
  theme(legend.title = element_blank(),           # Remove the legend title
        legend.position = "bottom",
        legend.background =element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        legend.box = "vertical", legend.title.align=1,
        axis.text.x=element_text(angle=30,hjust=0.55,vjust=0.6, size = 8, color = "grey8"),
        plot.title = element_text(size=18), plot.subtitle = element_text(size=13),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        axis.ticks = element_line(size = 0.8, color="grey40"), axis.ticks.length = unit(.3, "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", size = 0.2))

plot4b

# ===================================================================================
# ===================================================================================
# ===================================================================================

########################################################   
###########  Baloon plot for employment rate ########### 
########################################################   

###### Data prepartion for plot
df_data_cleaned$Employment_cut3<-cut2(df_data_cleaned$Employment_rate,cuts = c(0,50,100))
global2<-df_data_cleaned[df_data_cleaned$SUBJECT=='TOT',] 
global2$SUBJECT<-factor(global2$SUBJECT)
global2<-as.data.frame(setDT(global2)[,.(AVG = round(mean(Suicide_rate),2)), 
                                      by = c("Employment_cut3")])
colnames(global2)<-c("Employment_rate_cut","Avg_suicide_rate")

general2<-df_data_cleaned[df_data_cleaned$SUBJECT=='TOT',] 
general2<-general2[general2$continent!='Oceania',]   # Remove Oceania (it has only one value!!)
general2<-general2[general2$continent!='Americas',]  # Remove Oceania (it has only one value!!)
general2$SUBJECT<-factor(general2$SUBJECT)
general2<-as.data.frame(setDT(general2)[,.(AVG = round(mean(Suicide_rate),2)), 
                                        by = c("continent", "Employment_cut3")])
colnames(general2)<-c("Continent","Employment_rate_cut","Avg_suicide_rate")

europe_data<-general2[general2$Continent=='Europe',] 
asia_data<-general2[general2$Continent=='Asia',] 

greekdata_temp<-df_data_cleaned[df_data_cleaned$SUBJECT=='TOT',] 
greekdata<-as.matrix(by(greekdata_temp$Suicide_rate[greekdata_temp$Country=='Greece'], 
                        greekdata_temp$Employment_cut3[greekdata_temp$Country=='Greece'], mean))

greekdata <- as.data.frame(cbind(rownames(greekdata),greekdata))
colnames(greekdata)<-c('Employment_rate_cut', 'Avg_suicide_rate')
rownames(greekdata)<-NULL


turk_data<-as.matrix(by(greekdata_temp$Suicide_rate[greekdata_temp$Country=='Turkey'], 
                        greekdata_temp$Employment_cut3[greekdata_temp$Country=='Turkey'], mean))
turk_data <- as.data.frame(cbind(rownames(turk_data),turk_data))
colnames(turk_data)<-c('Employment_rate_cut', 'Avg_suicide_rate')
rownames(turk_data)<-NULL

data_baloon<-data.frame("Employment_rate"=greekdata$Employment_rate_cut, 
                        "Global" = global2$Avg_suicide_rate,
                        "Asia" = asia_data$Avg_suicide_rate,"Europe" = europe_data$Avg_suicide_rate,
                        "Greece"=greekdata$Avg_suicide_rate, "Turkey" = turk_data$Avg_suicide_rate)
rownames(data_baloon)<-NULL


# data_baloon <- melt(data_baloon, id="Employment_rate")  # convert to long format
data_baloon <- melt(as.data.table(data_baloon), id="Employment_rate")  # convert to long format
data_baloon<-as.data.frame(data_baloon)
names(data_baloon)[names(data_baloon) == "variable"] <- "Location"
names(data_baloon)[names(data_baloon) == "value"] <- "Avg_Suicides"

data_baloon$Avg_Suicides<-as.numeric(data_baloon$Avg_Suicides)
data_baloon$Employment_rate<-factor(data_baloon$Employment_rate)
levels(data_baloon$Employment_rate)<-c('Below 50%','Above 50%')


###### Plot
# library(ggpubr)
# library(scales)  # for wrap_format()
wrap_13 <- wrap_format(13)

ggballoonplot(data_baloon, fill = "Avg_Suicides", color = "lightgray",size = 15, show.label = TRUE, shape=23)+
  gradient_fill(c("steelblue2", "white", "red"))+
  guides(size = FALSE) + theme_bw() +         
  labs(title = "Indicative suicide rates (per 100K) per employment rate",
       subtitle = "Benchmark: 50% employment rate",
       x = "Employment rate",y = "Area", fill = "Employment_rate_cut") +
  theme(legend.position = "right", 
        legend.title=element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13)) +
  labs(fill=wrap_13('Suicide rate (per 100K)'))



# ===================================================================================
# ===================================================================================
# ===================================================================================

#################################################################################    
###########  Correlation between suicide and alcohol consumption Plot ########### 
#################################################################################    


data_rel98<-data_rel[data_rel$SUBJECT=='TOT',] 
data999 <- inner_join(data_totals_alc[,c(2:6)], data_rel98[,c(2:6)], c("Country", "SUBJECT","TIME","Suicide_rate"))
data999b<-as.data.frame(setDT(data999)[,.(AVG1 = round(mean(Suicide_rate),2),
                                          AVG2 = round(mean(Alcohol_consumption),2)),
                                       by = c("Country","Main.religion")])
colnames(data999b)<-c("Country","Main.religion","Avg_suicide_rate","Avg_alc_consumption")


#install.packages('ggalt')
# library(ggalt)
# library(scales)  # for wrap_format()
wrap_12 <- wrap_format(12)

ggplot(data999b, aes(Avg_alc_consumption, Avg_suicide_rate, color = Main.religion)) +
  geom_point(aes(shape = Main.religion, color=Main.religion), size = 4) +
  scale_shape_manual(values = c("Christianity" = 18,
                                "Unaffiliated Religions" = 18, 
                                "Judaism" = 18, 
                                "Islam" = 18)) +
  scale_color_manual(values = c("Christianity" = "mediumseagreen",
                                "Unaffiliated Religions" = "red3", 
                                "Judaism" = "saddlebrown", 
                                "Islam" = "blueviolet")) +
  annotate(geom="text", x=13, y=10, label="Christianity",color="mediumseagreen", size=5, fontface="bold") +
  annotate(geom="text", x=7.5, y=24, label=wrap_12('Unaffiliated Religions'),color="red3", size=5, fontface="bold") +
  annotate(geom="text", x=2.8, y=8, label="Judaism",color="saddlebrown", size=5, fontface="bold") +
  annotate(geom="text", x=1.5, y=4.4, label="Islam",color="blueviolet", size=5, fontface="bold") + 
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13)) +
  geom_text(label = ifelse(data999b$Avg_suicide_rate>25, as.character(data999b$Country[data999b$Avg_suicide_rate>25]), ""), 
            position = position_nudge(y = -0.6)) +
  geom_text(label = ifelse(data999b$Country=='Greece', 'Greece', ""), 
            position = position_nudge(y = -0.6)) +
  geom_text(label = ifelse(data999b$Main.religion=='Islam', as.character(data999b$Country[data999b$Main.religion=='Islam']), ""), 
            position = position_nudge(y = 1)) +
  geom_text(label = ifelse(data999b$Main.religion=='Judaism', as.character(data999b$Country[data999b$Main.religion=='Judaism']), ""), 
            position = position_nudge(y = 1)) +
  scale_x_continuous(breaks = seq(0, 15, 2),labels = paste(seq(0, 15, 2), "L", sep = "")) +
  labs(title = "Correlation between suicide and alcohol consumption",
       subtitle = "Average values for the period 2010 to 2016",
       x = "Alcohol cunsumption (per capita)",y = "Suicide Rate (per 100K)")

# ===================================================================================
# ===================================================================================
# ===================================================================================

#################################################################################    
###########  Correlation between suicide and alcohol consumption Plot ########### 
###########                     * Outliers removed *                  ###########
#################################################################################    

################# Outlier detection and removal ###################
df54<-as.data.frame(setDT(df_totals_cleaned)[,.(AVG1 = round(mean(Alcohol_consumption),2),
                                                AVG2 = round(mean(Suicide_rate),2)), 
                                             by = c("Country", "continent")])
colnames(df54)<-c("Country","Continent","Avg_alc_consumption","Avg_suicide_rate")


####### Exclude outliers with Cook's Distance > 4/n

# library(broom)
# library(dplyr)
model1 <- lm(Avg_suicide_rate ~ Avg_alc_consumption, data = data999b)
round(glance(model1)$p.value, 4) # p-value<0.05 => Reject Ho. 
# Check if there are outliers in the data

# Reshape model into augmented data
# library(broom)
# augment(model1)

### Remove outliers from the model (based on cook's distance)
df756<-as.data.frame(augment(model1))
df756<-df756[order(df756$.cooksd, decreasing = TRUE),]  

df756c<-df756[df756$.cooksd < 4/dim(df756)[1],] # removed 2 countries from the 32 ('Lithuania','South Korea')
df756c4 <- inner_join(df756c, data999b, c("Avg_suicide_rate", "Avg_alc_consumption"))
kept54<-df756c4[,c('Country','Main.religion', 'Avg_alc_consumption', 'Avg_suicide_rate')]

# Fit model without outliers
model2 <- lm(Avg_suicide_rate ~ Avg_alc_consumption, data = kept54)
summary(model2)

round(glance(model2)$p.value, 4) # Now, p-value>0.05. So we do not reject Ho.
# So, the alcohol consumption has an association with the suicide rate

# -----------------------------------

##### Plot

wrap_68 <- wrap_format(68)

ggplot(kept54, aes(Avg_alc_consumption, Avg_suicide_rate, color = Main.religion)) +
  geom_point(aes(shape = Main.religion, color=Main.religion), size = 4) +
  theme_bw()+
  scale_x_continuous(breaks = seq(0, 16, 2),labels = paste(seq(0, 16, 2), "L", sep = "")) +
  scale_shape_manual(values = c("Christianity" = 18,
                                "Unaffiliated Religions" = 18, 
                                "Judaism" = 18, 
                                "Islam" = 18)) +
  scale_color_manual(values = c("Christianity" = "mediumseagreen",
                                "Unaffiliated Religions" = "red3", 
                                "Judaism" = "saddlebrown", 
                                "Islam" = "blueviolet")) +
  annotate(geom="text", x=12.5, y=11, label="Christianity",color="mediumseagreen", size=5) +
  annotate(geom="text", x=8.4, y=19, label=wrap_12('Unaffiliated Religions'),color="red3", size=5) +
  annotate(geom="text", x=2.8, y=7, label="Judaism",color="saddlebrown", size=5) +
  annotate(geom="text", x=1.5, y=3.2, label="Islam",color="blueviolet", size=5) + 
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13)) +
  geom_text(data=kept54[kept54$Country=='Greece',],label='Greece', position = position_nudge(y = -0.6)) +
  geom_text(data=kept54[kept54$Country=='Italy',],label='Italy', position = position_nudge(y = 0.6,x=0.2)) +
  geom_text(data=kept54[kept54$Country=='Brazil',],label='Brazil', position = position_nudge(y = 0.6)) +
  geom_smooth(data=kept54, method = "lm", aes(group = 1), fill='gray82', color="black", size=0.5) +
  labs(title = "Correlation between suicide and alcohol consumption",
       subtitle = wrap_68("Outliers removed based on Cook's distance (2/32 countries excluded) Average values for the period 2010 to 2016"),
       x = "Alcohol cunsumption (per capita)",y = "Suicide Rate (per 100K)")


# ===================================================================================
# ===================================================================================
# ===================================================================================

############################################################################   
###########  Bubble plot of Alcohol consumption vs. Hours Worked ########### 
############################################################################   

# library(data.table)
df5443<-as.data.frame(setDT(df_totals_cleaned)[,.(AVG1 = round(mean(Suicide_rate),2),
                                                  AVG2 = round(mean(Alcohol_consumption),2),
                                                  AVG3 = round(mean(Hours_worked),2)),
                                               by = c("Country","continent")])
colnames(df5443)<-c("Country","Continent","Avg_suicide_rate","Avg_alc_consumption","Avg_hours_worked")

## Sort by suicide rate valye
df5443<-df5443[order(df5443$Avg_suicide_rate, decreasing = TRUE),]  


df5443$Country<-factor(df5443$Country)
levels(df5443$Country)[18]<-"Republic of Korea"

ggplot(df5443, aes(x=Avg_hours_worked, y=Avg_alc_consumption, size = Avg_suicide_rate, color = Continent)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(2, 7), name="Average Suicide Rate (per 100K)") +
  scale_colour_hue(na.value = "black") +
  geom_text_repel(data=df5443[df5443$Country!="Greece",], aes(label=Country), size=2.5,box.padding = 0.4) + 
  geom_text_repel(data=df5443[df5443$Country=="Greece",], aes(label=Country), size=6, fontface="bold", 
                  lineheight=3,box.padding = 0.5, point.padding = 1e-02) + 
  theme_bw() + 
  labs(title="Bubble plot of Alcohol consumption vs. Hours worked among countries", 
       subtitle="Bubble size: Average Suicide rate (per 100K) from 2010 to 2016",
       x = "Hours worked (per worker)", y = "Alcohol consumption  (per capita)") +
  scale_y_continuous(breaks = seq(0, 16, 4),labels = paste(seq(0, 16, 4), "L", sep = "")) +
  scale_x_continuous(breaks = seq(1200, 2200, 200),labels = paste(seq(1200, 2200, 200), "h", sep = "")) +
  theme(legend.position="bottom", legend.box = "vertical",
        legend.background = element_rect(fill="white", color = "grey88",size=0.5, linetype="solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        axis.title.x=element_text(size=12),axis.title.y=element_text(size=12),
        plot.title = element_text(size=18),
        plot.subtitle = element_text(size=13))
