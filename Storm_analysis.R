
library(dplyr)
mdata <- read.csv("StormData.csv")

md <- mdata %>% select(EVTYPE, FATALITIES,INJURIES) #subset for human damages
dead <- md %>% filter(FATALITIES > 0) %>% group_by(EVTYPE) %>% summarise(FATALITIES = sum(FATALITIES)) %>% arrange(desc(FATALITIES))
hurt <- md %>% filter(INJURIES > 0) %>% group_by(EVTYPE) %>% summarise(INJURIES = sum(INJURIES)) %>% arrange(desc(INJURIES))

death <- head(dead,5)
barplot(death$FATALITIES,names.arg = death$EVTYPE, xlab='Weather event',ylab='Deaths',main='Top 5 of weather events causing death', col = c("#3399FF"))
hurt <- head(hurt,5)
barplot(hurt$INJURIES,names.arg = hurt$EVTYPE, xlab='Weather event',ylab='Injuries',main='Top 5 of weather events causing injuries', col = c("#3399FF") )

dmg <-  mdata %>% select(EVTYPE, PROPDMG , PROPDMGEXP) %>% filter(PROPDMG > 0) %>% group_by(EVTYPE) #subset for economics damages

# Rework alphabetical characters used to signify magnitude include “K” for thousands, “M” for millions, and “B” for billions
dmg$PROPDMGEXP <- as.character(dmg$PROPDMGEXP)
dmg$PROPDMGEXP <- toupper(dmg$PROPDMGEXP)
dmg <- dmg %>% filter(PROPDMGEXP %in% c("B","K","M"))

# Create a new column and convert alpha character into numerical.
eco_dmg <- dmg %>% mutate(PROPDMGNUM = 10)
eco_dmg$PROPDMGNUM <- replace(eco_dmg$PROPDMGNUM,eco_dmg$PROPDMGEXP == "K",1000)
eco_dmg$PROPDMGNUM <- replace(eco_dmg$PROPDMGNUM,eco_dmg$PROPDMGEXP == "M",1000000)
eco_dmg$PROPDMGNUM <- replace(eco_dmg$PROPDMGNUM,eco_dmg$PROPDMGEXP == "B",1000000000)

# Calculate the total damage (need to make a product) and create a new variable which is clean for plot/screening
eco_dmg <- eco_dmg %>% mutate(Total_damage = PROPDMG*PROPDMGNUM)
total_eco_dmg <- eco_dmg %>% select(EVTYPE,Total_damage) %>% group_by(EVTYPE) %>% summarise(Total_damage = sum(Total_damage)/1e+09) %>% arrange(desc(Total_damage))
total_eco_dmg$Total_damage <- round(total_eco_dmg$Total_damage,2)

total_eco_dmg <- head(total_eco_dmg,5)
barplot(total_eco_dmg$Total_damage,names.arg = total_eco_dmg$EVTYPE, xlab='Weather event',ylab='in Billion of dollars',main='Top 5 of weather events causing economic losses', col = c("#3399FF") )



