library("data.table")
library("readxl")
library ("xlsx")
library("ggplot2")

setwd("/Users/adamsarissky/Desktop/R")
StormData <-read.csv("repdata-data-StormData.csv.bz2")

##EVtype - what type of disaster
##Fatalities and Property Damage = FATALITIES and PROPDMG alsoo INJURIES 
## and CROPDMG

## load the dataset as data.table
Storm_Data <- as.data.table(StormData)

##Fatalities and Type
rmcol <- colnames(Storm_Data [, !c("EVTYPE", "FATALITIES", "INJURIES")])
StormDF <- Storm_Data[, c(rmcol) := NULL]


##Calculating it as a sum 
StormDF1 <- StormDF[, .(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)), by = .(EVTYPE)]

##ordering it by fatalities
StormDF1 <- StormDF1[order(-FATALITIES), ]

StormDF1 <- StormDF1[1:10, ]


StormDF1 <-  melt(StormDF1, id.vars="EVTYPE", variable.name = "Catastrophy")

##Creating a chart
healthChart <- ggplot(StormDF1, aes(x=reorder(EVTYPE, -value), y=value))
healthChart = healthChart + geom_bar(stat="identity", aes(fill=Catastrophy), position="dodge")
healthChart = healthChart + ylab("Count") 
healthChart = healthChart + xlab("Event Type") 
healthChart = healthChart + theme_light() + theme(axis.text.x = element_text(angle=45, hjust=1))
healthChart = healthChart + ggtitle("Catastrophies that cause the most:") + theme(plot.title = element_text(hjust = 0.5))


##Part 2 - Property damage tracking 

# Creating another dataset wil all variables and reding it in as a table
StormData2 <- as.data.table(StormData)

#checking the variables 
colnames(Storm_Data)

# looking for variables we want to explore
# - PROPDMG, CROPDMG (?) Cropdmgexp and propdmgexp (?)

#Designating which collumns we actually want to remove
rmcol2 <- colnames(StormData2[, !c("PROPDMG", "CROPDMG", "EVTYPE")])

#Removing the collumns
StormDF2 <- StormData2[, c(rmcol2) := NULL]

#There are individual recordings for each incident
#We need to see it calculated as a sum 
StormDF2.1 <- StormDF2[, .(CROPDMG = sum(CROPDMG), PROPDMG = sum(PROPDMG)), by = .(EVTYPE)]

#ordering it by greatest damage to crops and property. 
StormPROPDMG <- StormDF2.1[order(-PROPDMG)]
StormPROPDMG <- StormPROPDMG[1:10]

StormCROPDMG <- StormDF2.1[order(-CROPDMG)]
StormCROPDMG <- StormCROPDMG[1:10]

#pushing them into one dataset
StormDMG <- rbind(StormPROPDMG, StormCROPDMG)

#Making sure there are not duplicated
StormDMG <- melt(StormDMG, id.vars = "EVTYPE", variable.name = "Damage")

healthChart2 <- ggplot(StormDMG, aes(x=reorder(EVTYPE, -value), y = value), yaxt = "n")
healthChart2 = healthChart2 +geom_bar(stat = "identity", aes(fill=Damage), position = "dodge")
healthChart2 = healthChart2 + ylab("Count")
healthChart2 = healthChart2 + xlab("Event")
healthChart2 = healthChart2 + theme_light()
healthChart2 = healthChart2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
healthChart2 = healthChart2 + ggtitle("Catastrophies that cause the most:") + theme(plot.title = element_text(hjust = 0.5))

#Exporting it into excel because I want to
library("writexl")
write_xlsx(StormDMG, "/Users/adamsarissky/Desktop/R/StormDMG.xlsx")

