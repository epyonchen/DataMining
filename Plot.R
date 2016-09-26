library(ggplot2)
dataset <- read.csv("GDELT.csv")
dataset$MonthYear <- as.factor(dataset$MonthYear)
dataset$EventCode <- as.factor(dataset$EventCode)
g <- ggplot(dataset,aes(x = MonthYear, fill = EventCode)) + geom_bar(stat="count", position = "stack")  + 
theme(axis.text.x=element_text(angle=45,colour="black", hjust = 1)) 
g
