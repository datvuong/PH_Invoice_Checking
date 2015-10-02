library(ggplot2)
library(scales)
library(dplyr)

load("../4_RData/rawData.RData")
boxplot(a, log="y")
boxplot(a, log="y", col = "bisque")
options(scipen=999)

ggplot(a) + geom_boxplot()
rawData %<>% mutate(varianceWeight=TotalSKUWeight-Invoice.Chargeable.Weight)
rawData <- rawData %>% mutate(varianceWeight=abs(TotalSKUWeight-Invoice.Chargeable.Weight))

fmt <- function(){
    function(x) format(x,nsmall = 0,scientific = FALSE)
}

ggplot(filter(rawData,varianceWeight!=0), aes(x="WeightVariance",y=varianceWeight)) + 
    geom_boxplot() + coord_flip() +
    scale_y_continuous(name="Weight Variance",
                       trans=log10_trans(), breaks=log_breaks(n=10),
                       labels = fmt())
newData <- rawData
oldData <- rawData

finalAmount_NoCheck_New <- sum(filter(newData, ManualCheck=="No")$FinalAmount)
finalAmount_NoCheck_Old <- sum(filter(oldData, ManualCheck=="No")$FinalAmount)
finalAmount_NoCheck_Old - finalAmount_NoCheck_New

finalAmount_Check_New <- sum(filter(newData, ManualCheck=="Yes")$FinalAmount)
finalAmount_Check_Old <- sum(filter(oldData, ManualCheck=="Yes")$FinalAmount)
finalAmount_Check_Old - finalAmount_Check_New

((finalAmount_NoCheck_Old+finalAmount_Check_Old) - (finalAmount_NoCheck_New+finalAmount_Check_New))/46.51
