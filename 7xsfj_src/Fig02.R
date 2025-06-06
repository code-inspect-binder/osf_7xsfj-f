## ----------------------------- Bret Beta Mixed-Effects Models ---------------------
if (!require("RColorBrewer")) {install.packages("RColorBrewer"); require("RColorBrewer")}


## ------ data preparation -----
load("AvProbDF.rda")
Data <- AvProbDF

head(Data)
dim(Data)
str(Data)

summary(Data)
as.data.frame(table(Data$REGION))

## Adding MtnRange column
Data$MtnRange[Data$REGION %in% c("Northwest - BC", "Northwest Coastal", "Northwest Inland")] <- "Coast - North"
Data$MtnRange[Data$REGION %in% c("South Coast", "Sea-to-Sky", "South Coast Inland", "South Coast - Inland", "Sea-to-Sky")] <- "Coast - South"
Data$MtnRange[Data$REGION %in% c("North Columbia - Cariboos", "North Columbia - Monashees & Selkirks", "Cariboos", "North Columbia")] <- "Columbia - North"
Data$MtnRange[Data$REGION %in% c("South Columbia", "Purcells", "Kootenay Boundary")] <- "Columbia - South"
Data$MtnRange[Data$REGION %in% c("Banff, Yoho and Kootenay", "Jasper", "Kananaskis Country")] <- "Rockies - North"
Data$MtnRange[Data$REGION %in% c("Lizard Range", "South Rockies")] <- "Rockies - South"
Data$MtnRange <- factor(Data$MtnRange, levels = c("Coast - North", "Coast - South", "Columbia - North", "Columbia - South", "Rockies - North", "Rockies - South"))

table(Data$REGION, Data$MtnRange, useNA = "always")
table(Data$MtnRange, Data$Season, useNA = "always")

str(Data)

## Create hazard array
hazard <- names(Data)[2:11]      ## Extracting hazard names 
hazard
length(hazard)                   ## 9 av prob + no problem in total



## Plotting all
PlotData <- Data
mainpre <- "All"

Main <- c("Storm slab", "Wind slab", "Persistent slab", "Deep pers. slab", "All pers. slab", "Wet slab", 
          "Wet loose avalanche", "Dry loose avalanche", "Cornice avalanche", "No avalanche problem")

Col <- c(getAvCharColor("Storm slabs"), getAvCharColor("Wind slabs"), getAvCharColor("Persistent slabs"), 
         getAvCharColor("Deep persistent slabs"), getAvCharColor("Deep persistent slabs"), getAvCharColor("Wet slabs"), 
         getAvCharColor("Loose wet"), col = getAvCharColor("Loose dry"), col = getAvCharColor("Cornice"), "grey")



op <- par(mfrow = c(5,2))
par(mar = c(2.6, 4.1, 2.1, 2.1))
for (i in 1:length(hazard)) plot(PlotData$Season, PlotData[, hazard[i]], las = 1, ylim = c(0, 1), ylab = "Prevalence", xlab = "", main = Main[i], col = Col[i])
par(op)
par(mar = c(5.1, 4.1, 2.1, 2.1))

summary(Data[Data$ELEVATION == "Alp",])
summary(Data[Data$ELEVATION == "Tl",])
summary(Data[Data$ELEVATION == "Btl",])


## Plotting North Rockies - Not AvCan
PlotData <- Data[Data$MtnRange == "Rockies - North",]
mainpre <- "Rockies N"

op <- par(mfrow = c(3,3))
for (i in 1:length(hazard)) plot(PlotData$Season, PlotData[, hazard[i]], main = paste(mainpre, "-", hazard[i]), las = 1, ylim = c(0, 1), ylab = "Prevalence", xlab = "Season")
par(op)


## Plotting Only AvCan
PlotData <- Data[Data$MtnRange != "Rockies - North",]
mainpre <- "Only AvCan"

op <- par(mfrow = c(3,3))
for (i in 1:length(hazard)) plot(PlotData$Season, PlotData[, hazard[i]], main = paste(mainpre, "-", hazard[i]), las = 1, ylim = c(0, 1), ylab = "Prevalence", xlab = "Season")
par(op)
