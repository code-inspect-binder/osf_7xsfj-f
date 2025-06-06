################################################
## Haegeli et al. (2020) - Code for Fig. 5-8  ##
## May 28, 2020                               ##
################################################

## Required libraries
if (!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}
if (!require("glmmTMB")) {install.packages("glmmTMB"); require("glmmTMB")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}
if (!require("effects")) {install.packages("effect"); require("effect")}


## Loading data
## ************
rm(list = ls())
load("AnalysisWorkspace.RData")

## Quick descriptives
hazard <- names(DataBeta)[11:20]      ## Extracting hazard names 
hazard
length(hazard)                        ## 9 problems + NoProb in total


## Parameter setting for figures
## *****************************
ConfInt <- 0.95
InclMIA <- TRUE
Chart_ylim = c(0, 1)

## Figure 5
AvProb <- "Storm"
FileName <- "Fig05_Storm.png"
LegendLoc <- c("top", "top", "top", "top")

## Figure 6
AvProb <- "Wind"
FileName <- "Fig06_Wind.png"
LegendLoc <- c("bottom", "bottom", "top", "top")

## Figure 7
AvProb <- "NoProb"
FileName <- "Fig07_NoProb.png"
LegendLoc <- c("top", "top", "top", "top")

## Figure 8
AvProb <- "DPers"
FileName <- "Fig08_Dpers.png"
LegendLoc <- c("top", "top", "top", "top")



## Functions for plotting models
## *****************************

## Extracts significance from models for chart legend
extractSign <- function (model, modeltype, MtnRanges, InclMIA = TRUE) {
  
  coef <- summary(model)$coefficients$cond
  
  SignDF <- data.frame(mtnrng = MtnRanges, main = NA, int_ao = NA, int_po = NA)
  
  if (InclMIA) {
  
    if (modeltype == "int") {
      
      if (coef[row.names(coef) == "AO:MtnRange1", 4] < 0.05) {SignDF$int_ao[SignDF$mtnrng == "Coast - North"] <- "I"}
      if (coef[row.names(coef) == "AO:MtnRange2", 4] < 0.05) {SignDF$int_ao[SignDF$mtnrng == "Coast - South"] <- "I"}
      if (coef[row.names(coef) == "AO:MtnRange3", 4] < 0.05) {SignDF$int_ao[SignDF$mtnrng == "Columbia - North"] <- "I"}
      if (coef[row.names(coef) == "AO:MtnRange4", 4] < 0.05) {SignDF$int_ao[SignDF$mtnrng == "Columbia - South"] <- "I"}
      if (coef[row.names(coef) == "AO:MtnRange5", 4] < 0.05) {SignDF$int_ao[SignDF$mtnrng == "Rockies - North"] <- "I"}
      
      if (coef[row.names(coef) == "MtnRange1:PO", 4] < 0.05) {SignDF$int_po[SignDF$mtnrng == "Coast - North"] <- "I"}
      if (coef[row.names(coef) == "MtnRange2:PO", 4] < 0.05) {SignDF$int_po[SignDF$mtnrng == "Coast - South"] <- "I"}
      if (coef[row.names(coef) == "MtnRange3:PO", 4] < 0.05) {SignDF$int_po[SignDF$mtnrng == "Columbia - North"] <- "I"}
      if (coef[row.names(coef) == "MtnRange4:PO", 4] < 0.05) {SignDF$int_po[SignDF$mtnrng == "Columbia - South"] <- "I"}
      if (coef[row.names(coef) == "MtnRange5:PO", 4] < 0.05) {SignDF$int_po[SignDF$mtnrng == "Rockies - North"] <- "I"}
      
    }
    
  }
  
  SignAOMain <- paste("AO: ", format(round(coef[row.names(coef) == "AO", 1], 3), nsmall=3), " (", format(round(coef[row.names(coef) == "AO", 4], 3), nsmall=3),")")
  SignPOMain <- paste("PO: ", format(round(coef[row.names(coef) == "PO", 1], 3), nsmall=3), " (", format(round(coef[row.names(coef) == "PO", 4], 3), nsmall=3),")")
  
  return(list(SignAOMain = SignAOMain, 
              SignPOMain = SignPOMain, 
              SignDF = SignDF))
  
}

## Plotting function
plot_aopo <- function(DF, model_type, ConfInt,
                      HzdSit = "",
                      MtnRngs = c("Coast - North", "Coast - South", "Columbia - North", "Columbia - South", "Rockies - North", "Rockies - South"),
                      MtnRngCol = c("#1f78b4", "#1f78b4", "#33a02c", "#33a02c", "#ff7f00", "#ff7f00"),
                      MtnRngLty = c(1, 3, 1, 3, 1, 3),
                      MtnRngPch = c(1, 4, 1, 4, 1, 4),
                      WithLegend = T,
                      LegendText = MtnRngs,
                      LegendLoc = "top",
                      OscPValue = NA,
                      RawData = NULL,
                      RawDataSize = 0.5,
                      ...) {
  
  plot(DF$x, DF$pred, type = "n", las = 1, ...)
  abline(v = 0, lty = 2, col = "grey")
  
  for (ind in 1:length(MtnRngs)) {
    polygon(x = c(DF$x[DF$grp == MtnRngs[ind]], rev(DF$x[DF$grp == MtnRngs[ind]])),
            y = c(DF$low[DF$grp == MtnRngs[ind]], rev(DF$high[DF$grp == MtnRngs[ind]])),
            col = paste0(MtnRngCol[ind], "30"), border = NA) 
  }
  for (ind in 1:length(MtnRngs)) {
    lines(DF$x[DF$grp == MtnRngs[ind]], DF$pred[DF$grp == MtnRngs[ind]], col = MtnRngCol[ind], lwd = 1, lty = MtnRngLty[ind])
  }
  
  if (!is.null(RawData)) {
    for (ind in 1:length(MtnRngs)) {
      points(RawData[RawData$MtnRange == MtnRngs[ind],1], RawData[RawData$MtnRange == MtnRngs[ind],2], pch = MtnRngPch[ind], col = MtnRngCol[ind], cex = RawDataSize)  
    }
  }
  
  if (WithLegend) {
    legend(LegendLoc, legend = LegendText, bty = "o", col = MtnRngCol, lty = MtnRngLty, pch = MtnRngPch, lwd = 1, cex = 0.75, bg = "#FFFFFF80", box.lty = 0, ncol = 3)
  }
  
  box()
  
} 


## Explore and visualize single model
## **********************************

## Region labels
MtnRanges <- c("Coast-N", "Coast-S", "Columbia-N", "Columbia-S", "Rockies-N", "Rockies-S")

## Index for model that needs to be plotted
model_ind <- which(hazard == AvProb)

cat(paste0("\n=================================", 
           "\n=== ", model_ind, ") MODELS FOR ", toupper(AvProb), " ===",
           "\n=================================\n\n"))

## Get models
fit_main_Alp <- beta_fit_AlpTl_main[[model_ind]]
fit_int_Alp <- beta_fit_AlpTl_int[[model_ind]]
fit_main_Btl <- beta_fit_Btl_main[[model_ind]]
fit_int_Btl <- beta_fit_Btl_int[[model_ind]]

## Identify relevant av problem column in original data and write to ProbPlot column
Data$ProbPlot <- Data[,AvProb]
head(Data)

## ALP/TL: Choose models
cat(paste0(toupper(AvProb), ": ALPINE/TREELINE LIKELIHOOD TEST", "\n***************************************\n\n"))
print(anova(fit_main_Alp, fit_int_Alp))

if (anova(fit_main_Alp, fit_int_Alp)$`Pr(>Chisq)`[2] < 0.05) {
  model_Alp <- "int"
  fit_Alp <- fit_int_Alp
} else {
  model_Alp <- "main"
  fit_Alp <- fit_main_Alp
}

## ALP/TL: Predict values for charts and ranges
if (model_Alp == "int") {
  pred_Alp <- get_model_data(fit_Alp, type = "int", terms = c("AO", "PO"), ci.lvl = ConfInt)
} else {
  pred_Alp <- list(get_model_data(fit_Alp, type = "eff", terms = c("AO", "MtnRange"), ci.lvl = ConfInt), get_model_data(fit_Alp, type = "eff", terms = c("PO", "MtnRange"), ci.lvl = ConfInt))
}

ao_alp <- data.frame(x = pred_Alp[[1]]$x, pred = pred_Alp[[1]]$predicted, grp = pred_Alp[[1]]$group, low = pred_Alp[[1]]$conf.low, high = pred_Alp[[1]]$conf.high)
po_alp <- data.frame(x = pred_Alp[[2]]$x, pred = pred_Alp[[2]]$predicted, grp = pred_Alp[[2]]$group, low = pred_Alp[[2]]$conf.low, high = pred_Alp[[2]]$conf.high)

## ALP/TL: Print summaries and differences at the end of the ranges
cat(paste0("\n\n", toupper(AvProb), ": ALPINE/TREELINE FINAL MODEL", "\n*********************************\n\n"))
print(summary(fit_Alp))
compareRangesWithPvalues(fit_Alp)

## ALP/TL: Extract significance and create labels for legend
Sign_alp <- extractSign(fit_Alp, model_Alp, MtnRanges, InclMIA = InclMIA)
ldg_alp_ao <- paste0(Sign_alp$SignDF$mtnrng, ifelse(is.na(Sign_alp$SignDF$main), "", " - M"), ifelse(is.na(Sign_alp$SignDF$int_ao), "", " - I"))
ldg_alp_po <- paste0(Sign_alp$SignDF$mtnrng, ifelse(is.na(Sign_alp$SignDF$main), "", " - M"), ifelse(is.na(Sign_alp$SignDF$int_po), "", " - I"))


## BTL: Choose models
cat(paste0("\n\n", toupper(AvProb), ": BELOW TREELINE LIKEIHOOD TEST", "\n**********************************\n\n"))
print(anova(fit_main_Btl, fit_int_Btl))

if (anova(fit_main_Btl, fit_int_Btl)$`Pr(>Chisq)`[2] < 0.05) {
  model_Btl <- "int"
  fit_Btl <- fit_int_Btl
} else {
  model_Btl <- "main"
  fit_Btl <- fit_main_Btl
}

## BTL: Predict values for charts and ranges
if (model_Btl == "int") {
  pred_Btl <- get_model_data(fit_Btl, type = "int", terms = c("AO", "PO"), ci.lvl = ConfInt)
} else {
  pred_Btl <- list(get_model_data(fit_Btl, type = "eff", terms = c("AO", "MtnRange"), ci.lvl = ConfInt), get_model_data(fit_Btl, type = "eff", terms = c("PO", "MtnRange"), ci.lvl = ConfInt))
}

ao_btl <- data.frame(x = pred_Btl[[1]]$x, pred = pred_Btl[[1]]$predicted, grp = pred_Btl[[1]]$group, low = pred_Btl[[1]]$conf.low, high = pred_Btl[[1]]$conf.high)
po_btl <- data.frame(x = pred_Btl[[2]]$x, pred = pred_Btl[[2]]$predicted, grp = pred_Btl[[2]]$group, low = pred_Btl[[2]]$conf.low, high = pred_Btl[[2]]$conf.high)

## BTL: Print summaries and differences at the end of the ranges
cat(paste0("\n\n", toupper(AvProb), ": BELOW TREELINE FINAL MODEL", "\n**********************************\n\n"))
print(summary(fit_Btl))
compareRangesWithPvalues(fit_Btl)

## BTL: Extract significance and create labels for legend
Sign_btl <- extractSign(fit_Btl, model_Btl, MtnRanges, InclMIA = InclMIA)
ldg_btl_ao <- paste0(Sign_btl$SignDF$mtnrng, ifelse(is.na(Sign_btl$SignDF$main), "", " - M"), ifelse(is.na(Sign_btl$SignDF$int_ao), "", " - I"))
ldg_btl_po <- paste0(Sign_btl$SignDF$mtnrng, ifelse(is.na(Sign_btl$SignDF$main), "", " - M"), ifelse(is.na(Sign_btl$SignDF$int_po), "", " - I"))

 
## Plotting

png(filename = FileName, width = 16, height = 12, units = "cm", pointsize = 9, bg = "white", res = 700, type = c("cairo"))
par(mfrow=c(2,2))

par(mar = c(3.6, 3.8, 3.1, 1.1))
plot_aopo(ao_alp, model_Alp, HzdSit = AvProb, ylim = Chart_ylim, ConfInt = ConfInt,
          LegendText = ldg_alp_ao, LegendLoc = LegendLoc[1],
          xlab = "", ylab = "Prevalence",
          main = "a) Arctic Oscillation - Alpine and Treeline",
          RawData = Data[Data$ELEVATION != "Btl", c("AO", "ProbPlot", "MtnRange")]) 

par(mar = c(3.6, 3.1, 3.1, 1.8))
plot_aopo(po_alp, model_Alp, HzdSit = AvProb, ylim = Chart_ylim, ConfInt = ConfInt, 
          LegendText = ldg_alp_po, LegendLoc = LegendLoc[2],  
          xlab = "", ylab = "",
          main = "c) Pacific Oscillations - Alpine and Treeline",
          RawData = Data[Data$ELEVATION != "Btl", c("PO", "ProbPlot", "MtnRange")]) 

par(mar = c(4.1, 3.8, 2.6, 1.1))
plot_aopo(ao_btl, model_Btl, HzdSit = AvProb, ylim = Chart_ylim, ConfInt = ConfInt,
          LegendText = ldg_btl_ao, LegendLoc = LegendLoc[3],
          xlab = "AO index", ylab = "Prevalence",
          main = "b) Arctic Oscillation - Below Treeline",
          RawData = Data[Data$ELEVATION == "Btl", c("AO", "ProbPlot", "MtnRange")]) 

par(mar = c(4.1, 3.1, 2.6, 1.8))
plot_aopo(po_btl, model_Btl, HzdSit = AvProb, ylim = Chart_ylim, ConfInt = ConfInt,
          LegendText = ldg_btl_po, LegendLoc = LegendLoc[4],
          xlab = "PO index", ylab = "",
          main = "d) Pacific Oscillations - Below Treeline",
          RawData = Data[Data$ELEVATION == "Btl", c("PO", "ProbPlot", "MtnRange")]) 

dev.off()
  