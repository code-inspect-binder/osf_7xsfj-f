##############################################
## Haegeli et al. (2020) - Code for Fig. 3  ##
## May 28, 2020                             ##
##############################################

## Getting data
load("AtmosOsc.rda")
Osc <- AtmosOsc

## Correlations
round(cor <- cor(Osc[, 2:5]), 2)

## Averaging of Pacific Oscillations
Osc$PO <- (Osc$MEI + Osc$PDO + Osc$PNA)/3
round(cor <- cor(Osc[, 2:6]), 2)

## Opening  png
png(filename = "Fig03.png", width = 8, height = 8, units = "cm", pointsize = 9, bg = "white", res = 300, type = c("cairo"))

## Defining margins
par(mar=c(5.1,3.6,2.1,2.1))

## Plotting
plot(Osc$Season, Osc$AO, type = "n", ylim = c(-2.5, 2.5),xaxt = "n", ylab = "", xlab = "", las = 1)

## Axes
axis(side = 1, at = c(2010:2019), labels = c(2010:2019), las = 2)
mtext("Season", side = 1, line = 3.5)
mtext("Oscillation Index", side = 2, line = 2.5)

## Grid
abline(h = 0, col = "grey", lty = 3, lwd = 0.5)
abline(v = Osc$Season, col = "grey", lty = 3, lwd = 0.5)

## Lines
lines(Osc$Season, Osc$MEI, col = "#A9A9A9", type = "b", pch = 23, lty = 1, cex = 1)
lines(Osc$Season, Osc$MEI, bg = "#D3D3D3", col = "#A9A9A9", type = "p", pch = 23, lty = 1, cex = 1)

lines(Osc$Season, Osc$PDO, col = "#A9A9A9", type = "b", pch = 24, lty = 1, cex = 1)
lines(Osc$Season, Osc$PDO, bg = "#D3D3D3", col = "#A9A9A9", type = "p", pch = 24, lty = 1, cex = 1)

lines(Osc$Season, Osc$PNA, col = "#A9A9A9", type = "b", pch = 25, lty = 1, cex = 1)
lines(Osc$Season, Osc$PNA, bg = "#D3D3D3", col = "#A9A9A9", type = "p", pch = 25, lty = 1, cex = 1)

lines(Osc$Season, Osc$PO, col = "red", type = "b", pch = 21, lty = 1, lwd = 1.5, cex = 1)
lines(Osc$Season, Osc$PO, bg = "red", type = "p", pch = 21, lty = 1, lwd = 1.5, cex = 1)

lines(Osc$Season, Osc$AO, col = "blue", type = "b", pch = 22, lty = 1, lwd = 1.5, cex = 1)
lines(Osc$Season, Osc$AO, bg = "blue", type = "p", pch = 22, lty = 1, lwd = 1.5, cex = 1)

## Legends
legend(x = 2016.4, y = -1.15, legend = c("ENSO", "PNA", "PDO"), pch = c(23, 25, 24),
       pt.bg = c("#D3D3D3", "#D3D3D3", "#D3D3D3"), cex = 1.0, bty = "n", )

legend(x = 2014.5, y = -1.15, legend = c("AO", "PO"), pch = c(22, 21),
       pt.bg = c("blue", "red"), cex = 1.0, bty = "n")

## Close png device
dev.off()


