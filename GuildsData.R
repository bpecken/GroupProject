setwd("C:/Users/dusti/Box Sync/Personal Computer/Quantitative Biodiversity/Group Project/GroupProject")
rm(list=ls())

library(vegan)
library(psych)
library(ggplot2)

Guilds <- read.csv("ForagingGuildsDataFile.csv")

str(Guilds)
summary(Guilds)

sem <- function(x){
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

#Compute means for omnivores and insectivores in the different mortality classes

Omnivores.Mean <- tapply(Guilds$Omn_16, Guilds$mortality.class, mean)

Insectivores.Mean <- tapply(Guilds$Ins_29, Guilds$mortality.class, mean)

#Compute standard errors for those means

Omnivores.SE <- tapply(Guilds$Omn_16, Guilds$mortality.class, sem)

Insectivores.SE <- tapply(Guilds$Ins_29, Guilds$mortality.class, sem)

#Omnivore bar plot
OmnivorePlot <- barplot(Omnivores.Mean, ylim = c(0, round(1.5*max(Omnivores.Mean), digits = 0)),
               pch = 15, cex = 1.25, las = 1, cex.lab = 1.4, cex.axis = 1.25,
               xlab = "mortality class",
               ylab = "mean site abundance",
               names.arg = c("low", "medium", "high A", "high B"))

arrows(x0 = OmnivorePlot, y0 = Omnivores.Mean, y1 = Omnivores.Mean - Omnivores.SE, angle = 90,
       length = 0.1, lwd = 1)
arrows(x0 = OmnivorePlot, y0 = Omnivores.Mean , y1 = Omnivores.Mean + Omnivores.SE, angle = 90,
       length = 0.1, lwd = 1)

#Insectivore bar plot
InsectivorePlot <- barplot(Insectivores.Mean, ylim = c(0, round(1.5*max(Insectivores.Mean), digits = 0)),
                        pch = 15, cex = 1.25, las = 1, cex.lab = 1.4, cex.axis = 1.25,
                        xlab = "mortality class",
                        ylab = "mean site abundance",
                        names.arg = c("low", "medium", "high A", "high B"))

arrows(x0 = InsectivorePlot, y0 = Insectivores.Mean, y1 = Insectivores.Mean - Insectivores.SE, angle = 90,
       length = 0.1, lwd = 1)
arrows(x0 = InsectivorePlot, y0 = Insectivores.Mean , y1 = Insectivores.Mean + Insectivores.SE, angle = 90,
       length = 0.1, lwd = 1)




#Try to get the omnivore and insectivore bars on same plot

#Run Omnivore Anova

OmnivoreAnova <- aov(Guilds$Omn_16~Guilds$mortality.class, data = Guilds)
OmnivoreAnova

summary(OmnivoreAnova)
TukeyHSD(OmnivoreAnova)

#Run Insectivore ANOVA

InsectivoreAnova <- aov(Guilds$Ins_29~Guilds$mortality.class, data = Guilds)

summary(InsectivoreAnova)
TukeyHSD(OmnivoreAnova)




