setwd("C:/Users/dusti/GitHub/QB2019_Brewer/GroupProjectData")
rm(list=ls())
data <- read.csv("hf085-01-bird.csv", header = TRUE)
data

library(vegan)
library(psych)
library(corrplot)


str(data)
data.num <- data[ ,3:51]

S.obs <- function(x = ""){
  rowSums(x > 0) * 1
}


obs.rich <- S.obs(data.num)

obs.rich

S.chao2 <- function(site = "", SbyS = ""){
  SbyS = as.data.frame(SbyS)
  x = SbyS[site, ]
  SbyS.pa <- (SbyS > 0) * 1
  Q1 = sum(colSums(SbyS.pa) == 1)
  Q2 = sum(colSums(SbyS.pa) == 2)
  S.chao2 = S.obs(x) + (Q1^2)/(2*Q2)
  return(S.chao2)
}

S.chao2

est.rich <- S.chao2(1:40, data.num)
est.rich

data.rich <- cbind(data, est.rich)
data.rich

m1 <- lm(est.rich ~ mortality.class, data = data.rich)
summary(m1)

?lm


mortality <- factor(data.rich$mortality.class, levels = c("Low", "Med", "Hi-A", "Hi-B"))
mortality

data.means <- tapply(data.rich$est.rich, mortality, mean)

sem <- function(x){
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

data.sem <- tapply(data.rich$est.rich, mortality, sem)


bp1 <- barplot(data.means, ylim = c(0, round(max(data.rich$est.rich), digits = 0)),
               pch = 15, cex = 1.25, las = 1, cex.lab = 1.4, cex.axis = 1.25,
               xlab = "mortality class",
               ylab = "estimated richness",
               names.arg = c("low", "medium", "high A", "high B"))

arrows(x0 = bp1, y0 = data.means, y1 = data.means - data.sem, angle = 90,
       length = 0.1, lwd = 1)
arrows(x0 = bp1, y0 = data.means, y1 = data.means + data.sem, angle = 90,
       length = 0.1, lwd = 1)


fitanova <- aov(est.rich ~ mortality, data = data.rich)
fitanova

summary(fitanova)
TukeyHSD(fitanova)

par(mfrow = c(2,2), mar = c(5.1, 4.1, 4.1, 2.1))
plot(fitanova)

