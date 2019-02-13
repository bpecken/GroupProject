rm(list = ls())
setwd("~/GitHub/GroupProject")
package.list <- c('vegan', 'ade4', 'viridis', 'gplots', 'BiodiversityR', 'indicspecies', 'dplyr', 'decostand')
for (package in package.list) {
  if (!require(package, character.only=TRUE, quietly=TRUE)) {
    install.packages(package)
    library(package, character.only=TRUE)
  }
}

bird <- read.csv("hf085-01-bird.csv", header = TRUE)
over <- read.csv("hf085-02-overstory.csv", header = TRUE)
under <- read.csv("hf085-03-understory.csv", header = TRUE)

bird <- arrange(bird, mortality.class)
over <- arrange(over, mortality.class)
under <- arrange(under, mortality.class)

bird.num <- bird[3:51]
over.num <- over[3:23]
under.num <- under[3:20]

# bird.num <- decostand(bird.num, method = "pa")
# over.num <- decostand(over.num, method = "pa")
# under.num <- decostand(under.num, method = "pa")

# Sorenson & PCoA
bird.rm <- vegdist(bird.num, method = "bray", binary = TRUE)
bird.pcoa <- cmdscale(bird.rm, eig = TRUE, k = 3)

explainvar1 <- round(bird.pcoa$eig[1] / sum(bird.pcoa$eig), 3) * 100
explainvar2 <- round(bird.pcoa$eig[2] / sum(bird.pcoa$eig), 3) * 100
explainvar3 <- round(bird.pcoa$eig[3] / sum(bird.pcoa$eig), 3) * 100
sum.eig <- sum(explainvar1, explainvar2, explainvar3)

# Plotting PCoA
palette(c('lightblue1', 'lightblue3', 'gray50', 'darkolivegreen3'))
par(mar = c(5, 5, 1, 2) + 0.1)
plot(bird.pcoa$points[ ,1], bird.pcoa$points[ ,2], ylim = c(-0.4, 0.7),
     xlab = paste("PCoA 1 (", explainvar1, "%)", sep = ""),
     ylab = paste("PCoA 2 (", explainvar2, "%)", sep = ""),
     pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(bird.pcoa$points[ ,1], bird.pcoa$points[ ,2],
       pch = 19, cex = 3, bg = bird$mortality.class, col = bird$mortality.class)
legend("topleft", legend=c("Low", "Med", "Hi-A", "Hi-B"), fill=c('gray50', 'darkolivegreen3', 'lightblue1', 'lightblue3'))

# phi coefficient of association
mortality <- c(rep("Hi-A", 10), rep("Hi-B", 11), rep("Low", 11), rep("Med", 8))

bird.rel <- decostand(bird.num, method = "total")
phi <- multipatt(bird.rel, cluster = mortality, func = "r.g", control = how(nperm=999))
summary(phi)


# mantel test
bird.dist <- vegdist(bird.num, method = "bray")
over.dist <- vegdist(over.num, method = "bray")

bird2.dist <- vegdist(bird.num[-29,], method = "bray")
under.dist <- vegdist(under.num[-29,], method = "bray")

mantel(bird.dist, over.dist)
mantel(bird2.dist, under.dist)


