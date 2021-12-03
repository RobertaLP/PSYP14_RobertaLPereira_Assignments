# Assignment Part 2 - Roberta Lopes Pereira

library(psych)    
require(smacof)   # needed for the sim2diss function
require(MASS)    
library(MASS)
library(smacof)

data(Nations)
summary(Nations)
describe(Nations)
str(Nations)

#Similarities to dissimilarities
#Nations.d = sim2diss(Nations, method = 1)
#Nations.d

Nations.d2 <- sim2diss(Nations, method = 9, to.dist = TRUE)
Nations.d2

#Non-metric multidiensional scale
# Cite (Kruskal, 1964) when describing the cutoff points for stress values. 
#The stress value has to be between 0 and 1.

N_mds2=isoMDS(Nations.d2)
N_mds2

# Stress value - the lower the better. Below 5 is a good fit. Decent fit - below 15-10.
####
#Plots

x <- N_mds2$points[,1] # 1 refers to 1st coordinate
y <- N_mds2$points[,2] # 2 refers to 2nd coordinate

plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(N_mds2$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(Nations), cex = 0.6)
abline(h=0, v=0, col = "gray60", lty = 2)

# Shepard plot

Nations_shep2 <- Shepard(Nations.d2, N_mds2$points)

plot(Nations_shep2, pch = 20, xlab = "Dissimilarity",
     ylab = "Distance", main = "Shepard's Diagram")
lines(Nations_shep2$x, Nations_shep2$yf, type = "S")


#book's version
plot(Nations_shep2, pch = 20, xlab = "Dissimilarity",
     ylab = "Distance", main = "Shepard's Diagram", xlim = range(Nations_shep2$x),
     ylim = range(Nations_shep2$x))
lines(Nations_shep2$x, Nations_shep2$yf, type = "S")
