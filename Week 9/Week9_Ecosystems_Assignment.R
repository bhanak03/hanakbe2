# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

library(readxl)

setwd("C:/GitHub/hanakbe2/Week 9")
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")

abiotic <- as.data.frame(abiotic.tibble)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)

abiotic.names <- paste(abiotic$Parcel, abiotic$Land_Use)
abiotic$names <- abiotic.names

invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")



abiotic.means1 <- abiotic.means[,c(-1,-2,-3,-5,-6,-16)]
invert.means1 <- invert.means[-5,c(-1:-3,-73)]


invert.means2 <- as.data.frame(sapply(invert.means1, as.numeric))
abiotic.means2 <- as.data.frame(sapply(abiotic.means1, as.numeric))


library(vegan)
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord
anova(ord)
plot(ord, ylim = c(-2,2), xlim = c(-5,5))  


ord <- rda(invert.means2 ~., abiotic.means2)
ord.int <- rda(invert.means2 ~1, abiotic.means2) 

step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")


#there were no significant variables for this data set which indicates that abiotic factors do not influence the invertebrate community.

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.


colnames(invert.means2)

<<<<<<< Updated upstream
mod1 <- lm(invert.means2 ~ abiotic.means2)
=======
mod1 <- lm(invert.means2$Vitrina_pellucida ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
>>>>>>> Stashed changes
summary(mod1)
anova(mod1)
AIC(mod1)

mod2 <- lm(invert.means2$Vitrina_pellucida ~ Perc_ash , abiotic.means2)
summary(mod2)
anova(mod2)
AIC(mod2)

mod3 <- lm(invert.means2$Vitrina_pellucida ~ Kalium , abiotic.means2)
summary(mod3)
anova(mod3)
AIC(mod3)

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


