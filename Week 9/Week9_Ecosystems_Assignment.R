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

mod1 <- lm(invert.means2$Vitrina_pellucida ~ pH , abiotic.means2)
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

mod4 <- lm(invert.means2$Vitrina_pellucida ~ totalN , abiotic.means2)
summary(mod4)
anova(mod4)
AIC(mod4)


mod5 <- lm(invert.means2$Vitrina_pellucida ~ Magnesium, abiotic.means2)
summary(mod5)
anova(mod5)
AIC(mod5)


mod6 <- lm(invert.means2$Vitrina_pellucida ~ Ca , abiotic.means2)
summary(mod6)
anova(mod6)
AIC(mod6)

mod7 <- lm(invert.means2$Vitrina_pellucida ~ Al , abiotic.means2)
summary(mod7)
anova(mod7)
AIC(mod7)

mod8 <- lm(invert.means2$Vitrina_pellucida ~ TotalP , abiotic.means2)
summary(mod8)
anova(mod8)
AIC(mod8)

mod9 <- lm(invert.means2$Vitrina_pellucida ~ OlsenP, abiotic.means2)
summary(mod9)
anova(mod9)
AIC(mod9)

#Kalium was the most significant predictor in the system. Potassium plays an important role in many 
#environmental cycles, so this is not surprising. 

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#there are a lot of factors that contribute to the health of an ecosystem and the organisms within
#that ecosystem. Comparing how abiotic factors like chemicals in the soil influence specific species
#and how different species influence each other can give a more rounded view of how the community flows.
#Potassium, which had the most importance in this data set, plays important roles in photosynthesis, cell
#size, and nutrient transportation in plants. 

#How does the significance of potassium for one species relate back to the lack of significance for the whole community? Otherwise great!
