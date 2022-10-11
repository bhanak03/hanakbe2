# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."


getwd()

setwd("C:/GitHub/hanakbe2/Week 6")
data <- read.csv("Toscano_Griffen_Data.csv")
library(MASS)
library(MuMIn)
library(mgcv)
install.packages("lme4")
library(lme4)

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.
?glmmPQL

data$prop.cons <- data$eaten/data$prey 

glmm.add <- glmmPQL(prop.cons~claw.width + activity.level + carapace.width, family = gaussian, random = ~ 1 | block, data = data)
summary(glmm.add)


glmm.int <- glmmPQL(prop.cons~carapace.width * claw.width + activity.level, family = gaussian, random = ~ 1 | block, data = data)
summary(glmm.int)

#I initially had family as binomial because I thought that was correct but guassian
#got me very similar graphs and less errors so I went with that.

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey 

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#it's creating a proportion and adding a line to a data frame.

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)
#the model with the multiplication (i wrote it as interactive but idk) because the p value is significant in most of them as
#opposed to the other one where most of the p values are not significant.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.add)
plot(glmm.int)
 #both models seem like good fits, their residuals are equally spread.

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.mod1 <- gam(prop.cons~claw.width + activity.level, family = gaussian, random = list(ID=~ 1), data = data)
summary(gam.mod1)
gam.mod2 <- gam(prop.cons~carapace.width, family = gaussian, random = list(ID=~ 1), data = data)
summary(gam.mod2)

# (Q4) - Which model is a better fit? (2 pt)
#the gam models are better fits because their residuals are more spread out. 

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)

plot(gam.mod1$residuals, ylim = c(-.1,.1))
AIC(gam.mod1)
plot(gam.mod2$residuals, ylim = c(-.1,.1))
AIC(gam.mod2)



