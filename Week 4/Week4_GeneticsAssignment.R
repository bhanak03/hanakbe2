# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)
  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).
#the data points don't appear to be correlated in any way, they are scattered across the board, even most color groups are not all next to each other.
#the two yellow dots are correlated closer than most of the other locations. Even if a specific color has a cluster close together, most of them also have
#significant outliers, causing the plot to look so haphazard. 

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)

install.packages("stability")
library(stability)
data(ge_data)
ge_data

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
mod.env <- lm(ge_data$Yield ~ ge_data$Env)
mod.geno <- lm(ge_data$Yield ~ ge_data$Gen)

# Test the significance of both models and look at the model summary. (3 points each)
  # Which model is a better fit to explain the yield response, and WHY? (6 points)
  # Hint: Does one model seem more likely to be over-fitted?
anova(mod.env)
anova(mod.geno)
summary(mod.env)
summary(mod.geno)

plot(ge_data$Yield ~ ge_data$Env)
plot(ge_data$Yield ~ ge_data$Gen)
# I believe the environment is a better fit, it has a higher R value and Genotype has so many points that it looks like its trying to over-fit the model.

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)

#KSK would be the worst because it has the lowest estimate.


#also i'm a biology major I have no clue what some of these questions are asking.
#at least in the very Matt way in which you have asked them.