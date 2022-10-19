# Load the "anytime" and "ggplot2" packages to complete this week's assignment.

install.packages(c("ggplot2","anytime"))
library(ggplot2)
library(anytime)

setwd("C:/GitHub/hanakbe2/Week 7")
data <- read.csv("Plankton_move_average.csv", header=TRUE)


# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).

# (1) - Which species is most likely to be r-selected prey and which its primary predator? 
#D.mendotae are R selected due to their high populations and Limncalanus are their primary predator. 
# What is one relationship the third species MIGHT have to the first two? (2 pts)
#The third species could be a predator to both of the other species, explaining why its population still follows the general curvature of the others at a lower population.

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
install.packages("deSolve")
library(deSolve)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "Time", ylab = "Population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
#alpha represents how quickly the fluctuations in population occur, ie how fast the prey repopulate
#beta is how fast the prey is being killed
#gamma is how stable populations are
#delta is predator life spans

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
# Are there other parameter changes that could have created the same end result? (2 pts)


Pars <- c(alpha =3, beta = 0.5, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "Time", ylab = "Population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)

#by increasing alpha by 1, the graph more accurately represents the increase in prey followed by the increase in predator species, as opposed to an increase in prey first.
#changing most of these values more than a point or two causes the graph to flat line, or shows predator increase first, adjusting alpha was the best solution.
#the sensitivity of these points indicates that these plankton populations are fragile.

# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)



