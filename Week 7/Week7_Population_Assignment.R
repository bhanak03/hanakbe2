# Load the "anytime" and "ggplot2" packages to complete this week's assignment.

install.packages(c("ggplot2","anytime"))
library(ggplot2)
library(anytime)

setwd("C:/GitHub/hanakbe2/Week 7")
data <- read.csv("Plankton_move_average.csv", header=TRUE)

install.packages("leafpop")
library(leafpop)

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

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
# What is one relationship the third species MIGHT have to the first two? (2 pts)

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
legend("topright", c("Rabid foxes", "Cute bunnies"), lty = c(1,2), col = c(1,2), box.lwd = 0)

Pars <- c(alpha = 4, beta = 0.5, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "Time", ylab = "Population")
legend("topright", c("Rabid foxes", "Cute bunnies"), lty = c(1,2), col = c(1,2), box.lwd = 0)

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
# Are there other paramenter changes that could have created the same end result? (2 pts)
# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)




