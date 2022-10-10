# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

setwd("C:/GitHub/hanakbe2/Week 5")
data <- read.csv("Figure4Data.csv", header=TRUE)


library(ggplot2)
library(dplyr)
library(reshape2)

data.new <- data.frame(data[1:4])
data.new$freq <- data.new$Num.Cells.Progeny
data.new <- na.omit(data.new)
data.weight <- data.new[rep(row.names(data.new), data.new$freq), 1:4]
data.weight$log.progeny <- log(data.weight$Num.Cells.Progeny, 2)
means.progeny <- aggregate(data.weight$Num.Cells.Progeny ~ data.weight$Strain, FUN = mean)
sd.progeny <- aggregate(data.weight$Num.Cells.Progeny ~ data.weight$Strain, FUN = sd)
colnames(means.progeny) <- c("Strain","Mean.Num.Progeny")
colnames(sd.progeny) <- c("Strain","SD.Num.Progeny")
data.weight <- inner_join(data.weight,means.progeny, by = "Strain")
data.weight <- inner_join(data.weight,sd.progeny, by = "Strain")
data.new$log.progeny <- log(data.new$Num.Cells.Progeny, 2)
samplesizes <- summary(data.new$Strain)
samplesizes

data.weight$Strain <- factor(data.weight$Strain, levels = c('K101','K106',
                                                            'B506','B210',
                                                            'B201','B505',
                                                            'B211','B203',
                                                            'B204'))

data.new$Strain <- factor(data.new$Strain, levels = c('K101','K106',
                                                      'B506','B210',
                                                      'B201','B505',
                                                      'B211','B203',
                                                      'B204'))

myplot <- ggplot(data = data.weight,
                 aes(x=Strain,
                     y=log.progeny)) +
  geom_boxplot(size = 1.2) +
  geom_jitter(data = data.new,
              aes(x=factor(Strain),
                  y=log.progeny),
              size = 0.7, alpha = 0.3, color = 'green4', fill = 'green4') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = 'black'),
        text = element_text(size=20),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black')) +
  scale_y_continuous(limits=c(-0.5,6)) +
  scale_x_discrete(labels = c('K101','K106',
                              'B506','B210',
                              'B201','B505',
                              'B211','B203',
                              'B204')) +
  labs(x="Strain", y=expression("Cells per Propagule (Log"[2]*")")) +
  annotate('text', x=1, y=-0.5, label="N = 668") +
  annotate('text', x=2, y=-0.5, label="162") +
  annotate('text', x=3, y=-0.5, label="162") +
  annotate('text', x=4, y=-0.5, label="158") +
  annotate('text', x=5, y=-0.5, label="119") +
  annotate('text', x=6, y=-0.5, label="704") +
  annotate('text', x=7, y=-0.5, label="51") +
  annotate('text', x=8, y=-0.5, label="115") +
  annotate('text', x=9, y=-0.5, label="103")




# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt)

#no

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)

#a Kruskal-Wallis test doesn't assume a normal distribution like an anova does, making it a better fit for non-normal variables.

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))

?fitdist

one.col <- data$Num.Cells.Progeny
fit.pois <- fitdist(c(na.exclude(one.col)), distr = "pois")
fit.logis <- fitdist(c(na.exclude(one.col)), distr = "logis")
fit.nbinom <- fitdist(c(na.exclude(one.col)), distr = "nbinom")


gofstat(list(fit.pois, fit.nbinom, fit.logis),chisqbreaks=c(1,2,4,8,16,32,64))


sec.col <- data$RepTime.sec
fit.pois2 <- fitdist(c(na.exclude(sec.col)), distr = "pois")
fit.logis2 <- fitdist(c(na.exclude(sec.col)), distr = "logis")
fit.nbinom2 <- fitdist(c(na.exclude(sec.col)), distr = "nbinom")


gofstat(list(fit.pois2, fit.nbinom2, fit.logis2))

# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?

#negative binomial because it has the lowest AIC score.

  # (2) - The replication time (data$RepTime.sec)?

#negative binomial, also has the lowest AIC score.

# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)

hist(data$RepTime.sec)


# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.
#The tallest bars most likely represent functions that are essential to life, things relating to fitness like gamete production (or fission rate in the case of microbes),
#or growth/maturation of cells. 

#The bars represent replication time rather than gene expression. If this was expression, your answer would be spot on!





