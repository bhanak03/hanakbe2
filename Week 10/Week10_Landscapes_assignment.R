# Load the packages from this week's tutorial.
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).

library(spdep)
library(adespatial)
library(vegan)

setwd("C:/GitHub/hanakbe2/Week 10")
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
clingers.csv <- read.csv("Clingers.csv", header=T)
Trichoptera.csv <- read.csv("Trichoptera.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
Swimmers.csv <- read.csv("Swimmers.csv", header=T)

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Clingers.mat <- as.matrix(clingers.csv)
Trich.mat <- as.matrix(Trichoptera.csv)
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
Swimmers.mat <- as.matrix(Swimmers.csv)


nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)

bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]

aem.df

Space.rda.cling <- rda(Clingers.mat, as.data.frame(aem.df))
Space.r2a.cling <- RsquareAdj(Space.rda.cling)$adj.r.squared

aem.fwd <- forward.sel(Clingers.mat,aem.df, adjR2thresh=Space.r2a.cling)

aem.fwd$order

SpaceNoHab.rda.clings <- rda(Clingers.mat, as.data.frame(aem.df[,aem.fwd$order]), Clingers.mat)#Typo - second "clingers.mat" should be the habitat df.
SpaceNoHab.rda.clings
anova(SpaceNoHab.rda.clings, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda.clings)

HabNoSpace.rda.clings <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda.clings
anova(HabNoSpace.rda.clings, perm.max = 10000)
RsquareAdj(HabNoSpace.rda.clings)


Space.rda.Trich <- rda(Trich.mat, as.data.frame(aem.df))
Space.r2a.Trich <- RsquareAdj(Space.rda.Trich)$adj.r.squared


aem.fwd <- forward.sel(Trich.mat,aem.df, adjR2thresh=Space.r2a.Trich)

aem.fwd$order

SpaceNoHab.rda.Trich <- rda(Trich.mat, as.data.frame(aem.df[,aem.fwd$order]), Trich.mat)#Same issue here.
SpaceNoHab.rda.Trich
anova(SpaceNoHab.rda.Trich, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda.Trich)

HabNoSpace.rda.Trich <- rda(Trich.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda.Trich
anova(HabNoSpace.rda.Trich, perm.max = 10000)
RsquareAdj(HabNoSpace.rda.Trich)



#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#For Caddisflies, they are explained mostly by habitat and not space. This makes sense because they range in mobility as larvae, although the adults can fly. The space
#doesn't matter because of this range, but this range of motions can still exist in similar/the same habitats.
#For Clingers, habitat also matters more than space. This is because they are clinging to rocks, unmoving mostly, so space is not influenced unless they were to be stacked
#on top of each other. Habitat matters because the environment in which they live needs to be suitable for their species.
#Together, the habitat only explanation makes sense due to the diverse nature of caddisfly movement (which would be the thing to specify space) but their 
#relative dependability to be in freshwater sediment.
#you've the space and habitat influence backward with the correct interpretation. Space explains a lot, which means they can't move a lot and are stuck with the local variable habitat (which is why habitat is lower than space)


#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected




Trich.rda <- rda(Trich.mat, as.data.frame(HabitatbyPatch.csv))
Trich.r2a <- RsquareAdj(Trich.rda)$adj.r.squared
aem.fwd.t <- forward.sel(Trich.mat,HabitatbyPatch.csv, adjR2thresh=Trich.r2a)
aem.fwd.t

Clingers.rda <- rda(Clingers.mat, as.data.frame(HabitatbyPatch.csv))
Clingers.r2a <- RsquareAdj(Clingers.rda)$adj.r.squared
aem.fwd.c <- forward.sel(Clingers.mat,HabitatbyPatch.csv, adjR2thresh=Clingers.r2a)
aem.fwd.c

#In the clingers data set, depth and chlorophyll concentration are significant. In Triphs, nothing is
#significant.

#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)

#I think that if you looked at the spatial and habitat data sets together you would see similar results to the separate RDAs, specifically
#because space had such low influence on both Clingers and Triph. In addition, I specified above why space was less important than habitat for 
#both groups: clingers don't move and caddisflies have a variety of movement types and the adults are flying around the real world where space 
#isn't super important. If I had to estimate which set would have more reuslts from incorporating space, I would however chose Caddisflies due to
#their varied movement. 

#This is all true, but it's a little tangential to the question. Without selecting variables for habitat, you are likely overfitting that part of the model right now.
#So the "real" influence of space is likely much stronger than what you generated with the above models.