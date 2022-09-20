# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  
# approximately 9 hours and 15 minutes.

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)

# knowing how long it takes peak flow to reach specific towns from a set starting point gives citizens and officials a warning on how much time they'll have to evacuate/prepare for major flooding events. 

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.

install.packages('learnPopGen')
library(learnPopGen)

?drift.selection

drift.selection()
p<-drift.selection(p0=0.01,Ne=100,w=c(1,0.9,0.8),ngen=200,nrep=5)
plot(p)

colors()

drift.selection()
b<-drift.selection(p0=0.01,Ne=100,w=c(3,1.2,0.9),ngen=200,nrep=8)
plot(b)


          ## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.


install.packages('diverse')
library(diverse)

data(pantheon)
diversity(pantheon)
diversity(pantheon, type='variety')
diversity(geese, type='berger-parker', category_row=TRUE)
#reading csv data matrix
path_to_file <- system.file("extdata", "PantheonMatrix.csv", package = "diverse")
X <- read_data(path = path_to_file)
diversity(data=X, type="gini")
diversity(data=X, type="rao-stirling", method="cosine")
diversity(data=X, type="all", method="jaccard")

#              variety  entropy       HHI blau.index gini.simpson gini.simpson.C gini.simpson.R
#Canada            27 2.559899 0.1391628  0.8608372    0.8608372      0.1391628       7.185827
#Chile              7 1.626709 0.2396450  0.7603550    0.7603550      0.2396450       4.172840
#China             24 2.340469 0.1831446  0.8168554    0.8168554      0.1831446       5.460167
#Latvia            10 1.889159 0.2345679  0.7654321    0.7654321      0.2345679       4.263158
#New Zealand        9 2.106577 0.1326531  0.8673469    0.8673469      0.1326531       7.538462
#Portugal          11 1.610050 0.2899660  0.7100340    0.7100340      0.2899660       3.448680
#Saudi Arabia       7 1.331425 0.3453061  0.6546939    0.6546939      0.3453061       2.895981
#South Africa      16 2.440072 0.1211073  0.8788927    0.8788927      0.1211073       8.257143
#Uruguay            4 1.135551 0.3739612  0.6260388    0.6260388      0.3739612       2.674074
#Vietnam            4 1.168518 0.3719008  0.6280992    0.6280992      0.3719008       2.688889
#simpson.D simpson.I simpson.R hill.numbers berger.parker.D berger.parker.I
#Canada       0.13174182 0.8682582  7.590604           27       0.3162393        3.162162
#Chile        0.20923077 0.7907692  4.779412            7       0.3846154        2.600000
#China        0.17480932 0.8251907  5.720519           24       0.3838384        2.605263
#Latvia       0.18954248 0.8104575  5.275862           10       0.4444444        2.250000
#New Zealand  0.06593407 0.9340659 15.166667            9       0.2142857        4.666667
#Portugal     0.28141136 0.7185886  3.553517           11       0.4285714        2.333333
#Saudi Arabia 0.32605042 0.6739496  3.067010            7       0.4857143        2.058824
#South Africa 0.09447415 0.9055258 10.584906           16       0.2647059        3.777778
#Uruguay      0.33918129 0.6608187  2.948276            4       0.5263158        1.900000
#Vietnam      0.30909091 0.6909091  3.235294            4       0.5454545        1.833333
