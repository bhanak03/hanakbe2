s# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: your last name_Week1_Assignment
  # e.g. mine would be Wilson_Week1_Assignemnt


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures:
  # One character vector with all unique values
  # One character vector with exactly 3 unique values
  # One numeric vector with all unique values
  # One numeric vector with some repeated values (number of your choosing)
  # One numeric vector with some decimal values (of your choosing)

1+1

a<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
b<-c(1,1,1,4,5,6,7,8,9,10,11,12,13,14,15)
c<-c(1.9,4.3,5,6.8,7.2,8.8,9.93,9.97,10.1,10.3,11.3,12,13,14,15)
d<-c("rug","plant","lamp","rain","blanket","shoe","pillow","laptop","wall","microwave","door","window","stairs","stove","television")
e<-c("red","orange","yellow","green","blue","violet","indigo","green","black","green","sage","periwinkle","beige","pink","lavender")
  
data<- cbind(a,b,c,d,e)
data

df <- as.data.frame(data)
df

colnames(df) <- c("throw","bug","on","da","floor")
df
df[1,]
df[,1]

row.names(df) <- df$da
df
df[,4]
df[,-4]

milkshake<-data.frame(16,19,34,"tissue","white")
milkshake

colnames(milkshake)<-colnames(df) 
df.r<-rbind(milkshake,df)

names(milkshake)<-colnames(df)
df
df.r

row.names(df.r)<-df.r$da

df.r
newdf<-df.r[,-4]

newdf


setwd("C:/GitHub/hanakbe2/Week1")

write.csv(newdf, file = "week1.csv")
read.df <- read.csv('week1.csv')
read.df

read.df <- read.csv('week1.csv', row.names = 1, header = TRUE)
read.df

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names# 
# Remove the character vector with unique values from the data frame.

# Add 1 row with unique numeric values to the data frame.

# Export the data frame as a .csv file 

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading.

# Push your script and your .csv file to GitHub in a new "Week1" folder.


