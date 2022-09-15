# With the data frame you created last week you will:

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

# Create a scatter plot between two of your numeric columns.
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.

a<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
b<-c(1,1,1,4,5,6,7,8,9,10,11,12,13,14,15)
c<-c(1.9,4.3,5,6.8,7.2,8.8,9.93,9.97,10.1,10.3,11.3,12,13,14,15)
d<-c("rug","plant","lamp","rain","blanket","shoe","pillow","laptop","wall","microwave","door","window","stairs","stove","television")
e<-c("pink","pink","pink","pink","green","green","green","green","violet","violet","violet","violet","violet","green","green")

df<- as.data.frame(cbind(a,b,c,d,e))
df

df$a <- as.numeric(as.character(df$a))
df$b <- as.numeric(as.character(df$b))
df$c <- as.numeric(as.character(df$c))

row.names(df) <- df$d
df

df1 <- df[,-4]
df1


#barplot####

df.mean2 <- aggregate(df1$b ~df1$e, FUN = "mean")
df.mean2
colnames(df.mean2) <- c("Factor","Mean")
df.mean2
barplot(df.mean2$Mean)
barplot(df.mean2$Mean, names.arg = df.mean2$Factor)

df.sd <- aggregate(df1$b ~df1$e, FUN = "sd")

colnames(df.sd) <- c("Factor","StanDev")
df.sd

b.plot <- barplot(df.mean2$Mean, names.arg = df.mean2$Factor)

arrows(b.plot, df.mean2$Mean-df.sd$StanDev,
       b.plot, df.mean2$Mean+df.sd$StanDev,angle=90,code=3)

b.plot <- barplot(df.mean2$Mean, names.arg = df.mean2$Factor, ylim = c(0,15))

arrows(b.plot, df.mean2$Mean-df.sd$StanDev,
       b.plot, df.mean2$Mean+df.sd$StanDev,angle=90,code=3)

title(main = "My Fav Bar Graph")
title(xlab = "color", ylab= "How much I Like")

pdf( file = "MyFavoriteBar.pdf", width = 4, height = 7)#So close - needed dev.off() here to make it work. Exported with the wrong dimensions.
par(family = "serif")
b.plot <- barplot(df.mean2$Mean, names.arg = df.mean2$Factor, ylim = c(0,15))

arrows(b.plot, df.mean2$Mean-df.sd$StanDev,
       b.plot, df.mean2$Mean+df.sd$StanDev,angle=90,code=3)

title(main = "My Fav Bar Graph")
title(xlab = "color", ylab= "How much I Like")

getwd()
dev.off()

plot(df1$b ~ df1$e, xlab = "color", ylab = "How much I Like", main = "My Fav Bar Graph")


#scatterplot####

plot(df1$b ~ df1$c)
plot(df1$b ~ df1$c, xlab = "Explanatory", ylab = "Response")
plot(df1$b ~ df1$c, xlab = "Explanatory", ylab = "Response", main = "My Favorite Scatter Plot")

?pch

colors()

plot(df1$b ~ df1$c, xlab = "Explanation", ylab = "Responses", main = "My Scatter Plot", 
     cex.axis=0.8, cex.main = 1.1, cex.lab = 1.25, pch= 18, col = "orchid3") #Supposed to change the x and y labels...otherwise spot on.



