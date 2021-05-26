# # # # # # # # # # # # # # # # # # # # # # # # # # 
# Introduction to Principal Components Analysis in R
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# Part 1: Linear Data
# # # # # # # # # # # # # # # # # # # # # # # # # # 
# The data you will eventually work with will be based on 3-dimensional points, not linear measurements.
# However, the sample linear datasets are (1) smaller and (2) more intuitive to interpret, better for demonstrating concept.
# Once you understand the concepts and connections with linear data, 3D geometric morphometric data will be easy.
# Start by loading in a dataset
data(iris) # pre-made data package of flower part measurements for three species

# # # TASK:
# Explore the structure of the data. Note the use of dollar signs in front of variables
str(iris)
iris$Petal.Length

# # # QUESTIONs: 
# How many plants were measured in the iris dataset? 
# How many measurements were taken per plant? 
# How dimensions is each measurement?
# How many dimensions, total, is the "variation space" of this iris dataset?

# # # TASK:
# Make a vector of the colors that you want to use when plotting the species (if 3 species, use 3 colors).
# Plot the data in any way you think appropriate to answer the questions below. Use color.
# an example plot:
plot(x=iris$Petal.Length,y=iris$Sepal.Length) #is a good start, but hard to know what is what. Add color
plot(x=iris$Petal.Length,y=iris$Sepal.Length,col="red") # not quite. Still doesn't differentiate groups.
plot(x=iris$Petal.Length,y=iris$Sepal.Length,col=c("red","blue","green")[iris$Species]) #one way

flower.colors<-c("red","blue","green") #another way
plot(x=iris$Petal.Length,y=iris$Sepal.Length,col=flower.colors[iris$Species]) #make sure you understand what`flower.colors[iris$Species]` does
legend('topleft',legend=levels(iris$Species),col=flower.colors,pch=1) #a legend gives the colors meaning. 

# another example
boxplot(iris$Petal.Length~iris$Species,col=flower.colors)

# Now, try making other plots so you can answer the following questions.

# # # QUESTIONS:
# Which species do you think is most different from the others? Why?
# Which trait(s) is(are) most important for telling the species apart? Why do you think so?

# # # EXAMPLE:
# perform a principal components analysis with the dataset

# First, log transform (ln, not log-10) the data. Generally a good idea with linear biological measurements
# Note: Gingerich 2000 explores why biological data are log-transformed. Others do it for parametric convenience
# call the object you make `iris_ln`. Use Google to figure out the correct function. 
# first, log transform (ln, not log-10) the data. Generally a good idea with linear biological measurements
# Note: Gingerich 2000 explores why biological data are log-transformed. There are many other papers on the topic.

# perform the principal components analysis. In this case, we want to scale and center the data.
iris_pca <- prcomp(iris_ln, center = TRUE, scale. = TRUE) 

# look at how much variation is explained by each newly made principal component
summary(iris_pca) 

# PC1 + PC2 alone explain more than 95% of all the variation in the entire dataset. 
# Examine the summary until you understand how the above conclusion was reached. 

# look at the same information visually
plot(iris_pca, type = "l") # makes a scree plot. 
# Note that variation always decreases as you move down PCs (x-axis)

# NOTES:
# in this case, where PC1 and PC2 explain a vast majority of the data, we can move ahead using only those two axes and ignore PC3 and PC4
# that takes us from a four-dimensional system to a two-dimensional system with very little loss of information
# plus, two-dimensional systems are much easier to visualize. 
plot(iris_pca$x[,1:2]) #this is a simple principal component plot.

# # # TASK:
# plot the same information as above, but color the points by species.
# put a legend on the plot showing which species equals which color

# # # EXAMPLE:
# show the loadings on a PC plot. 
# That is, a visualization of how the original measurements relate to the PC scores
iris_pca$rotation # loading information is kept here
#assume we will plot PCs 1 & 2. They are the most important, and therefore most interesting.
load_x<-iris_pca$rotation[,1]
load_y<-iris_pca$rotation[,2]
variables<-row.names(iris_pca$rotation)

plot(iris_pca$x[,1:2],col="orange")
lines(c(0,load_x[1]),c(0,load_y[1]),col="black") #this is nice, but a bit short. Let's make it longer
lines(c(0,load_x[1]*2),c(0,load_y[1]*2),col="black") #better
text(load_x[1],load_y[1],labels=variables[1],col="black",pos=4) #change pos to different numbers to see what it does, unless you already know the answer

# INFERENCE: Sepal length plays a role in both PCs 1 and 2. 

# TASK: Because the loading vector points in a positive direction in the X axis,
# we should expect that specimens with larger (more positive) PC1 values should have longer sepals
# than specimens with smaller PC1 values. Find a way to check and make sure this is true.
# Hint: If the species form clusters in the PC plot along the x axis, then one species should have shorter sepals in general than at least one other species. Figure out which species and check with a box plot.

# # # CHALLENGE:
# Plot the loadings of all four variables, not just the first one.
# Write a summary of what the PC plot and loadings tell you about variation in the dataset:
#   Which species are the most different? Which most similar?
#   Which features are most associated with PC1? PC2?
