# # # # # # # # # # # # # # # # # # # # # # # # # # 
# Introduction to Principal Components Analysis in R
# # # # # # # # # # # # # # # # # # # # # # # # # # 
# Part 2: Understanding Some Key Terms with More Linear Data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This part is mean to be challenging. Before asking for help, ask the internet.
# Google-fu is an essential skill in learning to code.
# Asking Google or Stack Overflow a query like "r variance-covariance matrix" is good practice.

# Start by loading in a dataset
data(iris) # pre-made data package of flower part measurements for three species

# # # INTRODUCTION (or review): dplyr
# dplyr is a helpful package of functions for doing lots of common tasks with a table.
# In this case, I want you to work with a smaller, lighter version of the iris dataset.
library(dplyr)

# dplyr Feature 1: straightforward matrix functions:
(species_groups<-group_by(iris, Species)) # groups the iris table by species. Note that you don't need the dollar sign format ("iris$Species"), unlike other R functions
(species_means1<-summarize(species_groups,sepal_length=mean(Sepal.Length),
                          sepal_width=mean(Sepal.Width), 
                          petal_length=mean(Petal.Length),
                          petal_width=mean(Petal.Width))) # Note that parentheses print results to screen.
# dplyr Feature 2: link functions together with "%>%". The line of code below does the exact  same thing as above, without needing intermediate objects like "species_groups"
(species_means<-group_by(iris, Species) %>% summarize(sepal_length=mean(Sepal.Length),sepal_width=mean(Sepal.Width), petal_length=mean(Petal.Length),petal_width=mean(Petal.Width)))

# species_means is our new dataset: simplified 150 individual plant observations to the mean observations for 3 species.

# # # OVERALL TASK: 
# show the mathematical relationship between eigenvalues, eigenvectors, and principal components.

# # # KEY INFORMATION:
# eigenvectors and eigenvalues are sort of simple linear operations, just expanded to matrices, which makes everything more complicated.
# PCs are linear combinations of original variables -- essentially forming a new coordinate system for the same data, organized by variation
# eigenvectors provide the tranformation (coefficients) you need to get your original coordinates to PC coordinates
# variance of principal components (PCs) equal eigenvalues
# eigenvalues tell you how much variation is explained by a given PC
# sums of variances equals sum of eigenvalues
# if you multiply the matrix of original data by the matrix of eigenvectors, then you'll get matrix of PC scores
# further introduction to these terms is here: http://www.indiana.edu/~g562/PBDB2013/Day%203B%20-%20PCA%20and%20morphospace.pdf

# here are how to get each of those objects using R's prcomp() function:
(example<-as.data.frame(species_means) %>% .[,2:5]) %>% as.matrix(.) # format the table properly
row.names(example)<-species_means$Species #put the species labels in as row names
PC_analysis<-prcomp(example,center=TRUE,scale.=FALSE)	#run PCA. We want to center these linear measurements (mean of a PC should be zero)
(eigenvectors<-PC_analysis$rotation)   #is a 3x3 matrix
(eigenvalues<-(PC_analysis$sdev)^2) #is a vector of 3 numbers
(PC_scores<-PC_analysis$x) #is a 3 x 3 matrix: you can't have more PCs than observations. 
# note: by not having fewer observations than measurements, you've already reduced the dimensionality of your data from 4D to 3D

# CHECK: We meant to center the data on zero. is the mean basically zero? If not, something is wrong. 
mean(PC_analysis$x[,1]) 

# # # TASK 1:
# Show that total variance doesn't change between your original data and PC data.
# alternatively, show that the sum of the eigenvalues equals the variance of the principal components, which equals the variance in the original measurements

# As an example of how to approach these tasks in general, Task 1 is broken into a series of smaller steps.
# Start by finding the variance in the original data. Do it two ways, and see that they are the same.

# QUESTION: What is variance?

# QUESTION: What R function can you use to find the variance of a vector?


# Use that function for variance to find the variance of each variable (ex., sepal_length) in the example dataset


# QUESTION: What R function can you use to sum up a vector?


# QUESTION: What R function concatenates, or puts together, a series of numbers or strings into a vector?


# Use the answers to the two questions above to find the sum of the variance of all variables.
# Your answer is the total variance in the original dataset. Make it an object.


# Find the same thing using a variance-covariance matrix.
# QUESTION: How do you make a variance-covariance matrix in R?


# Make a variance-covariance matrix from the example dataset


# There is a decent chance you'll use variance-covariance matrices later in your research.
# Here's what's helpful to know about a variance-covariance matrix for now:
# (a) Each cell in the matrix tells you how each variable is related to each other variable
# (b) For example, the number at the intersection between petal_length & petal_width is the covariance between the two variables
#     The (relatively) large, positive covariance means that the variables are positively and (relatively) highly correlated.
# (c) By extension, the diagonal values (ex., [1,1], [2,2], etc.) are the variance for each variable
# (d) At least a little bit of this information should sound vaguely familiar from introductory statistics

# Find the sum of the diagonals of the variance-covariance matrix. Make it an object. 


# QUESTION: Do these two ways of calculating the total original variance in your sample give you the same answer?
# (If the answer to this question is not yes, then something is wrong.)


# Believe it or not, if you've made it his far you have all the functions you need to finish this task.
# As a reminder, the task was to show:
# variance of original data = variance of PC scores = sum of eigenvalues?

# Get an easy one out of the way. Find the sum of the eigenvalues. Make it an object.


# Is the object you just made the same number as your objects containing the original variance?
# (If the answer to this question is not yes, then something is wrong.)


# Now, find the variance of principal component scores. Make it an object.
# You can use the same approach to find the total variance of PC scores as you used with total variance of original data.


# QUESTION: Does variance of original data = variance of PC scores = sum of eigenvalues?



# # # TASK 2:
# Understand how to interpret eigenvalues.
# Eigenvalues are already interpreted, in a way, in summary(PC_analysis). 
summary(PC_analysis)

# QUESTION: How do you calculate the percent of total variance ("Proportion of Variance") explained by each principal component?

# QUESTION: How would you turn that proportion of variance explained into a percent of variance explained? (not a trick question)

# # # TASK 3:
# Relate eigenvalues to PC_scores.
# QUESTION: What parts of the KEY INFORMATION might help you figure out which functions you need for this task?
# Answer: variance of principal components (PCs) equal eigenvalues; eigenvalues tell you how much variation is explained by a given PC

# Use the KEY INFORMATION or other information from the internet, be creative, 
# and show how  to get the first eigenvalue ("eigenvalue[1]") from the scores of PC1 ("PC_scores[,1]")


# # # TASK 4:
# Relate eigenvectors to PC_scores. Show how to move back and forth between them.
# HINT: Use the KEY INFORMATION to figure out what kinds of functions and arithmetic you will need.
# HINT: In the principal component function, we centered our original data. You should center it here. 
# HINT HINT: centering means transforming the data so that the mean of every column is zero. 
# HINT: If you want to try the matrix multiplication route, the operation in R is "%*%"






# CHEAT (or time-saver, your choice) for TASK 1: sum(var(PC_scores)) will get you the same answer as summing up the variance of all the columns in PC_scores, but with fewer keystrokes
