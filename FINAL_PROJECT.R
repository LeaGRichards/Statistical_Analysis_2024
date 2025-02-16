################################
########## IRIS 1 ##############
################################
  
# Project by: 
  # Jonah Mende
  # Lea Giguère-Richards
  # Claudia Sturba
  # Nazareno Gimenez Zapiola

# Setting the working directory 
  getwd() # Informs us where the working directory currently is
  setwd("/Users/lea/Desktop/R") # Sets your working 
    # Place working directory location in quotation marks
  
# Installing and calling the necessary packages and libraries for the project
  # install.packages("ggplot2")
  library(ggplot2)
  
  # install.packages("gridExtra")
  library(gridExtra)
  
  # install.packages("ade4")
  library(ade4)

# Calling the iris dataset (built into R)
  iris <- iris
  
################################
########## Question 1 ##########
################################

# Creating a set of colors for the scatterplots
  spp_col <- c("red", "blue", "green")

# Making a multiframe of the scatterplots
  par(mfrow = c(1,2)) # Creating a multiframe of 1 row and 2 columns
 
 # Plotting petal width as a function of petal length
  plot(iris$Petal.Length, iris$Petal.Width, # Petal length as x values, petal width as y values
      ylab = "Petal Width", # Adds a y axis label
      xlab = "Petal Length", # Adds a x axis label
      main = "Petal length and width of 3 different species", # Adds a title
      pch = 19, # Type of points (big filled circle)
      col = spp_col[iris$Species]) # The colors of the points are the colors of spp_col 
                                   # according to the levels of the Species column of iris
 
 # Adding a legend to associate the colors with the 3 species
  legend("topleft", # Positions the legend in the top left corner of the scatterplot
        legend = levels(iris$Species), # The levels of the Species column of iris as levels of the legend
        col = spp_col, # Colors of the col object
        pch = 19, # Type of points (big filled circle)
        title = "Species", # Title of the legend
        cex = 0.8,  # Reduces text size
        bty = "n")  # Removes the legend box
 
 # Plotting sepal width as a function of sepal length
  plot(iris$Sepal.Length, iris$Sepal.Width, # Sepal length as x values, sepal width as y values
      ylab = "Sepal Width", # Adds a y axis label
      xlab = "Sepal Length", # Adds a x axis label
      main = "Sepal length and width of 3 different species", # Adds a title
      pch = 19, # Type of points (big filled circle)
      col = spp_col[iris$Species]) # The colors of the points are the colors of spp_col 
                                   # according to the levels of the Species column of iris

 # Adding a legend to associate the colors with the 3 species
  legend("topright", # Positions the legend in the top right corner of the scatterplot
        legend = levels(iris$Species), # The levels of the Species column of iris as levels of the legend
        col = spp_col, # Colors of the col object
        pch = 19, # Type of points (big filled circle)
        title = "Species", # Title of the legend
        cex = 0.8,  # Reduces text size
        bty = "n")  # Removes the legend box
 
 # Saving multiframe as pdf file in the working directory
  dev.copy2pdf(file = "question1.pdf")
 
 # Using ggplot2 as an alternative to plot the scatterplots
  graph1 <- ggplot(iris, # Using the iris dataset
   aes(x = Petal.Length, # Petal length as x values
       y = Petal.Width, # Petal width as y values
       color = Species)) + # Colors are set according to the Species column of the iris dataset
   geom_point(size = 2) + # Sets the size of the points
   labs(title = "Petal length and width of 3 different species", # Adds a title
        x = "Petal Length", # Adds a x axis label
        y = "Petal Width") + # Adds a y axis label
   theme_minimal() + + # Makes the graph look simpler (minimal)
   theme(legend.position = "bottom") # Places the legend at the bottom
 
 graph2 <- ggplot(iris, # Using the iris dataset
   aes(x = Sepal.Length, # Sepal length as x values
       y = Sepal.Width, # Sepal width as y values
       color = Species)) + # Colors are set according to the Species column of the iris dataset
   geom_point(size = 2) +  # Sets the size of the points
   labs(title = "Sepal length and width of 3 different species", # Adds a title
        x = "Sepal Length", # Adds a x axis label
        y = "Sepal Width") + # Adds a y axis label
   theme_minimal() + + # Makes the graph look simpler (minimal)
   theme(legend.position = "bottom") # Places the legend at the bottom
 
 # Placing these plots into a 2 column grid
 grid.arrange(graph1, graph2, ncol = 2)
 
 # Saving the grid as pdf file in the working directory
 dev.copy2pdf(file = "question1a.pdf")

 ################################
 ########## Question 2 ##########
 ################################
 
# Making a linear regression of the petal width as a function of length
 petal_lm <- lm(iris$Petal.Width ~ iris$Petal.Length)
 
# Making a linear regression of the sepal width as a function of length
 sepal_lm <- lm(iris$Sepal.Width ~ iris$Sepal.Length)
 
# Plotting the scatterplots
 dev.off() # This stops the par() function effect previously set
 par(mfrow = c(1,2)) # Creating a multiframe of 1 row and 2 columns
 
 # Plotting petal width as a function of petal length
 plot(iris$Petal.Length, iris$Petal.Width, # Petal length as x values, petal width as y values
      ylab = "Petal Width", # Adds a y axis label
      xlab = "Petal Length", # Adds a x axis label
      main = "Petal length and width of 3 different species", # Adds a title
      pch = 19) # Type of points (big filled circle)
 
 # Adding the linear regression to scatterplot
 abline(petal_lm) 
 
 # Plotting sepal width as a function of sepal length
 plot(iris$Sepal.Length, iris$Sepal.Width, # Sepal length as x values, sepal width as y values
      ylab = "Sepal Width", # Adds a y axis label
      xlab = "Sepal Length", # Adds a x axis label
      main = "Sepal length and width of 3 different species", # Adds a title
      pch = 19) # Type of points (big filled circle)
 
 # Adding the linear regression to scatterplot
 abline(sepal_lm)
 
 # Saving the multiframe as pdf file in the working directory
 dev.copy2pdf(file = "question2.pdf")

 ################################
 ########## Question 3 ##########
 ################################
 
# Testing for normality of distribution of the sepal width data
  shapiro.test(iris$Sepal.Width)
  # The p-value = 0.1012
  
# Making a histogram to visualize the distribution of the sepal width data
  dev.off() # This stops the par() function effect previously set
  
  hist(iris$Sepal.Width, # Using the sepal width of the iris dataset
       xlab = "Setal Width", # Adds a x axis label
       main = "Density-based histogram of sepal width", # Adds a title
       freq = FALSE) # The histogram will plot the probability densities rather then the frequencies
  
  # Adding the associated density line to the histogram
  lines(density(iris$Sepal.Width))

# Saving the plot as pdf file in the working directory
  dev.copy2pdf(file = "question3a.pdf")

# Testing for normality of distribution of the petal width data
  shapiro.test(iris$Petal.Width)
  # The p-value = 1.68 x 10^(-8)
  
# Making a histogram to visualize the distribution of the petal width data
  hist(iris$Petal.Width, # Using the sepal width of the iris dataset
       xlab = "Petal Width", # Adds a x axis label
       main = "Density-based histogram of sepal width", # Adds a title
       freq = FALSE) # The histogram will plot the probability densities rather then the frequencies
  
  # Adding the associated density line to the histogram
  lines(density(iris$Petal.Width))
  
# Saving the plot as pdf file in the working directory
  dev.copy2pdf(file = "question3b.pdf")
  
################################
########## Question 4 ##########
################################
  
# Scaling the iris dataset
  iris_sc <- scale(iris[,-5]) # This only considers the numeric variables of the iris dataset 
                              # It excludes the factorial Species column
  
# Computing the distance matrix
  iris_dist <- dist(iris_sc)
  
# Performing the hierarchical clustering with the “complete” method
  iris_hc <- hclust(iris_dist) # the "complete" method is the default method used
  
# Visualizing the clustering
  plot(iris_hc, # It uses the clusters made from the iris dataset
       labels = iris$Species, # Each cluster is associated to the species
       cex = 0.4, # Makes the labels slightly smaller
       xlab = "Cluster Index", # Defines teh x label
       sub = "") # Removes the subtitle
  
# Adding boxes around the 3 clusters
  rect.hclust(iris_hc, # Using the clustering just performed
              k = 3, # 3 clusters
              border = spp_col) # Attributes colors to the 3 clusters

# Saving the plot as pdf file in the working directory
  dev.copy2pdf(file = "question4a.pdf")  

# Creating a vector for the 3 clusters
  iris_hc2 <- cutree(iris_hc, k = 3)

# Classing the vector as a factor (of 3 levels)
  iris_hc3 <- as.factor(iris_hc2)
  
# Creating a multiframe with a scatterplot of question 1 and the equivalent but 
  # with the clusters replacing the species to define the colors of the points
  
  par(mfrow = c(1,2)) # Creating a multiframe of 1 row and 2 columns
  
  # Plotting petal width as a function of petal length
    # The colors are associated with the species
  plot(iris$Petal.Length, iris$Petal.Width, # Petal length as x values, petal width as y values
       ylab = "Petal Width", # Adds a y axis label
       xlab = "Petal Length", # Adds a x axis label
       main = "Petal length and width of 3 different species", # Adds a title
       pch = 19, # Type of points (big filled circle)
       col = spp_col[iris$Species]) # The colors of the points are the colors of spp_col 
                                    # according to the levels of the Species column of iris
  
  # Adding a legend to associate the colors with the 3 species
  legend("topleft", # Positions the legend in the top left corner of the scatterplot
         legend = levels(iris$Species), # The levels of the Species column of iris as levels of the legend
         col = spp_col, # Colors of the col object
         pch = 19, # Type of points (big filled circle)
         title = "Species", # Title of the legend
         cex = 0.8,  # Reduces text size
         bty = "n")  # Removes the legend box
  
  # Plotting petal width as a function of petal length
    # The colors are associated with the clusters
  plot(iris$Petal.Length, iris$Petal.Width, # Petal length as x values, petal width as y values
       ylab = "Petal Width", # Adds a y axis label
       xlab = "Petal Length", # Adds a x axis label
       main = "Petal length and width of 3 different species", # Adds a title
       pch = 19, # Type of points (big filled circle)
       col = spp_col[iris_hc2]) # The colors of the points are the colors of spp_col 
                                # according to the levels of the clustering
  
  # Adding a legend to associate the colors with the 3 species
  legend("topleft", # Positions the legend in the top left corner of the scatterplot
         legend = levels(iris_hc3), # The levels of the clusters 
         col = spp_col, # Colors of the col object
         pch = 19, # Type of points (big filled circle)
         title = "Clusters", # Title of the legend
         cex = 0.8,  # Reduces text size
         bty = "n")  # Removes the legend box
  
  # Saving the multiframe as pdf file in the working directory
  dev.copy2pdf(file = "question4b.pdf")
