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
  setwd("/Users/lea/Desktop/R") # Sets your working directory
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
 
# Testing for normality of distribution of the error terms
  shapiro.test(resid(sepal_lm)
  # The p-value = 0.1012
  
# Making a histogram to visualize the distribution of the sepal width data
  dev.off() # This stops the par() function effect previously set
  
  hist(iris$Sepal.Width, # Using the sepal width of the iris dataset
       xlab = "Setal Width", # Adds a x axis label
       main = "Density-based histogram of sepal width", # Adds a title
       freq = FALSE) # The histogram will plot the probability densities rather than the frequencies
  
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
       freq = FALSE) # The histogram will plot the probability densities rather than the frequencies
  
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

################################
#### Addition to question 4 ####
################################
  
# Definitions
  iris.labels=(paste("Iris", iris$Species))     # create a new vector that adds 'Iris' to the species names
  cluster_colors <- c("red", "green", "blue")   # create a vector with colors for the clusters 
  # (3 colors for 3 clusters / 3 species)
  
  
## Cluster Analysis ##

# calculate euclidian distance on balanced data
  iris.dist=dist(scale(iris[,-5]))
  
# complete linkage method
  iris.hc=hclust(iris.dist)
  plot(iris.hc, labels=iris.labels,                   # plot dendrogram with the predefined labels and define a labelsize
       xlab="Cluster Index",                          # set custom x label
       ylab="Height", cex=0.4,                        # set custom y label and set size of cluster inndices
       main="Cluster Dendrogram (Complete Linkage)",  # set title
       sub="")                                        # remove subtitle
  rect.hclust(iris.hc, k=3, border=cluster_colors)    # plot rectangles for the 3 main clusters using the pre-defined colors
  dev.copy2pdf(file="Cluster_hc.pdf")                 # save graph as pdf
  iris.hc2 <- cutree(iris.hc, k=3)                    #'cut' the dendrogram and save the 3 different clusters
  
# average method
  iris.ha=hclust(iris.dist, method="average")
  plot(iris.ha, labels=iris.labels,                   # see above
       xlab="Cluster Index",            
       ylab="Height", cex=0.4,               
       main="Cluster Dendrogram (Average)",           
       sub="")                                        
  rect.hclust(iris.ha, k=3, border=cluster_colors)
  dev.copy2pdf(file="Cluster_ha.pdf")
  iris.ha2 <- cutree(iris.ha, k=3)
  
# Ward's D method
  iris.hw=hclust(iris.dist, method="ward.D")
  plot(iris.hw, labels=iris.labels,                   # see above
       xlab="Cluster Index",
       ylab="Height", cex=0.4,             
       main="Cluster Dendrogram (Ward's D)",       
       sub="")                                    
  rect.hclust(iris.hw, k=3, border=cluster_colors)
  dev.copy2pdf(file="Cluster_hw.pdf")
  iris.hw2 <- cutree(iris.hw, k=3)
  
# create tables to compare the clusters to the original species (saved as screenshot)
  table(iris.hc2, iris$Species)     # Complete linkage vs. actual species
  table(iris.ha2, iris$Species)     # Average linkage vs. actual species
  table(iris.hw2, iris$Species)     # Ward's D vs. actual species
  
  
## PCA analysis ##
  pca1 <- dudi.pca(iris[,-5],          # [,-5] to exclude species
                   center=T, scale=T,  # data balancing to render all variables numerically homogeneous and comparable
                   scannf=F, nf = 2)   # automatically select and retain only the first two PCs
  
# create a new data frame to save values for plotting
  pca_df <- data.frame(PC1 = pca1$li[,1],                    # first column are PC1 values
                       PC2 = pca1$li[,2],                    # second column are PC2 values
                       Species = paste("Iris",iris$Species)) # keep species column for plotting
  
# create a function to generate the recolored PCA plots
  create_pca_plot <- function(pca_df, cluster_labels, plot_title) {   # define a function and the input parameters
    pca_df$Cluster <- factor(cluster_labels)                          # add cluster labels to the data frame
    ggplot(pca_df, aes(x=PC1, y=PC2, color=Cluster, shape=Species)) + # use ggplot to plot the PCA results, differentiate...
      # ...species by symbol shape and species by color
      geom_point(size=1.5) +                                          # set symbol size
      labs(title=plot_title, x="PC1", y="PC2") +                      # set title and axis labels
      scale_color_manual(values=cluster_colors) +                     # use the defined colors
      theme_minimal()                                                 # choose a pre-set-theme for visualization
  }
  
  
## create the PCA plots for the PCA result and the three clustering methods
  # use pca_df as input for the PCA plots and the above generated cluster data as labels to differentiate the clusters...
  # ...by different colors - for the original PCA plot use the species column; and set the title of each subplot
  original_pca_plot <- create_pca_plot(pca_df, pca_df$Species, "PCA (without cluster analysis)")
  hc_pca_plot <- create_pca_plot(pca_df, iris.hc2, "PCA (Complete Linkage Clustering)")
  ha_pca_plot <- create_pca_plot(pca_df, iris.ha2, "PCA (Average Linkage Clustering)")
  hw_pca_plot <- create_pca_plot(pca_df, iris.hw2, "PCA (Ward's D Clustering)")
  
# use gridExtra to plot the original PCA and the 3 recolored ones in subplots
  grid.arrange(original_pca_plot, hc_pca_plot, ha_pca_plot, hw_pca_plot, ncol=2)
  dev.copy2pdf(file="PCA_3clusters.pdf") # save graph as pdf
