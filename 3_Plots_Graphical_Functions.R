### Plots and Graphical Functions ###

# Pie chart & Barplot
  # ONE Qualitative variable
  # Each number reprensents the frequency of each state of the variable
  
  # Pie chart
  titanclas <- c(325,285,706,885)
  names(titanclas)=c("1st","2nd","3rd","Crew")
  
  pie(titanclas, main="Passengers of Titanic", col=c("white", "yellow", "orange", "darkred"))
  palgrey <- grey(seq(0, 1, 0.25))  # Will give sprectrum based on vector
  pie(titanclas, main="Passengers of Titanic", col=palgrey)

  # More colours =
    # install.packages("RcolorBrewer")
    library(RColorBrewer)
    display.brewer.all()
    pal4 <- brewer.pal(4,"Blues")  
    pie(titanclas, main="Passengers of Titanic", col=pal4)
  
  # Barplot
    # Horizontal axis = classes of the qualitative variables
    # Vertical axis = frequencies of the qualitative variables
    barplot(titanclas, main="Passengers of Titanic",col=pal4)
    barplot(titanclas, main="Passengers of Titanic",col=pal4, hor = T) # For horizontal bars
    
# Mosaic plot
    # TWO qualitative variables
    
    # Get dataset
    library(MASS)
    data(caith) # 5 x 4 table
    
    mosaicplot(caith,main="Hair-eye color for 5387 people in Caithness, Scottland")
      # Dimensions of rectangles are proportional 
      # to frequency of the different combinations of classes of the qualitative variables   
    
    mosaicplot(caith,main="Hair-eye color for 5387 people in Caithness, Scottland", shade = T)
      # Colour is asigned on the basis of the value of the residuals (difference between observed values)

# Histogram & Boxplot
    # ONE quantitative variable
    
    data(crabs)
    head(crabs)    
    
    # Select the CL variable for the orange crabs
    caleor <- crabs$CL[crabs$sp=="O"]
   
    # Histogram
    hist(caleor, main="Histogram of carapace length in orange crabs", col="orange")  
    hist(caleor, main="Histogram of caleor", col="orange", freq=FALSE) 
      # freq= FALSE : y scale is the density frequency rather than absolute frequency
      # Frequency density : relative frequency of a class / class width 
          # Relative frequency of a class = absolute frequency of a class / total observations
          # ex: class [25, 30[
            # 20 observations out of 100 total observations = 0.2 ratio
            # 0.2 / 5 = 0.04 :)
          # Sum of all rectangles = 1
          # Frequency density = the probability of a random datapoint to fall in a class
        
    # Boxplot
    boxplot(caleor, main="Boxplot") # default
    boxplot(caleor,col=grey(0.8),main="Carapace Lengths",las=1) 
      # las changes orientation of axis numbers to horizontal
    rug(caleor,side=2)
      # representation of the data distribution on side (counting clockwise from the bottom side)
    boxplot(caleor,notch=T,col=grey(0.8),main="carapace lengths",las = 1)
      # Makes a notch at median witch the amplitude is proportional to variance of data
      # Good when comparing datasets, overlapping notch means similar data distribution
    
    # Exercise
    blue <- crabs$RW[crabs$sp=="B"]
    hist(blue, prob=TRUE, main="Blue crab's RW", col="forestgreen")
      lines(density(blue), col="orange")
    boxplot(blue, notch=T, main="Boxplot of Blue Crab's RW", col="pink", las=1)

## Scatterplot
    # Two quantitative variables
    
    cawior <- crabs$CW[crabs$sp=="O"]
    plot(caleor, cawior)
    
    par(mfrow=c(1,3))
    plot(caleor, cawior)
    plot(cawior~caleor,col="red",main="Length and Width of carapaces")    
    plot(cawior~caleor,pch=20)    
  
    plot(cawior~caleor,main="Length and Width of 100 crabs", pch=20, xlab="length (in mm)",ylab="width (in mm)")
      abline(h=mean(cawior), lty=2, col=grey(0.6)) 
      abline(v=mean(caleor), lty=2, col=grey(0.6))  
        # lty for the format of the line
      points(mean(caleor),mean(cawior),col="red",pch=20,cex=2)
    
    sexcol <- ifelse(crabs$sex=="F", "orange", "skyblue")  
    plot(cawior~caleor,main="Length and Width of 100 crabs", pch=20, col=sexcol, xlab="length (in mm)",ylab="width (in mm)")
      abline(h=mean(cawior), lty=2, col=grey(0.6)) 
      abline(v=mean(caleor), lty=2, col=grey(0.6))  
      points(mean(caleor),mean(cawior),col="red",pch=20,cex=2)
    
  # Exercise
    blue <- crabs$RW[crabs$sp=="B"]
    blueFL <- crabs$FL[crabs$sp=="B"]
    
    par(mfrow=c(1,3))
    plot(cawior~caleor, col="forestgreen", pch=17)
    plot(cawior~caleor, col="skyblue", pch=15,)
      abline(h=mean(cawior), col="orange")
      abline(v=mean(caleor), col="orange")
    plot(cawior~caleor, pch=3, col="red")
      points(mean(caleor), mean(cawior),pch=8, col="purple", cex=2 )

# Boxplot
  # ONE quantitative + ONE qualitative variable
  
  sex <- crabs$sex[crabs$sp=="O"]
  par(mfrow=c(1,2))  
  par(mar=c(3,2,2,2))
  boxplot(caleor~sex) # quantitative ~ qualitative
  boxplot(caleor~sex,col=c("purple","green"),notch = T)

  # Exercise :
  Mcol <- crabs$sp[crabs$sex=="M"]
  MRW <- crabs$RW[crabs$sex=="M"]
  par(mar=c(3,2,2,2))
  boxplot(MRW ~ Mcol)
