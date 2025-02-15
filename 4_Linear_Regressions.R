### Linear regression ###

## Linear regression ##
  # Importing the data
    thuesen <- read.table("thuesen.txt", header=T)

  # Creating a linear regression
    mod <- lm(short.velocity ~ blood.glucose, data = thuesen)

  # Extracting information in the regression
    summary(mod)
    # simple statistics on the value of the error terms
    # estimates of the coefficients of the linear regression with
      # respective standard errors and t-tests 
      # Note : H0 for each of them is correlation coefficient = 0 
      # Note: t-test on the intercept value has no real meaning in the model
    # Residual standard error + degree of freedom
    # R-squared / adjusted-R-squared 
      # change in the square sum of error terms / variance with respect to a null model
    # F-test (variance test) = is a simple linear regression coinciding with a t-test (??)

  # Expected y values
    fitted(mod)

  # Error term distribution
    resid(mod) 

  # Plotting a scatterplot of the data 
      # + regression line -> abline(lm(data))
      # + error terms -> segments(x0, y0, x1, y1)
    plot(thuesen)
    abline(mod)
    segments(thuesen$blood.glucose,fitted(mod),thuesen$blood.glucose, thuesen$short.velocity)

  # Remove NA values (2 options)
    # Option 1 : options(na.action=na.exclude)
    # Option 2 (what I prefer) :
      thuesen <- na.omit(thuesen)
      plot(thuesen)
      abline(mod)
      segments(thuesen$blood.glucose,fitted(mod),thuesen$blood.glucose, thuesen$short.velocity)

  # Q-Q plot : Visualizing if the error terms are normally distributed
    qqnorm(resid(mod))
  
  # Shapiro-Wilk test. : testing for normal distribution of the error terms
    shapiro.test(resid(mod))
    
## Multiple linear regression ## 

  # Call the data (default dataset)
    data(trees)
    
  # Get some information on the dataset 
    str(trees)
    summary(trees)
  
  # Plot the dataset
    plot(trees) 
  
  # Make a multiple linear regression
    mod2 <- lm(Volume ~ Girth + Height, data = trees) 
    
  # Get some information on this multiple linear regression 
    summary(mod2)

## Exercise ##
  
  # Calling the data
    met <- read.table("rmr.txt", header = TRUE)
  
  # Plot the scatterplot of the data
    plot(met[[1]], met[[2]])  
  
  # Create a linear regression and extract the residuals
    met_lm <- lm(met[[2]] ~ met[[1]])
    met_ey <- fitted(met_lm)
  
  # Check for any NA values
    sum(is.na(met)) # None
  
  # Plot the regression on the scatterplot with the residuals
    abline(met_lm)
    segments(met[[1]], met[[2]], met[[1]], met_ey)

## Correlation coefficient ##
  
  # Pearson's correlation coefficient
    cor(thuesen$blood.glucose, thuesen$short.velocity, use = "complete.obs")
    # use = "complete.obs" -> takes out any rows with NA from the computation
    # Returns r (the Pearson correlation coefficient)
    cor(thuesen, use = "complete.obs") # results in a matrix
  
    # Test against H0 (r = 0)
      cor.test(thuesen$blood.glucose,thuesen$short.velocity)
      # p-value < 0.05 = we reject H0, the correlation is statistically significant
      # No need to specify to avoid rows containing NAs
    
  # Spearman Correlation Coefficient
    cor.test(thuesen$blood.glucose, thuesen$short.velocity, method = "spearman")
    #p-value > 0.05 = we accept H0, there is no correlation
    # rho is 
  
  # Exercise :
    sum(is.na(trees)) # no NA values
    cor(trees)
    cor.test(trees$Girth, trees$Height, method = "spearman")    
    # p-value < 0.05, we reject the null hypothesis

## ANOVA : analysis of variance
    
  # Call the data
    red.cell.folate <- read.table("red.cell.folate.txt", header=TRUE)
    
  # Get some information on the dataset
    str(red.cell.folate)
    
  # Class the categorical variable as a factor
    red.cell.folate$ventilation <- as.factor(red.cell.folate$ventilation)
    
  # Create a regression
    rcf_lm <- lm(folate ~ ventilation, data = red.cell.folate)
    rcf_lm
      # Intercept = mean folate measurement of the 1st groups alphabetically
      # 2 next values = differences between the means of the second and third 
        # group and the mean of the first group
    
  # ANOVA
    anova(rcf_lm)
      # Ventilation row = Variance between groups
      # Residual row = variance within groups
      # Degrees of freedom = 
        # Factorial: groups - 1
        # REsiduals: obs - groups - 1
      # Sum sq = 
        # Ventilation: sum of square differences between group mean and the total mean 
        # Residuals: sum of square differences between observation and mean of their group
      # Mean sq = variances (Sum sq / df)
      # F test for variance comparison + p-value
    
    summary(rcf_lm)
      # mean of the 1st group, difference of means between the 2nd or 3rd group and the 1st
      # t-test for significance of these differences
      
    # paired t-test
      pairwise.t.test(red.cell.folate$folate, red.cell.folate$ventilation, p.adj="bonf erroni")
      # Compares all possible means
      # Doing a test multiple time on the same data = risk of getting a significant result by chance
      # To compensate for this risk = Bonferroni method
        # multiply each p-value by the number of tests performed
      
    # Visualizing the distribution for each group
      boxplot(red.cell.folate$folate ~ red.cell.folate$ventilation, 
              xlab = "Ventilation", ylab = "Folate", col = heat.colors(3), las = 1)
      # las = 1 : sets how the axis labels are placed (1 is horizontal)
      
  # Exercise
    juul <- read.table("juul.txt", header = T)
    str(juul)    
      # There are many NA values and some variables have to be classed as factors
    juul$tanner <- as.factor(juul$tanner)
    
    juul_lm <- lm(juul$igf1 ~ juul$tanner)
    anova(juul_lm)    
    boxplot(juul$igf1 ~ juul$tanner, xlab = "Tanner", ylab = "igf1",col = heat.colors(5), las = 1)    

  # Kruskal-Wallis test
    kruskal.test(red.cell.folate$folate, red.cell.folate$ventilation)
    # p-value not significant
    
## 2-way ANOVA ##
  
  # Calling the data
    heart.rate <- read.table("heart.rate.txt", header = T)
    heart.rate[,2] <- as.factor(heart.rate[,2]) 
    heart.rate[,3] <- as.factor(heart.rate[,3])
  
  # Doing an ANOVA
    HR_lm <- lm(heart.rate[[1]] ~ heart.rate[[2]] + heart.rate[[3]])
    anova(HR_lm)    
  
  # Visualizing the data
    interaction.plot(ordered(heart.rate$time), heart.rate$subj, heart.rate$hr)
    # Inside ordered() -> the x axis variable
    # Then variables for groups of different lines (patients)
    # Then the dependent variables
    # Very cool    
    
## Friedman test ##
  friedman.test(hr ~ time | subj, data = heart.rate) # look at the formula!

## Exercise ##
  lung <- read.table("lung.txt", header = T)
  lung$method <- as.factor(lung$method) 
  lung$subject <- as.factor(lung$subject)
  str(lung)  # Great
  
  lung_lm <- lm(lung$volume ~ lung$method + lung$subject)
  anova(lung_lm)  
  
  interaction.plot(ordered(lung$method), lung$subject, lung$volume)  
