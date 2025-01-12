### Distributions ###

## Probabilities
# Building a binomial distribution curve
  # create a vector
  x=0:50
  # Create a new object with all associated probability values to values between 0 and 50
  y <- dbinom(x, size=50, prob=0.33)
  # Plot y as a function of y
  plot(x,y, type="h") # h for histogram plot rather than scatter plot

# Building a standard normal distribution of a continuous variable
  # Create a vector (mean = 0) with finite intervals
  x1 <- seq(-4, 4, 0.1)
  # Compute density probability values
  y1 <- dnorm(x1, mean=0, sd=1)
  # Plot it
  plot(x1, y1, type="l") # type l will approximate a linear continuity

## Cumulative Probability 
# Cumulative probability of normal distribution of continuous variables
  
  # Find cumulative probability of X>160 
    # (right side of the area under the curve, past 160)
  pnorm(160, mean=132, sd=13, lower.tail = FALSE) # lower.tail=FALSE P[X > x], TRUE (default) P[X < = x]
  1-pnorm(160,mean=132,sd=13) # SAme value, different approach
  
  # Plot the distribution
  x2 <- seq(80, 184, 0.5) # 4 standard deviations before and after mean
  y2 <- dnorm(x2, mean=132, sd=13)
  plot(x2, y2, type="l")
  segments(80,0,184,0)  # 2 sets of coordinates (x,y) to draw a line across the plot
  abline(h=0) # same thing as previous line, makes a horzontal line as desired
  
  # add a vertical line for the x values of 160 in red
  prob160 <- dnorm(160, mean=132, sd=13)
  segments(160,0,160,prob160,col="red")
  
  # Save graph as pdf
  dev.copy2pdf(file="example_normal.pdf")
  
# Cumulative probability of Binomial distribution
  # Make a distribution of a NULL hypothesis (H0) between adverse effects of treatments A and B --> p=0.5
    # put a vertical line for the value of 16
  x3 <- 1:20
  y3 <- dbinom(x3, size=20, prob=0.5)
  plot(x3, y3, type="h")
  prob16 <- dbinom(16, size=20, prob=0.5)
  segments(16,0,16,prob16, col="red")
    
  # Find cumulative probability of X<= 16
  pbinom(16, size=20, prob=0.5) # default of lower.tail is TRUE which is P[X >= x] (what we want)
  
  # Find cumulative probability of X >= 16 (includes value for 16) --> 3 ways
  pbinom(16, size=20, prob=0.5, lower.tail=FALSE) + prob16
  1-pbinom(15, size=20, prob=0.5)
  pbinom(15, size=20, prob=0.5, lower.tail=FALSE)
    # this value is the P-VALUE
    # Typically p-value under 5% (0.05) is rare = refutes null hypothesis
    # The smaller the p-value, the stronger the hypothesis H1
  
  # (Considering 16 out of 20 patients show adverse effects of A) the result allows us 
    # to REFUTE our null hypothesis that the two treatments 
    # have similar adverse effects, as it is highly improbable (p~0.59%) and 
    # we will accept the alternative hypothesis that treatment A providesless 
    # adverse effects and is to be preferred over treatment B.
      
## Theoretical Quantiles
  # Using a normally distributed set of continuous variables
    # = symetrical around mean/median
    # = confidence interval also symetrical (2.5% on either end)
    # = quantiles 2.5% and 97.5%
  lowlim <- qnorm(0.025, mean=132, sd=13, lower.tail=TRUE)
  upplim <- qnorm(0.975, mean=132, sd=13, lower.tail=TRUE)

  # Make a plot )
  lowlimP <- dnorm(lowlim, mean=132, sd=13)
  upplimP <- dnorm(upplim, mean=132, sd=13)
  P160 <- dnorm(160, mean=132, sd=13)
  plot(x2, y2, type="l")
  segments(lowlim, 0, lowlim, lowlimP, col="red")
  segments(upplim, 0, upplim, upplimP, col="red")
  segments(160, 0, 160, P160, col="blue")
  # Fill the area under the curve between the confidence interval
  x5 <- c(lowlim, lowlim:upplim, upplim)
  y5 <- dnorm(x5, mean=132, sd=13)
  x6 <- c(lowlim, lowlim, lowlim:upplim, upplim, upplim)
  y6 <- c(0, y5, 0)
  polygon(x6, y6, col="orange") # Beautiful <3

# Excercises : 
  # standard normal distribution (mean = 0, sd = 1)
    # Probability of obtaining a value euqal to 2
    dnorm(2) # 5.4% probability
    # Probability of obtaining a value greater than 3
    pnorm(3, lower.tail = FALSE) # 0.13%
  # Binomial Distribution, probability of obtaining
    # 10/10 successes when p=0.8
    dbinom(10, size = 10, prob = 0.8) # 10.74%
    # at most 3 successes out of 20, p=0.1
    pbinom(3, size = 20, prob = 0.1) # 86.70%
    # more than 3 successes out of 6, p=0.5
    pbinom(3, size = 6, prob = 0.5, lower.tail = FALSE) # 34.38%
