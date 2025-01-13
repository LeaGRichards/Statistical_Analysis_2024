### Statistic tests ###

## Student-t
  # Make a dataset
  intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
  # Reference mean = 7725
  mean(intake)
  sd(intake)
  # Formula based t-test and with R formula
  (mean(intake)-7725)/(sd(intake)/sqrt(11))
  t.test(intake, mu = 7725)

## Wilcoson test
  # Manually
  D <- intake-7725
  signs <- ifelse(D > 0, 1, -1)
  ranks <- rank(abs(D))  
  rs <- ranks*signs  
  V <- sum(rs[rs>0])
  # with R formula
  wilcox.test(intake,mu=7725)
  
## Two-sample t-test
  energy <- read.table("energy.txt", header=T)
  energy$stature <- as.factor(energy$stature)  
  str(energy)  
  summary(energy)  

  t.test(energy$expend~energy$stature)  

## Two-sample wilcoxon test
  wilcox.test(energy$expend~energy$stature)

## Exercise 
  darwin <- read.table("darwin.txt", header=T)
  darwin$Group <- as.factor(darwin$Group)  
  wilcox.test(darwin$Height~darwin$Group)
  t.test(darwin$Height~darwin$Group) 

## Paired t-test and non-parametric paired test
  intake <- read.table("intake.txt", header=T)
  plot(intake$pre, intake$post)  
  t.test(intake$pre,intake$post,paired=TRUE)  
  t.test(intake$pre,intake$post) # To compare results if pairing is not considered
  wilcox.test(intake$pre, intake$post, paired = TRUE) # Non-parametric version

## Binomial test
  binom.test(39, 215, p=0.15)
  binom.test(16, 20, p=0.5)  

## Proportion test
  prop.test(39, 215, 0.15)
  
  success <- c(9,4) 
  total <- c(12,13)  
  prop.test(success, total)  
    # Confidence interval does NOT include 0 (which would be H0) but p-value > 0.05 ... 
    # Difference due to the test not working for small values 

## Exercise
  s1 <- c(210, 120)
  t2 <- c(740,660)
  prop.test(s1, t2)
    # The probability is not the same
  
## Chi-squared test
  data <- matrix(c(9, 4, 12, 13), nrow = 2)
  chisq.test(data)
  chisq.test(data)$exp  

## Fisher test
  fisher.test(data)
    # Confidence interval is opposite p-value significancy result
    # As confidence interval is approximated, and o-value is exact, we choose result based on p-value

## Exercise
  data2 <- matrix(c(38, 11, 14, 51), nrow = 2, byrow = TRUE,
                dimnames = list(Rows=c("Fair eyes", "Dark eyes"), 
                                Cols = c("Fair hair", "Dark hair")))

  chisq.test(data2)
  fisher.test(data2)

  predata3 <- c(241, 652, 1537, 598, 36, 46, 38, 21, 218, 327, 106, 67)
  data3 <- matrix(predata3, nrow=3, byrow = TRUE, 
                  dimnames = list(Rows = c("Married", "Prev. married", "Single"),
                                  Cols = c("0", "1-150", "151-300", ">300")))  # default is by column
  chisq.test(data3)    
  fisher.test(data3) # unsure... :(
