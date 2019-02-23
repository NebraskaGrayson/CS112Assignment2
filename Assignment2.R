#PROBLEM NO.1#

set.seed(5)
sample_size = 999 #number of units in original sample

x <- c(rnorm(sample_size, 10, 5)) #list of x values on a normal distribution with a mean of 10 & S.D. of 5
y <- 0.5*x + rnorm(sample_size, 15, 7) + 4 #linear regression equation (slope of 0.5, intercept of 4) with a mean of 15 & S.D. of 7

data_frame <- data.frame(x,y) #putting units in a dataframe

plot(data_frame$y ~ data_frame$x, main = "Original Dataframe w/ Outlier", xlab = "x-axis", ylab = "y-axis") #plot original dataset

regression1 <- lm(y ~ x, data = data_frame) #regression formula of original data
summary(regression1)

abline(regression1) #plots regression line on data 

outlier <- c(25, -10000) #create and add outlier to dataframe 
data_frame2 <- rbind(data_frame, outlier) #make new dataframe binding the outlier to the original dataset
regression2 <- lm(y ~ x, data = data_frame2) #regression formula of new dataset
summary(regression2)

abline(regression1) #plots regression line of original 999
abline(regression2) #plots regression line of original 999 w/ outlier, slope now appears as negative

#write caption for figure (as it would serve in an econometrics textbook, *see assign*)


##PROBLEM NO.2##

library(MASS)
library(Matching)
library(arm)

data("lalonde")

treatment <- which(lalonde$treat == 1) #defining all rows with treat value of 1 as treatment
control <- which(lalonde$treat == 0) #defining all rows with treat value of 0 as control

control_only <- lalonde[control,] #slicing data to separate control and treatment
treat_only <- lalonde[treatment,]

lalonde_lm = lm(re78 ~ age + educ + re74 + re75 + 
                   I(educ*re74) + I(educ*re75) +
                   I(age*re74) + I(age*re75) +
                   I(age*age) + I(re74*re75), data = control_only) #linear regression function including variables on control only
summary(lalonde_lm)

lalonde_sim = sim(lalonde_lm, n.sims = 10000) #setting up number of simulations to run


#create matrix datasets

matrix_median_predictive <- matrix(NA, nrow = 10000, ncol = 39) #making matrices for simulation data, ncol represents each age
matrix_median_expected <- matrix(NA, nrow = 10000, ncol = 39)

for(i in 17:55){
  predictor_values <-
    c(1, i, median(lalonde$educ),
      median(lalonde$re74),
      median(lalonde$re75),
      median(lalonde$educ)*median(lalonde$re74),
      median(lalonde$educ)*median(lalonde$re75),
      i*median(lalonde$re74),
      i*median(lalonde$re75),
      (i^2),
      median(lalonde$re74)*median(lalonde$re75)) #for loop which runs all variables held constant through age
  
  for(n in 1:10000){
    matrix_median_predictive[n, i-16] <-
      sum(lalonde_sim@coef[n,]*predictor_values) +  
      rnorm(1, mean = 0, sd = lalonde_sim@sigma[n]) #predictive values which include error
    matrix_median_expected[n, i-16] <- 
      sum(lalonde_sim@coef[n,]*predictor_values) #for loop which puts all simulation data in the storage matrix
  }
 
}

matrix_quantile_predictive <- matrix(NA, nrow = 10000, ncol = 39) #same as above but looking at quantile instead of median
matrix_quantile_expected <- matrix(NA, nrow = 10000, ncol = 39)

for(i in 17:55){
  predictor_values <-
    c(1, i, quantile(lalonde$educ, probs = c(0.75)),
      quantile(lalonde$re74, probs = c(0.75)),
      quantile(lalonde$re75, probs = c(0.75)),
      quantile(lalonde$educ, probs = c(0.75))*quantile(lalonde$re74, probs = c(0.75)),
      quantile(lalonde$educ, probs = c(0.75))*quantile(lalonde$re75, probs = c(0.75)),
      i*quantile(lalonde$re74, probs = c(0.75)),
      i*quantile(lalonde$re75, probs = c(0.75)),
      (i^2),
      quantile(lalonde$re74, probs = c(0.75))*quantile(lalonde$re75, probs = c(0.75)))
  
  for(n in 1:10000){
    matrix_quantile_predictive[n, i-16] <-
      sum(lalonde_sim@coef[n,]*predictor_values) +
      rnorm(1, mean = 0, sd = lalonde_sim@sigma[n])
    matrix_quantile_expected[n, i-16] <- 
      sum(lalonde_sim@coef[n,]*predictor_values)
  }
  
}

confint_median_pred <- apply(matrix_median_predictive, 2, quantile, probs = c(0.025, 0.975)) #95% confidence interval run on all 4 vectors make in for loops
confint_median_exp <- apply(matrix_median_expected, 2, quantile, probs = c(0.025, 0.975))
confint_quantile_pred <- apply(matrix_quantile_predictive, 2, quantile, probs = c(0.025, 0.975))
confint_quantile_exp <- apply(matrix_quantile_expected, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:10000), y = c(1:10000), type = "n", xlim = c(17,55), ylim = c(-10000, 20000),
     main = "Real Income in 1978, Predicted Values", xlab = "Age", ylab = "Income") #plot of all predicted

for(age in 17:55){ #for loop, inputting all predictive values, only looking at ages 17-55
  segments(
    x0 = age,
    y0 = confint_median_pred[1, age -16],
    x1 = age,
    y1 = confint_median_pred[1, age -16],
    lwd = 5, col = "cadetblue3") #confints of median predictive values in blue
  segments(
    x0 = age,
    y0 = confint_quantile_pred[1, age - 16],
    x1 = age,
    y1 = confint_quantile_pred[2, age - 16],
    lwd = 5, col = "indianred1") #confints of quantile predictive values in red
  
}

plot(x = c(1:10000), y = c(1:10000), type = "n", xlim = c(17,55), ylim = c(-10000, 20000),
     main = "Real Income in 1978, Expected Values", xlab = "Age", ylab = "Income")#plot of all expected

for(age in 17:55){ #for loop, inputting all expected values, only looking at ages 17-55
  segments(
    x0 = age,
    y0 = confint_median_exp[1, age -16],
    x1 = age,
    y1 = confint_median_exp[1, age -16],
    lwd = 5, col = "cadetblue3") #confints of median expectant values in blue
  segments(
    x0 = age,
    y0 = confint_quantile_exp[1, age - 16],
    x1 = age,
    y1 = confint_quantile_exp[2, age - 16],
    lwd = 5, col = "indianred1") #confints of quantile expected values in red
  
}


###PROBLEM NO.3###
data("PlantGrowth")
names(PlantGrowth)
PlantGrowth
PG <- PlantGrowth
new_PG <- PG[-c(21:30),]
new_PG #The datasubset excluding rows with group of "treatement2"

#setting treatment1 to 1 and control to 0.
new_PG$group <- (as.numeric(new_PG$group)) -1
new_PG$group 

y <- new_PG$weight
x <- new_PG$group

regression <- lm(y ~ x) #regression model 

summary(regression)

length_new_PG <- length(new_PG$weight) #determining the length of the vector
t1 <- which(new_PG$group == 1) #specifying treatment and control for bootstrapping
c1 <- which(new_PG$group == 0)
storage_vector <- c() #creating empty storage vector to imput results from sims

sims = 10000

for (i in 1:sims) {
  boot <- new_PG[sample(nrow(new_PG), 20, replace = TRUE),]
  model <- lm(weight ~ group, data = boot)
  coef <- model$coefficients[2]
  storage_vector <- c(storage_vector, coef) 
} #using model to compare results of resampling the data set (bootstrapping) & inputting into storage vector

quantile(storage_vector, probs = c(0.975, 0.025)) #finding confidence interval at 95% w/ bootstrapped data
#97.5%: 0.2473, 2.5%: -0.9640
summary(storage_vector) 

hist(storage_vector, breaks = 20, main = "Bootstrapped Coefs of New PlantGrowth Data", xlab = "Variation of Weight", ylab = "Frequency", col = "palegreen3")

#analytical method
y <- new_PG$weight
x <- new_PG$group
regression <- lm(y ~ x)
confint(regression, level = 0.95,) #finds values of confidence interval at 95% (97.5%: 0.2833, 2.5%: -1.0253)

summary(regression)$coef #0.3114 is the Std. Error of the coef


####PROBLEM NO.4#### write a function (5 lines max) 

#R^2 is the square of the correlation between two vectors.(independent is ys & predicted ys, dependent is R^2)
r_sqr<- function(y_predicted, y_actual){
  tss <-sum((y_actual-mean(y_actual))^2) # total sum of squares formula
  rss <-sum((y_actual-y_predicted)^2) # residual sum of squares formula
  return(1-rss/tss) #R^2 formula
}

#test on new_PG data
test <- lm(weight ~ group, data = new_PG) 
predicted <- predict(test)
r_sqr(predicted, new_PG$weight) #comparing actual weight to predicted weight
#0.0730776 r-squared value from manual formula 

summary(test) #confirmed match with output from above function (0.07308)


#####PROBLEM NO.5##### 

library(foreign)
df <- read.dta("nsw.dta")
View(df)
names(df)

#est. prob of being assigned treat for every obs in the df
log_model <- glm(treat ~ age + education + black + hispanic + married + nodegree + re75,  family = binomial, data = df,)#logistic regression model: with output as true or false for treat

summary(log_model)

predicted_probabilities <- predict.glm(log_model, data = df, type = "response")
newdf <- cbind(predicted_probabilities, df) #combining predictive model values to original regression model with data

treatment <- subset(newdf, df$treat == 1)
control <- subset(newdf, df$treat == 0)

hist_treat <- hist(treatment$predicted_probabilities, breaks = 25) #assigns hist of treat values alone, 25 breaks)
hist_control <- hist(control$predicted_probabilities, breaks = 25) #assigns hist of control values alone, 25 breaks)
plot(hist_control, col = "cadetblue1", main = "Lalonde Sample Distribution", xlab = "Distribution of Probability of Treatment", ylim = c(0,100)) #plots hist of control, includes titles
plot(hist_treat, col = "indianred1", add=T) #plots hist of control and adds it to the hist of treat
legend("topright",c("Control", "Treatment"), col = c("cadetblue1","indianred1"), lwd = 7) #adds legend to top right of hist, width of color symbol in legend included at end