#**Example Problem with Solution:**

# Use *rbinom()* to generate two random samples of size 10,000 from the binomial distribution. 
# For the first sample, use p = 0.45 and n = 10. For the second sample, use p = 0.55 and n = 10.
# Convert the sample frequencies to sample proportions and compute the mean number of successes
# for each sample. Present these statistics.

#```{r Example, eval = TRUE, echo = TRUE}

set.seed(123)
sample.one <- table(rbinom(10000, 10, 0.45)) / 10000
sample.two <- table(rbinom(10000, 10, 0.55)) / 10000

successes <- seq(0, 10)

round(sum(sample.one*successes), digits = 1) # [1] 4.5
round(sum(sample.two*successes), digits = 1) # [1] 5.5
#```

#**Question: How do the simulated expectations compare to calculated binomial expectations?**

#***Answer:  The calculated binomial expectations are 10(0.45) = 4.5 and 10(0.55) = 5.5.
# After rounding the simulated results, the same values are obtained.***

#-----

##Data Analysis #2

#```{r analysis_setup1, message = FALSE, warning = FALSE}

# Perform the following steps to start the assignment.
 
# 1) Load/attach the following packages via library():  flux, ggplot2, gridExtra, moments, rockchalk, car.
# NOTE:  packages must be installed via install.packages() before they can be loaded.

library(flux)
library(ggplot2)
library(gridExtra)
library(moments)
# library(rockchalk) # base R code replaces requirement for this package
library(car)

# 2) Use the "mydata.csv" file from Assignment #1 or use the file posted on the course site.  Reading
# the files into R will require sep = "" or sep = " " to format data properly.  Use str() to check file
# structure.

mydata <- read.csv("mydata.csv", sep = ",")
# mydata <- read.csv(file.path("c:...", "mydata.csv"), sep = ",")
# mydata <- read.csv(file.path("c:/Rabalone/", "mydata.csv"), sep = ",")

str(mydata)

#```

### Test Items starts from here - There are 10 sections - total of 75 points ##############

##### Section 1: (5 points)

#(1)(a) Form a histogram and QQ plot using RATIO. Calculate skewness and kurtosis using
# 'rockchalk.' Be aware that with 'rockchalk', the kurtosis value has 3.0 subtracted from
# it which differs from the 'moments' package. 

#```{r Part_1a}
par(mfrow = c(1,2))
hist(mydata$RATIO, col = 'red', main = 'Histogram of Ratio')
qqnorm( mydata$RATIO, col = 'green', main = 'Normal Q-Q Plot' ) 
qqline(mydata$RATIO,col='red')
par(mfrow = c(1,1))

kurtosis(mydata$RATIO)
skewness(mydata$RATIO)

#```

#(1)(b) Tranform RATIO using *log10()* to create L_RATIO (Kabacoff Section 8.5.2, p. 199-200).
# Form a histogram and QQ plot using L_RATIO. Calculate the skewness and kurtosis. 
# Create a boxplot of L_RATIO differentiated by CLASS.

#```{r Part_1b}
L_RATIO<-log10(mydata$RATIO)
par(mfrow = c(1,2))
hist(L_RATIO, col = 'red', main = 'Histogram of log10(Ratio)')
qqnorm( L_RATIO, col = 'green', main = 'Normal log10 Q-Q Plot' ) 
qqline(L_RATIO,col='red')
par(mfrow = c(1,1))

kurtosis(L_RATIO)
skewness(L_RATIO)

boxplot(L_RATIO~CLASS, data=mydata, ylab="L_RATIO", xlab = "CLASS")

#```

#(1)(c) Test the homogeneity of variance across classes using *bartlett.test()* 
# (Kabacoff Section 9.2.2, p. 222). 

#```{r Part_1c}
bartlett.test(L_RATIO~mydata$CLASS)
#```

#**Essay Question: Based on steps 1.a, 1.b and 1.c, which variable RATIO or 
# L_RATIO exhibits better conformance to a normal distribution with homogeneous
# variances across age classes?  Why?** 

#***Answer: (Enter your answer here.)***


##### Section 2 (10 points) ###############################

#(2)(a) Perform an analysis of variance with *aov()* on L_RATIO using CLASS and SEX 
# as the independent variables (Kabacoff chapter 9, p. 212-229). Assume equal variances. 
# Perform two analyses. First, fit a model with the interaction term CLASS:SEX. Then, 
# fit a model without CLASS:SEX. Use *summary()* to obtain the analysis of variance tables
# (Kabacoff chapter 9, p. 227).

#```{r Part_2a}
anovacs<-aov(L_RATIO~CLASS*SEX, data = mydata)
summary(anovacs)
anovancs<-aov(L_RATIO~CLASS+SEX, data = mydata)
summary(anovancs)

#```

#**Essay Question:  Compare the two analyses.  What does the non-significant interaction
# term suggest about the relationship between L_RATIO and the factors CLASS and SEX?**

#***Answer: (Enter your answer here.)***

#(2)(b) For the model without CLASS:SEX (i.e. an interaction term), obtain multiple
# comparisons with the *TukeyHSD()* function. Interpret the results at the 95% 
# confidence level (*TukeyHSD()* will adjust for unequal sample sizes). 

#```{r Part_2b}
TukeyHSD(anovancs)

#```

#**Additional Essay Question:  first, interpret the trend in coefficients across
# age classes. What is this indicating about L_RATIO?  Second, do these results
# suggest male and female abalones can be combined into a single category labeled 
# as 'adults?' If not, why not?**

#***Answer: (Enter your answer here.)***


######  Section 3: (10 points) ##################

#(3)(a1) We combine "M" and "F" into a new level, "ADULT". (While this could be 
# accomplished using *combineLevels()* from the 'rockchalk' package, we use base R
# code because many students do not have access to the rockchalk package.) This 
# necessitated defining a new variable, TYPE, in mydata which had two levels:  "I" and "ADULT". 

#```{r Part_3a1}

# here we show how to define the new variable TYPE using only base R functions (no need for outside packages)
mydata$TYPE <- character(nrow(mydata))  # initialize the TYPE column as all blanks
for (i in seq(along = mydata$SEX)) {
  mydata$TYPE[i] <- 'I'
  if (mydata$SEX[i] == 'M' || mydata$SEX[i] == 'F') mydata$TYPE[i] <- 'ADULT'
}
mydata$TYPE <- factor(mydata$TYPE)
cat('\nCheck on definition of TYPE object (should be an integer): ', typeof(mydata$TYPE))
cat('\nmydata$TYPE is treated as a factor: ', is.factor(mydata$TYPE), '\n')
table(mydata$SEX, mydata$TYPE)

#```

#(3)(a2)  Present side-by-side histograms of VOLUME. One should display infant
# volumes and, the other, adult volumes. 

#```{r Part_3a2}
par(mfrow = c(1,2))
hist(mydata$VOLUME[mydata$TYPE == "I"], main = "Infant Volume", col = "red", xlab = "Volume")
hist(mydata$VOLUME[mydata$TYPE == "ADULT"], main = "Adult Volume", col = "green", xlab = "Volume")
par(mfrow = c(1,1))
#```


#**Essay Question: Compare the histograms.  How do the distributions differ?
# Are there going to be any difficulties separating infants from adults based on VOLUME?**

#***Answer: (Enter your answer here.)***

#(3)(b) Create a scatterplot of SHUCK versus VOLUME and a scatterplot of their 
# base ten logarithms, labeling the variables as L_SHUCK and L_VOLUME. Please be
# aware the variables, L_SHUCK and L_VOLUME, present the data as orders of magnitude 
# (i.e. VOLUME = 100 = 10^2 becomes L_VOLUME = 2). Use color to differentiate CLASS
# in the plots. Repeat using color to differentiate by TYPE. 

#```{r Part_3b}
L_SHUCK<-log10(mydata$SHUCK)
L_VOLUME<-log10(mydata$VOLUME)

color = rep(NA, length=length(mydata$CLASS))
color[which(mydata$CLASS == "A1")] = "lightblue"
color[which(mydata$CLASS == "A2")] = "blue"
color[which(mydata$CLASS == "A3")] = "violet"
color[which(mydata$CLASS == "A4")] = "darkgreen"
color[which(mydata$CLASS == "A5")] = "red"

par(mfrow = c(1,2))
plot(mydata$VOLUME,mydata$SHUCK, col = color, pch=16, xlab = "Volume",
     ylab = "Shuck")
legend("bottomright", legend=c("A1","A2","A3","A4","A5"),
       fill=c("lightblue","blue","violet","darkgreen","red"))
plot(L_VOLUME,L_SHUCK, col = color, pch=16, xlab = "L_Volume",
     ylab = "L_SHUCK")
legend("bottomright", legend=c("A1","A2","A3","A4","A5"),
       fill=c("lightblue","blue","violet","darkgreen","red"))
par(mfrow = c(1,1))

tcolor = rep(NA, length=length(mydata$TYPE))
tcolor[which(mydata$CLASS == "A1")] = "lightblue"
tcolor[which(mydata$CLASS == "A2")] = "violet"

par(mfrow = c(1,2))
plot(mydata$VOLUME,mydata$SHUCK, col = tcolor, pch=16, xlab = "Volume",
     ylab = "Shuck")
legend("bottomright", legend=c("Infant","Adult"),
       fill=c("lightblue","violet"))
plot(L_VOLUME,L_SHUCK, col = tcolor, pch=16, xlab = "L_Volume",
     ylab = "L_SHUCK")
legend("bottomright", legend=c("Infant","Adult"),
       fill=c("lightblue","violet"))
par(mfrow = c(1,1))


#```

#**Additional Essay Question:  Compare the two scatterplots. What effect(s) does
# log-transformation appear to have on the variability present in the plot?  What 
# are the implications for linear regression analysis? Where do the various CLASS 
# levels appear in the plots? Where do the levels of TYPE appear in the plots?**

#***Answer: (Enter your answer here.)***


######   Section 4: (5 points) ###################################

#(4)(a1) Since abalone growth slows after class A3, infants in classes A4 and A5
# are considered mature and candidates for harvest. Reclassify the infants in classes
# A4 and A5 as ADULTS. This reclassification could have been achieved using 
# *combineLevels()*, but only on the abalones in classes A4 and A5. We will do
# this recoding of the TYPE variable using base R functions. We will use this 
# recoded TYPE variable, in which the infants in A4 and A5 are reclassified as ADULTS,
# for the remainder of this data analysis assignment. 

#```{r Part_4a1}

for (i in seq(along = mydata$TYPE)) {
  if (mydata$CLASS[i] == 'A4' || mydata$CLASS[i] == 'A5') mydata$TYPE[i] <- 'ADULT'
}
mydata$TYPE <- factor(mydata$TYPE)
cat('\nCheck on redefinition of TYPE object (should be an integer): ', typeof(mydata$TYPE))
cat('\nmydata$TYPE is treated as a factor: ', is.factor(mydata$TYPE), '\n')
cat('\nThree-way contingency table for SEX, CLASS, and TYPE:\n')
print(table(mydata$SEX, mydata$CLASS, mydata$TYPE))

#```

#(4)(a2) Regress L_SHUCK as the dependent variable on L_VOLUME, CLASS and TYPE
# (Kabacoff Section 8.2.4, p. 178-186, the Data Analysis Video #2 and Black Section 14.2).
# Use the multiple regression model: L_SHUCK ~ L_VOLUME + CLASS + TYPE. Apply 
# *summary()* to the model object to produce results.

#```{r Part_4a2}
# model<-lm(formula = L_SHUCK~L_VOLUME + CLASS + TYPE, data = mydata)
model<-lm(L_SHUCK~L_VOLUME + CLASS + TYPE, data = mydata)
summary(model)


#```

#**Essay Question:  Interpret the trend in CLASS levelcoefficient estimates? 
# (Hint:  this question is not asking if the estimates are statistically significant. 
# It is asking for an interpretation of the pattern in these coefficients, and how this
# pattern relates to the earlier displays).**

# ***Answer: (Enter your answer here.)***

# **Additional Essay Question:  Is TYPE an important predictor in this regression? 
# (Hint:  This question is not asking if TYPE is statistically significant, but rather 
# how it compares to the other independent variables in terms of its contribution to 
# predictions of L_SHUCK for harvesting decisions.)  Explain your conclusion.**

# ***Answer: (Enter your answer here.)***

#-----

T#he next two analysis steps involve an analysis of the residuals resulting from the 
# regression model in (4)(a) (Kabacoff Section 8.2.4, p. 178-186, the Data Analysis Video #2).

#-----

###### Section 5: (5 points) #################################

#(5)(a) If "model" is the regression object, use model$residuals and construct
# a histogram and QQ plot. Compute the skewness and kurtosis. Be aware that with 
# 'rockchalk,' the kurtosis value has 3.0 subtracted from it which differs from the 'moments' package. 

#```{r Part_5a}
hist(model$residuals)
par(mfrow = c(1,2))
hist(model$residuals, col = 'red', main = 'Histogram of Residuals', xlab = "Residuals")
qqnorm( model$residuals, col = 'green', main = 'Q-Q Plot of Residuals' ) 
qqline(model$residuals,col='red')
par(mfrow = c(1,1))

kurtosis(model$residuals)
skewness(model$residuals)

#```

#(5)(b) Plot the residuals versus L_VOLUME, coloring the data points by CLASS and, a 
# second time, coloring the data points by TYPE. Keep in mind the y-axis and x-axis may
# be disproportionate which will amplify the variability in the residuals. Present boxplots
# of the residuals differentiated by CLASS and TYPE (These four plots can be conveniently
# presented on one page using *par(mfrow..)* or *grid.arrange()*. Test the homogeneity of
# variance of the residuals across classes using *bartlett.test()* (Kabacoff Section 9.3.2, p. 222).  

#```{r Part_5b}

par(mfrow = c(2,2))
color = rep(NA, length=length(mydata$CLASS))
color[which(mydata$CLASS == "A1")] = "lightblue"
color[which(mydata$CLASS == "A2")] = "blue"
color[which(mydata$CLASS == "A3")] = "violet"
color[which(mydata$CLASS == "A4")] = "darkgreen"
color[which(mydata$CLASS == "A5")] = "red"

plot(L_VOLUME, model$residuals, col = color, pch=16, xlab = "Volume",
     ylab = "Residuals")
legend("topleft", legend=c("A1","A2","A3","A4","A5"),
       fill=c("lightblue","blue","violet","darkgreen","red"))

tcolor = rep(NA, length=length(mydata$TYPE))
tcolor[which(mydata$CLASS == "A1")] = "lightblue"
tcolor[which(mydata$CLASS == "A2")] = "violet"

plot(L_VOLUME,model$residuals, col = tcolor, pch=16, xlab = "Volume",
     ylab = "Residuals")
legend("bottomright", legend=c("Infant","Adult"),
       fill=c("lightblue","violet"))
boxplot(model$residuals~CLASS, data=mydata, ylab="Residuals", xlab = "CLASS")
boxplot(model$residuals~TYPE, data=mydata, ylab="Residuals", xlab = "TYPE")
par(mfrow = c(1,1))


bartlett.test(model$residuals~mydata$CLASS)

#```

#**Essay Question:  What is revealed by the displays and calculations in (5)(a) and (5)(b)?
# Does the model 'fit'?  Does this analysis indicate that L_VOLUME, and ultimately VOLUME, 
# might be useful for harvesting decisions? Discuss.**  

#***Answer: (Enter your answer here.)***

#-----

#There is a tradeoff faced in managing abalone harvest. The infant population must 
# be protected since it represents future harvests. On the other hand, the harvest 
# should be designed to be efficient with a yield to justify the effort. This 
# assignment will use VOLUME to form binary decision rules to guide harvesting. 
# If VOLUME is below a "cutoff" (i.e. a specified volume), that individual will 
# not be harvested. If above, it will be harvested. Different rules are possible.

# The next steps in the assignment will require consideration of the proportions
# of infants and adults harvested at different cutoffs. For this, similar "for-loops"
# will be used to compute the harvest proportions. These loops must use the same values
# for the constants min.v and delta and use the same statement "for(k in 1:10000)."  
# Otherwise, the resulting infant and adult proportions cannot be directly compared 
# and plotted as requested. Note the example code supplied below.

#-----

#### Section 6: (5 points) ########################

#(6)(a) A series of volumes covering the range from minimum to maximum abalone volume
# will be used in a "for loop" to determine how the harvest proportions change as the
# "cutoff" changes. Code for doing this is provided.

#```{r Part_6a}

idxi <- mydata$TYPE == "I"
idxa <- mydata$TYPE == "ADULT"

max.v <- max(mydata$VOLUME)
min.v <- min(mydata$VOLUME)
delta <- (max.v - min.v)/10000
prop.infants <- numeric(10000)
prop.adults <- numeric(10000)
volume.value <- numeric(10000)

total.infants <- sum(idxi)  
total.adults <- sum(idxa)

for (k in 1:10000) { 
	value <- min.v + k*delta
	volume.value[k] <- value
	prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
	prop.adults[k] <-  sum(mydata$VOLUME[idxa] <= value)/total.adults
}

# prop.infants shows the impact of increasing the volume cutoff for
# harvesting. The following code shows how to "split" the population at
# a 50% harvest of infants.

n.infants <- sum(prop.infants <= 0.5)
split.infants <- min.v + (n.infants + 0.5)*delta  # This estimates the desired volume.
split.infants

n.adults <- sum(prop.adults <= 0.5)
split.adults <- min.v + (n.adults + 0.5)*delta
split.adults

max(mydata$VOLUME[mydata$TYPE == "I"])
#AT 526.6383 we will be protecting every infant in the data set.
#At 830 we will protecting essentially every body all but one adult
#```

#(6)(b) Present a plot showing the infant proportions and the adult proportions versus
# volume.value. Compute the 50% "split" volume.value for each and show on the plot.   

#```{r Part_6b}
plot(volume.value, prop.adults, col="red", main = "Proportion of Adult and Infants Protected",
     xlab = "Volume", ylab = "Proportion")
par(new=TRUE)
plot(volume.value, prop.infants, col="blue",
     xlab = "Volume", ylab = "Proportion")
abline(h=0.5)
abline(v=133.82)
text(175, 0.45, labels = c("133.82"), cex=0.8)
abline(v=384.51)
text(415, 0.45, labels = c("384.51"), cex=0.8)
legend("bottomright", legend=c("Adult","Infant"),
       col=c("lightblue","violet"), lty, seq_len(5))
#```

#**Essay Question:  The two 50% "split" values serve a descriptive purpose illustrating
# the difference between the populations. What do these values suggest regarding possible
# cutoffs for harvesting?** 

#***Answer: (Enter your answer here.)***

#-----

# This part will address the determination of a volume.value corresponding to the
# observed maximum difference in harvest percentages of adults and infants. To 
# calculate this result, the vectors of proportions from item (6) must be used. 
# These proportions must be converted from "not harvested" to "harvested" 
# proportions by using (1 - prop.infants) for infants, and (1 - prop.adults) 
# for adults. The reason the proportion for infants drops sooner than adults 
# is that infants are maturing and becoming adults with larger volumes.

#-----

###### Section 7: (10 points)  #######################

#(7)(a) Evaluate a plot of the difference 
# ((1 - prop.adults) - (1 - prop.infants)) versus volume.value. 
# Compare to the 50% "split" points determined in (6)(a). There is considerable
# variability present in the peak area of this plot. The observed "peak" difference
# may not be the best representation of the data. One solution is to smooth the 
# data to determine a more representative estimate of the maximum difference.

#```{r Part_7a}
y.loess.a <- loess(1 - prop.adults ~ volume.value, span = 0.25,
                   family = c("symmetric"))
y.loess.i <- loess(1 - prop.infants ~ volume.value, span = 0.25,
                   family = c("symmetric"))
smooth.difference <- predict(y.loess.a) - predict(y.loess.i)

difference <- ((1-prop.adults) - (1-prop.infants))

#```

#(7)(b) Since curve smoothing is not studied in this course, code is supplied 
# below. Execute the following code to create a smoothed curve to append to the 
# plot in (a). The procedure is to individually smooth (1-prop.adults) and 
# (1-prop.infants) before determining an estimate of the maximum difference. 

#```{r Part_7b}

y.loess.a <- loess(1 - prop.adults ~ volume.value, span = 0.25,
	family = c("symmetric"))
y.loess.i <- loess(1 - prop.infants ~ volume.value, span = 0.25,
	family = c("symmetric"))
smooth.difference <- predict(y.loess.a) - predict(y.loess.i)

#```

#(7)(c) Present a plot of the difference 
# ((1 - prop.adults) - (1 - prop.infants)) versus volume.value 
# with the variable smooth.difference superimposed. Determine the volume.value 
# corresponding to the maximum smoothed difference (Hint:  use *which.max()*). 
# Show the estimated peak location corresponding to the cutoff determined.

#```{r Part_7c}
plot(volume.value, difference, type = "l", col="red4", xlab = "Volume", 
     ylab = "Difference in Proportions Harvested", 
     main = "Difference in Harvest Proportions")
lines(volume.value, smooth.difference, col = "gray2", lwd = 1.5, lty = 2)
abline(v = volume.value[which.max(difference)], lwd = 1.5, lty = 2)
text(285, 0.35, labels = paste("volume = ", round(volume.value[which.max(difference)], digits = 3)), srt = 90, cex = 1)


#```

#(7)(d) What separate harvest proportions for infants and adults would result
# if this cutoff is used? Show the separate harvest proportions 
# (NOTE:  the adult harvest proportion is the "true positive rate" and the 
# infant harvest proportion is the "false positive rate").

# Code for calculating the adult harvest proportion is provided.

#```{r Part_7d}

max.difference <- volume.value[which.max(smooth.difference)] 
max.difference.adults <- (1 - prop.adults)[which.max(smooth.difference)]
max.difference.adults
max.difference.infants <- (1 - prop.infants)[which.max(smooth.difference)]
max.difference.infants

#```

#-----

#There are alternative ways to determine cutoffs. Two such cutoffs are described below.

#-----

######  Section 8: (10 points)  ###################

#(8)(a) Harvesting of infants in CLASS "A1" must be minimized. The smallest 
# volume.value cutoff that produces a zero harvest of infants from 
# CLASS "A1" may be used as a baseline for comparison with larger cutoffs. 
# Any smaller cutoff would result in harvesting infants from CLASS "A1."  

# Compute this cutoff, and the proportions of infants and adults with VOLUME
# exceeding this cutoff. Code for determining this cutoff is provided. 
# Show these proportions.

#```{r Part_8a}
volume.value[volume.value > max(mydata[mydata$CLASS == "A1" &
  mydata$TYPE == "I", "VOLUME"])][1] # [1] 206.786
zero.A1 <- volume.value[volume.value > max(mydata[mydata$CLASS == "A1" &
                                                    mydata$TYPE == "I", "VOLUME"])][1]
zero.A1.adults <- sum(mydata$VOLUME > zero.A1 & mydata$TYPE == "ADULT")/total.adults
zero.A1.adults
zero.A1.infants <- sum(mydata$VOLUME > zero.A1 & mydata$TYPE == "I")/total.infants
zero.A1.infants
#```

#(8)(b) Another cutoff is one for which the proportion of adults not harvested
# equals the proportion of infants harvested. This cutoff would equate these rates; 
# effectively, our two errors:  'missed' adults and wrongly-harvested infants. 
# This leaves for discussion which is the greater loss:  a larger proportion of 
# adults not harvested or infants harvested?  This cutoff is 237.7383. 
# Calculate the separate harvest proportions for infants and adults using this cutoff. 
# Show these proportions.  Code for determining this cutoff is provided.  

#```{r Part_8b}
volume.value[which.min(abs(prop.adults - (1-prop.infants)))] # [1] 237.6391

error.i<-nrow(mydata[mydata$TYPE == "I" & mydata$VOLUME > 237.6391, ])/ total.infants
error.i
error.a<-nrow(mydata[mydata$TYPE == "ADULT" & mydata$VOLUME > 237.6391, ])/ total.adults
error.a
# test<-nrow(mydata[mydata$TYPE == "A" , ])/ total.adults
# mydata$TYPE
#```


##### Section 9: (5 points) ###########

#(9)(a) Construct an ROC curve by plotting (1 - prop.adults) versus (1 - prop.infants). 
# Each point which appears correspondADULTs to a particular volume.value. Show the location
# of the cutoffs determined in (7) and (8) on this plot and label each. 

#```{r Part_9}
plot(1-prop.infants, 1-prop.adults, type="l", col="darkblue", lwd=2, 
     main = "ROC curve of adult and infant harvest proportion",
     xlab = "Infant Harvest Proportion", ylab = "Adult Harvest Proportion")
abline(0,1, lty=2, col="red4")

points(max.difference.infants, max.difference.adults, cex=1.5)
text(0.15, 0.82, labels = c("equal harvest\n\n", "vol = 237.6"), cex=0.8)
points(zero.A1.infants, zero.A1.adults, cex=1.5)
text(0.25, 0.65, labels = c("max difference\n\n", "vol = 262.1"), cex=0.8)
points(error.i, error.a, cex=1.5)
text(0.38, 0.75, labels = c("Zero A1 infants\n\n", "vol = 206.8"), cex=0.8)

#```

#(9)(b) Numerically integrate the area under the ROC curve and report your result. 
# This is most easily done with the *auc()* function from the "flux" package.  
# Areas-under-curve, or AUCs, greater than 0.8 are taken to indicate good discrimination potential. 

#```{r Part_9b}
auc((1 - prop.infants),(1 - prop.adults))

#```


##### Section 10: (10 points) ###################

#(10)(a) Prepare a table showing each cutoff along with the following:
# 	1) true positive rate (1-prop.adults,
# 	2) false positive rate (1-prop.infants),
# 	3) harvest proportion of the total population
 	
#```{r Part_10} 	
sum(mydata$VOLUME > 237.6391)/nrow(mydata)
py_1 <- sum(mydata$VOLUME > volume.value[which.max(smooth.difference)])/nrow(mydata)
py_2 <- sum(mydata$VOLUME> volume.value[volume.value>max(mydata[mydata$CLASS=="A1" &
                                                  mydata$TYPE=="I","VOLUME"])][1])/nrow(mydata)
py_3 <- sum(mydata$VOLUME>volume.value[which.min(abs(prop.adults-(1-prop.infants)))])/nrow(mydata)

max.diff <- c(volume.value[which.max(smooth.difference)],max.difference.adults, max.difference.infants, py_1)
max.diff <- round(max.diff,digits=3)
z.a.1 <- c(volume.value[volume.value>max(mydata[mydata$CLASS=="A1" & 
                     mydata$TYPE == "I", "VOLUME"])][1],zero.A1.adults, zero.A1.infants, py_2)
z.a.1 <- round(z.a.1,digits = 3)
equal_error <- c(volume.value[which.min(abs(prop.adults-(1-prop.infants)))], error.a, error.i, py_3)
equal_error<-round(equal_error,digits=3)

bind<-rbind(max.diff, z.a.1, equal_error)
rownames(bind)<-c("Max.Diff", "z.a.1", "equal_error")
cutoff<-data.frame(bind)
colnames(cutoff)<-c("Volume", "TPR", "FPR", "PropYield")
cutoff
#```
 	
#**Essay Question: Based on the ROC curve, it is evident a wide range of 
# possible "cutoffs" exist. Compare and discuss the three cutoffs determined 
# in this assignment.**   

#***Answer: (Enter your answer here.)***



#**Final Essay Question:  Assume you are expected to make a presentation of 
# your analysis to the investigators How would you do so?  Consider the following in your answer:**

# 1. Would you make a specific recommendation or outline various choices and tradeoffs?

# 2. What qualifications or limitations would you present regarding your analysis?

# 3. If it is necessary to proceed based on the current analysis, what suggestions 
# would you have for implementation of a cutoff?  4)  What suggestions would you have 
# for planning future abalone studies of this type? 

#***Answer: (Enter your answer here.)***

