# **Example Problem with Solution:**

# Use *rbinom()* to generate two random samples of size 10,000 from the binomial distribution. 
# For the first sample, use p = 0.45 and n = 10. For the second sample, use p = 0.55 and n = 10.
# Convert the sample frequencies to sample proportions and compute the mean number of successes
# for each sample. Present these statistics.


set.seed(123)
sample.one <- table(rbinom(10000, 10, 0.45)) / 10000
sample.two <- table(rbinom(10000, 10, 0.55)) / 10000

successes <- seq(0, 10)

round(sum(sample.one*successes), digits = 1) # [1] 4.5
round(sum(sample.two*successes), digits = 1) # [1] 5.5

#**Question:  How do the simulated expectations compare to calculated binomial expectations?**

#***Answer:  The calculated binomial expectations are 10(0.45) = 4.5 and 10(0.55) = 5.5.  
# After rounding the simulated results, the same values are obtained.***

#-----

# Submit both the .Rmd and .html files for grading. You may remove the instructions and 
# example problem above, but do not remove the YAML metadata block or the first, "setup"
# code chunk.  Address the steps that appear below and answer all the questions. 
# Be sure to address each question with code and comments as needed.  You may use either 
# base R functions or ggplot2 for the visualizations.

#-----

# The following code chunk will:

# (a) load the "ggplot2", "gridExtra" and "knitr" packages, assuming each has been installed on your machine,
# (b) read-in the abalones dataset, defining a new data frame, "mydata,"
# (c) return the structure of that data frame, and
# (d) calculate new variables, VOLUME and RATIO.

# Do not include package installation code in this document. Packages should be installed via the Console 
# or 'Packages' tab. You will also need to download the abalones.csv from the course site to a known 
# location on your machine. Unless a *file.path()* is specified, R will look to directory where this
# .Rmd is stored when knitting.


# a) Load the ggplot2 and gridExtra packages.

library(ggplot2)
library(gridExtra)
library(knitr)

# b) Use read.csv() to read the abalones.csv into R, assigning the data frame to "mydata."

mydata <- read.csv("abalones.csv", sep = ",")


# c) Use the str() function to verify the structure of "mydata." You should have 1036 observations
# of eight variables.

str(mydata)

# d) Define two new variables, VOLUME and RATIO. Use the following statements to define VOLUME and
# RATIO as variables appended to the data frame "mydata."

mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
mydata$RATIO <- mydata$SHUCK / mydata$VOLUME

# -----

### Test Items starts from here - There are 6 sections ##########################

##### Section 1: (6 points) Summarizing the data.

# (1)(a) (1 point) Use *summary()* to obtain and present descriptive statistics from mydata.
# Use table() to present a frequency table using CLASS and RINGS. There should be 115 cells
# in the table you present.  

#```{r Part_1a}


#```


# **Question (1 point):  Briefly discuss the variable types and distributional implications such
# as potential skewness and outliers.**

# ***Answer: (Enter your answer here.)***


# (1)(b) (1 point) Generate a table of counts using SEX and CLASS. Add margins to this table 
# (Hint: There should be 15 cells in this table plus the marginal totals. Apply *table()* first, 
# then pass the table object to *addmargins()* (Kabacoff Section 7.2 pages 144-147)).  
# Lastly, present a barplot of these data; ignoring the marginal totals.  

#```{r Part_1b}


#```

# **Essay Question (2 points):  Discuss the sex distribution of abalones.  What stands out about 
# the distribution of abalones by CLASS?**

#***Answer: (Enter your answer here.)***



# (1)(c) (1 point) Select a simple random sample of 200 observations from "mydata" and identify
# this sample as "work." Use *set.seed(123)* prior to drawing this sample. Do not change the
# number 123. Note that *sample()* "takes a sample of the specified size from the elements of x."
# We cannot sample directly from "mydata." Instead, we need to sample from the integers, 
# 1 to 1036, representing the rows of "mydata." Then, select those rows from the data frame 
# (Kabacoff Section 4.10.5 page 87). 

# Using "work", construct a scatterplot matrix of variables 2-6 with *plot(work[, 2:6])* 
# (these are the continuous variables excluding VOLUME and RATIO). The sample "work" will not
# be used in the remainder of the assignment. 

#```{r Part_1c}


#```

#-----

##### Section 2: (5 points) Summarizing the data using graphics.

# (2)(a) (1 point) Use "mydata" to plot WHOLE versus VOLUME. Color code data points by CLASS.

#```{r Part_2a}


#```


# (2)(b) (2 points) Use "mydata" to plot SHUCK versus WHOLE with WHOLE on the horizontal axis. Color
# code data points by CLASS. As an aid to interpretation, determine the maximum value of the ratio of
# SHUCK to WHOLE.  Add to the chart a straight line with zero intercept using this maximum value as the
# slope of the line. If you are using the 'base R' *plot()* function, you may use *abline()* to add this line to the plot. Use *help(abline)* in R to determine the coding for the slope and intercept arguments in the functions. If you are using ggplot2 for visualizations, *geom_abline()* should be used.

#```{r Part_2b}


# ```

# **Essay Question (2 points):  How does the variability in this plot differ from the plot in (a)?
# Compare the two displays.  Keep in mind that SHUCK is a part of WHOLE.  Consider the location of
# the different age classes.**

# ***Answer: (Enter your answer here.)***

# -----

##### Section 3: (8 points) Getting insights about the data using graphs.

# (3)(a) (2 points) Use "mydata" to create a multi-figured plot with histograms, boxplots and Q-Q plots
# of RATIO differentiated by sex. This can be done using *par(mfrow = c(3,3))* and base R or 
# *grid.arrange()* and ggplot2. The first row would show the histograms, the second row the 
# boxplots and the third row the Q-Q plots. Be sure these displays are legible.  

#```{r Part_3a}


#```

# **Essay Question (2 points): Compare the displays.  How do the distributions compare to normality?
# Take into account the criteria discussed in the sync sessions to evaluate non-normality.**

# ***Answer: (Enter your answer here.)***

# (3)(b) (2 points) Use the boxplots to identify RATIO outliers (mild and extreme both) for each sex.
# Present the abalones with these outlying RATIO values along with their associated variables in "mydata"
# (Hint:  display the observations by passing a data frame to the kable() function).  

#```{r Part_3b}


#```

# **Essay Question (2 points):  What are your observations regarding the results in (3)(b)?**

# ***Answer: (Enter your answer here.)***

#-----

##### Section 4: (8 points) Getting insights about possible predictors.

# (4)(a) (3 points) With "mydata," display side-by-side boxplots for VOLUME and WHOLE, 
# each differentiated by CLASS There should be five boxes for VOLUME and five for WHOLE. 
# Also, display side-by-side scatterplots:  VOLUME and WHOLE versus RINGS. Present these
# four figures in one graphic:  the boxplots in one row and the scatterplots in a second row. 
# Base R or ggplot2 may be used.


#```{r Part_4a}


#```

#**Essay Question (5 points) How well do you think these variables would perform as predictors of age?  Explain.**

#***Answer: (Enter your answer here.)***

#-----

##### Section 5: (12 points) Getting insights regarding different groups in the data.

# (5)(a) (2 points) Use *aggregate()* with "mydata" to compute the mean values of 
# VOLUME, SHUCK and RATIO for each combination of SEX and CLASS. Then, using *matrix()*, 
# create matrices of the mean values. Using the "dimnames" argument within *matrix()* or 
# the *rownames()* and *colnames()* functions on the matrices, label the rows by SEX and
# columns by CLASS. Present the three matrices (Kabacoff Section 5.6.2, p. 110-111). 
# The *kable()* function is useful for this purpose.  You do not need to be concerned with
# the number of digits presented.

#```{r Part_5a}


#```

#(5)(b) (3 points) Present three graphs. Each graph should include three lines, one for each sex.
# The first should show mean RATIO versus CLASS; the second, mean VOLUME versus CLASS; the third, 
# mean SHUCK versus CLASS. This may be done with the 'base R' *interaction.plot()* function or 
# with ggplot2 using *grid.arrange()*.

#```{r Part_5b, fig.width = 9}


#```

# **Essay Question (2 points):  What questions do these plots raise?
# Consider aging and sex differences.**

# ***Answer: (Enter your answer here.)***

# 5(c) (3 points) Present four boxplots using *par(mfrow = c(2, 2)* or *grid.arrange()*.
# The first line should show VOLUME by RINGS for the infants and, separately, for the adult;
# factor levels "M" and "F," combined. The second line should show WHOLE by RINGS for the 
# infants and, separately, for the adults. Since the data are sparse beyond 15 rings, 
# limit the displays to less than 16 rings. One way to accomplish this is to generate a new
# data set using subset() to select RINGS < 16.  Use ylim = c(0, 1100) for VOLUME 
# and ylim = c(0, 400) for WHOLE.  If you wish to reorder the displays for presentation
# purposes or use ggplot2 go ahead.

#```{r Part_5c}


#```

# **Essay Question (2 points):  What do these displays suggest about abalone growth?  
# Also, compare the infant and adult displays.  What differences stand out?**

# ***Answer: (Enter your answer here.)***

#-----

##### Section 6: (11 points) Conclusions from the Exploratory Data Analysis (EDA).

#**Conclusions**


#**Essay Question 1) (5 points)	Based solely on these data, what are plausible statistical
# reasons that explain the failure of the original study? Consider to what extent physical
# measurements may be used for age prediction.**

#***Answer: (Enter your answer here.)***

#**Essay Question 2) (3 points) Do not refer to the abalone data or study.  If you were
# presented with an overall histogram and summary statistics from a sample of some population
# or phenomenon and no other information, what questions might you ask before accepting them
# as representative of the sampled population or phenomenon?**

#***Answer: (Enter your answer here.)***

#**Essay Question 3)  (3 points)	Do not refer to the abalone data or study.  What do you
# see as difficulties analyzing data derived from observational studies? Can causality
# be determined?  What might be learned from such studies?**

#***Answer: (Enter your answer here.)***

