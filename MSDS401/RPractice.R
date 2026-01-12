houses <-read.csv("home_prices.csv")
str(houses)
price<-houses$PRICE
set.seed(9999)
SRS<-sample(price, 12)
print(SRS)
mean(SRS)
SS<-price[seq(from=7, to=117, by=10)]
print(SS)
mean(SS)
summary(SRS)
summary(SS)
par(mfrow=c(1,2))
hist(SRS)
hist(SS)
par(mfrow=c(1,1))

hist(houses$PRICE)
with(houses, hist(PRICE))
hist(houses$TAX)
plot(houses$PRICE,houses$TAX)
stem(houses$TAX)
par(mfrow=c(1,2))
with(houses, hist(PRICE))
with(houses, hist(TAX))
par(mfrow=c(1,1))
with(houses, hist(PRICE, breaks=c(1300, 1900, 2500, 3100, 3700, 4300, 4900, 5500)))
with(houses, hist(TAX, breaks=c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)))

mileage<-read.csv("mileage.csv")
str(mileage)

mpg_class<-aggregate(MPG~CLASS, mileage, mean)
mpg_class$SD<-aggregate(MPG~CLASS, mileage, sd)[,2]                     
mpg_class

hp_class<-aggregate(HP~CLASS, mileage, mean)
hp_class$SD<-aggregate(HP~CLASS, mileage, sd)[,2]
hp_class

shoppers<-read.csv("shoppers.csv",header = TRUE)
str(shoppers)
range<-function(x) {max(x, na.rm = TRUE) - min(x, na.rm = TRUE)}

summary_stats <- function(x) {
  stats <- data.frame(rbind(mean(x, na.rm = TRUE),
                            median(x, na.rm = TRUE),
                            range(x),
                            sd(x, na.rm = TRUE),
                            var(x, na.rm = TRUE),
                            quantile(x, probs = c(0.25), na.rm = TRUE),
                            quantile(x, probs = c(0.75), na.rm = TRUE),
                            quantile(x, probs = c(0.10), na.rm = TRUE)),
                      row.names = c("Mean", "Median", "Range", "StdDev", "Var",
                                    "Q1", "Q3", "P10"))
  colnames(stats) <- "Value"
  return(stats)
}

summary_stats(shoppers$Spending)

pontus<-read.csv("pontus.csv")
str(pontus)
summary_stats(pontus$Age)

apply(pontus[,5:6],2,summary_stats)

difference <- pontus$Ht - pontus$HtOpp
hist(difference)

with(pontus, boxplot(Ht, HtOpp, names = c("President's Height", "Opponent's Height")))

geyser<-read.csv("geyser.csv")
str(geyser)
apply(geyser, 2, summary)

par(mfrow=c(2,2))
hist(geyser$WEEK1)
hist(geyser$WEEK2)
boxplot(geyser$WEEK1)
boxplot(geyser$WEEK2)
par(mfrow=c(1,1))

shoppers<-read.csv("shoppers.csv")
str(shoppers)

table(shoppers$Spending>=40)
sum(shoppers$Spending>=40)/nrow(shoppers)
sum(shoppers$Spending<=10)/nrow(shoppers)

n<-nrow(shoppers)
n1<-sum(shoppers$Spending>=40)
n2<-sum(shoppers$Spending<=10)

n1*n2/((n*(n-1))/2)

m=sum(((shoppers$Spending>=10) & (shoppers$Spending <= 40)))
m
n
m*(m-1)/(n*(n-1))
successful_combinations <- n1*n2*m*(m-1)/2
total_combinations <- n*(n-1)*(n-2)*(n-3)/(4*3*2*1)
successful_combinations/total_combinations

shoppers_more_than_30<-subset(shoppers, subset = Spending > 30)
sum((shoppers_more_than_30$Spending > 40) == TRUE) /
  nrow(shoppers_more_than_30)

set.seed(1234)
count_duplicates <- 0
for(i in 1:100){
  this_sample <- sample(1:365, size = 22, replace = TRUE)
  if(length(this_sample) != length(unique(this_sample)))
    count_duplicates <- count_duplicates + 1
}
prob_any_duplicates <- count_duplicates/100
prob_any_duplicates

set.seed(1234)
mean(replicate(100,any(duplicated(sample(1:365,22,replace=TRUE)))))

set.seed(1234) # set random number seed for reproducibility
mean(replicate(10000,any(duplicated(sample(1:365, 22, replace=TRUE)))))

count <- function(N,p){
  x <- runif(n = N)
  count <- x <= p
  m <- sum(count)}

set.seed(1234)
result = NULL
for (i in 1:50)
  +(result <- c(result, count(20,0.6)))

result <- result>= 11
sum(result)/50

set.seed(1234)
result = NULL

for(i in 1:10000)
  +(result <- c(result, count(20,0.6)))

outcome<- table(result)
outcome <- data.frame(outcome)

Count_Frequency <- result
hist(Count_Frequency)

sum(result)/10000

summary(result)

result <- result >= 11
sum(result)/10000

trials <- c(0:20)
probabilies<-dbinom(trials,size=20, prob=0.6)
successes<-trials[5:20]
binomial_probabilities<-probabilies[5:20]
successes<-factor(successes)
barplot(binomial_probabilities, names.arg = successes, xlab = "successes", ylab = "binomial probabilities")

relative_frequency<- outcome[,2]/sum(outcome[,2])
successes<-outcome[,1]
successes<-factor(successes)
barplot(relative_frequency, names.arg = successes, xlab = "successes", ylab = "relative frequency")

par(mfrow=c(1,2))
barplot(binomial_probabilities, names.arg = successes, xlab = "successes", ylab = "binomial probabilities")
barplot(relative_frequency, names.arg = successes, xlab = "successes", ylab = "relative frequency")


