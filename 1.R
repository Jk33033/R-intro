x <- 1:20
sin(x)
plot(x, sin(x))
plot(x, sin(x), type = "l" )
?seq


x <- seq(0, 20, by = 0.1)
plot(x, sin(x), type = "l" )
# easier expression of sin curve 
curve(sin(x), 0, 2 * pi) 
curve(cos(x), 0, 2 * pi, add = TRUE, col = "red" ) 

## todo brownian motion / random walk
x  <- 0
## todo 100 iterations
for (i in 1:100) {
  if(runif(1) > 0.5){
    x <- x + 1
  }else{
    x <- x -1
  }
}
x

## easier expression of random walk
runif(100) < 0.5
round(runif(100))
cumsum(round(runif(100)) * 2 -1 )
set.seed(42)

h <- c(174, 170, 160)
w <- c(90, 80, 70)

cor(w,h)


## not hiphen, childa below
fit = lm(w~h)
str(fit)
summary(fit)
predict(fit, newdata = list(h = 165))
predict(fit, newdata = list(h = 52))
plot(h,w)

abline(fit, col = "red")

df <- data.frame(weight = w, height = h)
df
str(df)
df$weight
df$weight[1]

plot(df)
cor(df)

df$bmi <- df$weight / (df$height/100) ^ 2
df <- read.csv("http://bit.ly/CEU-R-heights")
## TODO compute BMI
df$weight <- df$weightLb * 0.45
df$height <- df$heightIn * 2.54
df$weightLb <- df$heightIn <- NULL
df$bmi <- df$weight / (df$height/100) ^ 2

library(pairsD3)
pairsD3::pairsD3(df)
library(GGally)
ggpairs(df)
library(ggplot2)
g = ggplot(df, aes(x = height, y = weight, color = sex)) + geom_point()

g + theme_bw()
g + geom_smooth(method = "lm", se = FALSE)


ggplot(df, aes(x = height, y = weight)) + 
  geom_point(aes(color = sex)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  geom_smooth(aes(color = sex), method = "lm", se = FALSE)
    
g + scale_y_log10()
ggplot(df, aes(x = height)) + geom_boxplot() 
ggplot(df, aes(sex, height)) + geom_boxplot() +
  geom_violin(alpha =  .5) +
  geom_jitter()

ggplot(df, aes(x = height, fill = sex)) +
  geom_density(alpha = .25)+
  theme_bw() + 
  ggtitle("height of boys and girls") + 
  xlab("height") + ylab("") + 
  theme(legend.position = "top")

theme_bw()

## TODO bar chart on the number of f/m
ggplot(df, aes(sex)) + geom_bar()
## TODO histogram of weight
ggplot(df, aes(weight)) + geom_histogram()
## TODO histogram of weight split by sex
ggplot(df, aes(weight)) + geom_histogram() + facet_wrap(~sex)
## TODO bar chart on the number of f/m above and below 160
df$height_cat <- cut(df$height, breaks = c(0, 160, Inf))
ggplot(df, aes(sex)) + geom_bar() + facet_wrap(~height_cat)

ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = "fill")
ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = "dodge")

## TODO avg weight per gender
mean(df$weight)
mean(df[df$sex == "f", "weight"])
mean(df[df$sex == "m", "weight"])

aggregate(weight ~sex, FUN = mean, data = df)

library(data.table)
dt <- data.table(df)
str(df)

dt[sex == "f"][1:5]
str(dt[ageYear == min(ageYear)][order(bmi)])

## dt[i. j]
dt[, mean(height)]
dt[, hist(height)]
dt[, summary(height)]
d[sex == "m", mesn(height)]


## dt[i, j, by = ...]
dt[, mean(height), by = sex]
## V1 = variavle 1


dt[, list(height = mean(height), weight = mean(weight)),
   by = list(gender = sex, height_cat)]

## new variable: elementary schoool = age < 14
dt$school_cat <- cut(dt$ageYear, breaks = c(0, 14, 20))
## compute the median for the elementary VS high school 20 > age >= 14
dt[, median(height), b = school_cat]
## draw 5 randam students from dt

set.seed(100)
dt[sample(1:.N, 5)]

round(runif(5, min = 1, max = 237))
dt[round(runif(5, min = 1, max = .N))]

booking <- fread("http://bit.ly/CEU-R-hotels-2018-prices")

## TODO count the number of bookings below 100 EUR
booking[price < 100, .N]
## TODO count the number of bookings below 100 EUR without an offer
booking[offer == 0][price < 100, .N]
## TODO avg price of bookings below 100 EUR
booking[price < 100, mean(price)]



## TODO avg price of bookings on weekdays
## TODO avg price of bookings on weekends
booking[, mean(price), by = list(weekend, nnights)]
## TODO include nnights, holiday and year in the aggregate variables
booking[, mean(price), by = .(weekend, nnights, holiday)]
## TODO compute the average price per number of stars
feature <- fread("http://bit.ly/CEU-R-hotels-2018-features")
feature[hotel_id ==2]

dt =merge(booking, feature, all = TRUE)
dt[, mean(price), by = stars][order(~stars)]


