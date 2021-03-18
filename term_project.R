library(devtools)
library(randomForest)
library(dplyr)
library(data.table)
library(regtools)
setwd("C:\\Users\\Claire Wong\\Documents\\School\\ECH132")
table = fread("train.csv")
table = filter(table, MISSING_DATA == FALSE)
copy <- table
og <- table
trip_duration = table$POLYLINE

number = nrow(table)
time = vector(length = number)

for (i in 1:number) {
  test = trip_duration[i]
  char = as.list(strsplit(test, "],")[[1]])
  time[i] = length(char)*15 - 15
}

call = subset(table, select = -c(TRIP_ID, ORIGIN_CALL, ORIGIN_STAND, TAXI_ID, TIMESTAMP, DAY_TYPE, POLYLINE))
call = filter(call, MISSING_DATA == FALSE)

#Part 3
time = time/60 #convert to minutes

call$time <- time

mean_time = mean(call[["time"]])
var_time = var(call[['time']])

a = filter(call, CALL_TYPE == 'A')
b = filter(call, CALL_TYPE == 'B')
c = filter(call, CALL_TYPE == 'C')

mean_a = mean(a[["time"]]) #convert to minutes
mean_b = mean(b[["time"]]) #convert to minutes
mean_c = mean(c[["time"]]) #convert to minutes


#Part 1
time_dat = data.frame(time)
time_dat <- filter(time_dat, time != 0)
hist(time_dat$time, main = 'Density Distribution of Trip Time', xlab = 'Time (min)', xlim=c(0, 100), breaks = 1500, prob = TRUE)
mean_time = mean(time_dat[["time"]])
var_time = var(time_dat[['time']])
max(time_dat$time, na.rm = TRUE)

lambda = mean_time/var_time
r = mean_time*lambda
curve(dgamma(x, r, lambda, log = FALSE), 0, 970, add = TRUE, col = "red")
legend(80, .08, legend = c("Gamma Model"), col = c("red"), lty = 1:1, cex = 0.8)

plot(ecdf(time_dat$time), xlim = c(0, 100))
curve(pgamma(x, r, lambda), 0, 970, add = TRUE, col = 'red')

#Part 2
p2 <- subset(copy, select = -c(TRIP_ID, CALL_TYPE, ORIGIN_CALL, ORIGIN_STAND, DAY_TYPE, MISSING_DATA, POLYLINE))
p2$time <- time
p2 <- p2[order(p2$TIMESTAMP),]

#convert hour format to minutes to use in ml
pnext <- subset(p2, select = c(TAXI_ID, TIMESTAMP, time))

taxi_id <- split(pnext, pnext$TAXI_ID)

busy <- vector(length = 448)
not_busy <- vector(length = 448)

as.POSIXct(end, origin = "1970-01-01")

difftime((as.POSIXct(end, origin = "1970-01-01")), (as.POSIXct(start, origin = "1970-01-01")), units = 'mins')
start = p2$TIMESTAMP[1]
end = p2$TIMESTAMP[1000000]

#448 rows
for (i in 1:488) {
  tot_busy <- 0
  tot_notbusy <- 0
  last <- 0
  for (j in taxi_id[i]) {
    rows <- nrow(j)
    for (k in 1:rows) {
      current <- j[k, ]
      if (k == 1) {
        start <- current$TIMESTAMP
      }
      else if (k ==  rows) {
        end <- current$TIMESTAMP
        last <- current$time
      }
      tot_busy <- tot_busy + current$time
    }
  }
  busy[i] <- tot_busy
  not_busy[i] <- as.numeric(difftime(as.POSIXct(end, origin = "1970-01-01"), as.POSIXct(start, origin = "1970-01-01"), units = 'mins')) + last
}

busy
not_busy
prop <- busy/(not_busy)
prop_dat <- data.frame(prop)
prop_dat[is.na(prop_dat)] = 0

maximum_prop = max(prop_dat$prop, na.rm = TRUE)
mean_prop <- mean(prop_dat[["prop"]])
sd_prop <- sd(prop_dat[["prop"]])

mean_prop
sd_prop

prop_dat[is.na(prop_dat)] = 0
 
hist(prop_dat$prop, main = "Histogram of Proportion a Driver is Active", freq = FALSE, breaks = 80, xlab = 'Probability A Taxi Driver is Active')
curve(dnorm(x, mean = mean_prop, sd = sd_prop), 0, 0.224220729583627, add = TRUE, col = "red")

plot(ecdf(prop_dat$prop), main = 'Comparison of ecdf with Predicted cdf', xlab = 'Proportion of active time', ylab = 'F(x)', xlim = c(0, 1))
curve(pnorm(x, mean_prop, sd_prop), 0, 1, add = TRUE, col = 'red')
legend(0.8, 0.2, legend = c("Actual", "pnorm"), col = c("black", "red"), lty = 1:1, cex = 0.8)

#Part 4: Converting TimeStamp into Date/Time
conf_int <- function(point, prediction, actual) {
  z = qgamma(0.025, mean(dtime)^2/var(dtime), mean(dtime)/var(dtime))
  se <- (sum((actual - prediction)^2)/length(actual))^(1/2)
  #95% confidence is z = 1.96
  inter <- z*se
  return(c(point - inter, point + inter))
}
date_ref <- table
date_ref <- subset(date_ref, select = c(TIMESTAMP))
test = as.POSIXct(date_ref$TIMESTAMP, origin = "1970-01-01")
date_ref$DATE <- format(test, format = "%Y-%m-%d")
date_ref$DAY_OF_YEAR <- as.integer(format(test, format = '%j'))
date_ref$HOUR <- as.integer(format(test, format = "%H"))
date_ref$MIN <- as.integer(format(test, format = "%M"))
date_ref$SEC <- as.integer(format(test, format = "%S"))
date_ref <- subset(date_ref, select =- c(TIMESTAMP))
#convert hour format to minutes to use in ml
date_ref$DAY_TIME <- (date_ref$HOUR * 60 + date_ref$MIN + date_ref$SEC/60)/60
date_ref$TIME <- time

#Machine learning functions
#based on time of day
day_time <- subset(date_ref, select = c(DAY_TIME, TIME))
day_time <- data.frame(day_time)

#time of day models

tday_lin <- qeLin(day_time, 'TIME')
tday_m1 <- qePolyLin(day_time, 'TIME')
tday_m2 <- qeKNN(day_time, 'TIME')
tday_m3 <- qeKNN(day_time, 'TIME', 100)
tday_m4 <- qeGBoost(day_time, 'TIME')

dtime <- day_time$DAY_TIME
dtime <- data.frame(dtime)

typeof(dtime)

pre_tday_lin <- predict(tday_lin, day_time)
pre_tday_m1 <- predict(tday_m1, dtime)
pre_tday_m2 <- predict(tday_m2, dtime)
pre_tday_m3 <- predict(tday_m3, dtime)
pre_tday_m4 <- predict(tday_m4, day_time)

pre_var <- c(10, 20, 15)
pre_var$DAY_TIME <- data.frame(pre_var)
p10 <- predict(tday_lin, pre_var)
conf_int(10, pre_tday_lin, dtime)

plot(density(time), xlim = c(0, 30))
lines(density(pre_tday_lin), col = 'yellow')
lines(density(pre_tday_m1), col = 'red')
lines(density(pre_tday_m2), col = 'blue')
lines(density(pre_tday_m3), col = 'green')
lines(density(pre_tday_m4), col = 'orange')
legend(20, .18, legend = c("Actual", "Lin", "PolyLin", "KNN", "KNN with hyperparameter 100", "GeBoost"), col = c("black", "yellow", "red", "blue", "green", 'orange'), lty = 1:1, cex = 0.8)

plot(day_time$DAY_TIME, day_time$TIME)
points(day_time$TIME, pre_tday_lin, col = 'yellow')
points(day_time$TIME, pre_tday_m2, col = 'blue')

#day of the year models
day_year <- subset(date_ref, select = c(DAY_OF_YEAR, TIME))
day_year <- data.frame(day_year)
day_year$DAY_OF_YEAR <- day_year$DAY_OF_YEAR + day_time$DAY_TIME/24
dyear <-data.frame(day_year$DAY_OF_YEAR)

dy_lin <- qeLin(day_year, 'TIME')
dy_m1 <- qePolyLin(day_year, 'TIME')
dy_m2 <- qeKNN(day_year, 'TIME')
dy_m3 <- qeKNN(day_year, 'TIME', 100)
dy_m4 <- qeGBoost(day_year, 'TIME')

pre_dy_lin <- predict(dy_lin, day_year)
pre_dy_m1 <- predict(dy_m1, dyear)
pre_dy_m2 <- predict(dy_m2, dyear)
pre_dy_m3 <- predict(dy_m3, dyear)
pre_dy_m4 <- predict(dy_m4, day_year)

plot(density(time), xlim = c(0, 30))
lines(density(pre_dy_lin), col = 'yellow')
lines(density(pre_dy_m1), col = 'red')
lines(density(pre_dy_m2), col = 'blue')
lines(density(pre_dy_m3), col = 'green')
lines(density(pre_dy_m4), col = 'orange')
legend(20, .18, legend = c("Actual", "Lin", "PolyLin", "KNN", "KNN with hyperparameter 100", "GeBoost"), col = c("black", "yellow", "red", "blue", "green", 'orange'), lty = 1:1, cex = 0.8)

plot(day_year$DAY_OF_YEAR, day_year$TIME)
points(day_year$DAY_OF_YEAR, pre_dy_lin, col = 'yellow')
points(day_year$DAY_OF_YEAR, pre_dy_m2, col = 'blue')

con_int_pred2 <- predict(dy_lin, day_year[1])
conf_int(con_int_pred[1], pre_tday_lin, dyear)


getInter <- function(T) {
  n = length(dtime)
  z = qgamma(0.025, mean(dtime)^2/var(dtime), mean(dtime)/var(dtime))
  se2 = sqrt(sum(dtime - pre_dy_lin)^2/n)
  inter = z*se2
  return(c(T - inter, T + inter))
}

getInter(5)

sqrt(sum(dtime - pre_tday_lin)^2/length(dtime))

# check = 