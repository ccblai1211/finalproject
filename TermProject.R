library('regtools');
library('rjson');
# library('ggplot2')

"/" <- function(x, y) ifelse(y == 0, 0, base:::"/"(x, y))

#Reads the data
readData <- function() {
  allData <- read.csv("train.csv", colClasses = c("TRIP_ID" = "character"))

  # trainData <- allData[1:n,]

  return(allData)
}

#Get the amount of time a trip takes by counting the number of coordinates in Polyline and multiplying by 15
getDur <- function(row) {
  return(length(fromJSON(row[[9]])) * 15 / 60);
}

#
spl <- function(l) {
  df <- data.frame(l)
  l <- list()
  d <- list()

  if (nrow(df) <= 1) {
    return(c(c(getDur(df)), c(0)))
  }

  for (n in 1:(nrow(df) - 1)) {
    # print(getDur(df[n,]))
    l[[n]] <- (df[n + 1, 6] - df[n, 6]) / 60
    d[[n]] <- getDur(df[n,])
  }
  # print(d)
  return(list(d, l))
}

#Calculate the distance
HaversineDistance <- function(lat1, lon1, lat2, lon2) {
  # returns the distance in m
  REarth <- 6371000
  lat <- abs(lat1 - lat2) * pi / 180
  lon <- abs(lon1 - lon2) * pi / 180
  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180
  a <- sin(lat / 2) * sin(lat / 2) + cos(lat1) * cos(lat2) * sin(lon / 2) * sin(lon / 2)
  d <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- REarth * d
  return(d)
}

get_dist <- function(row) {
  lonlat <- fromJSON(row[[9]])
  snapshots <- length(lonlat)
  if (snapshots != 0) {
    start <- lonlat[[1]]
    end <- lonlat[[snapshots]]
    # # d <- sqrt((start[1] - end[1]) ^ 2 + (start[2] - end[2]) ^ 2)
    d <- HaversineDistance(start[[1]], start[[2]], end[[1]], end[[2]])
    return(d)
  }
  else return(0)
}

getInter <- function(x, model, actual) {
  n = nrow(actual)
  prediction = predict(model, subset(actual, select = -c(dur)))
  T = predict(model, data.frame('dist' = x))
  z = qgamma(0.025, mean(actual$dur) ^ 2 / var(actual$dur), mean(actual$dur) / var(actual$dur))
  # z = 1.96
  # se = sqrt(sum((actual$dur - prediction) ^ 2)) / n
  se = sqrt(sum((actual$dur - prediction) ^ 2) / n)
  # se = (1 / n + (T - mean(actual$dist)) ^ 2 / (sum((actual$dist - mean(actual$dist)) ^ 2)))
  inter = z * se
  return(c(T - inter, T + inter))
}

partB <- function() {
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
        current <- j[k,]
        if (k == 1) {
          start <- current$TIMESTAMP
        }
        else if (k == rows) {
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
  prop <- busy / (not_busy)
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
}

partAB <- function() {
  train_split <- split(trainData, trainData$TAXI_ID)

  r <- lapply(train_split, spl)
  d <- list()
  w <- list()
  for (i in r) {
    d <- c(d, i[[1]])
    w <- c(w, i[[2]])
  }
  d <- unlist(d)
  w <- unlist(w)

  m1 <- mean(d)
  v1 <- var(d)
  r1 <- m1 / v1
  s1 <- m1 * m1 / v1

  #plots histogram curve and estimated gamma curves
  h1 <- hist(d, freq = FALSE, breaks = 200, xlim = c(0, 200))
  curve(dgamma(x, s1, r1), 0, max(d), add = TRUE, col = "red")
  curve(dgamma(x, 4, 0.4), 0, max(d), add = TRUE, col = "red")

  p <- d / w

  m2 <- mean(p)
  v2 <- var(p)
  r2 <- m2 / v2
  s2 <- m2 * m2 / v2

  #plots histogram graph and estimated gamma curves
  h2 <- hist(p, freq = FALSE, breaks = 20000, xlim = c(0, 1))
  print(h2)
  curve(dgamma(x, s2, 1 / r2), 0, max(p), add = TRUE, col = "red")

  # partB()
}

partC <- function() {
  callSplit <- split(trainData, trainData$CALL_TYPE)

  i <- 1
  res <- list()
  for (call in callSplit) {
    res[[i]] <- apply(call, 1, getDur);
    # print(res)
    i <- i + 1
  }

  print(paste0('A: mean: ', mean(res[[1]]), 'var: ', var(res[[1]])))
  print(paste0('B: mean: ', mean(res[[2]]), 'var: ', var(res[[2]])))
  print(paste0('C: mean: ', mean(res[[3]]), 'var: ', var(res[[3]])))
}

drawPlot <- function(m1,m2) {
  #machine learning predictions
  lin1 <- qeLin(m1, yName = "dur")
  lin2 <- qeLin(m2, yName = "dur")
  poly1 <- qePolyLin(m1, yName = "dur")
  poly2 <- qePolyLin(m2, yName = "dur")
  knn1 <- qeKNN(m1, yName = "dur", 100)
  knn2 <- qeKNN(m2, yName = "dur", 100)

  pre1 <- predict(poly1, subset(m1, select = -c(dur)))
  pre2 <- predict(poly2, subset(m2, select = -c(dur)))
  pre3 <- predict(knn1, subset(m1, select = -c(dur)))
  pre4 <- predict(knn2, subset(m2, select = -c(dur)))
  pre5 <- predict(lin1, subset(m1, select = -c(dur)))
  pre6 <- predict(lin2, subset(m2, select = -c(dur)))

  par(mfrow = c(1, 2))

  plot(density(m1$dur), xlim = c(0, 30))
  lines(density(pre1), col = "red")
  lines(density(pre2), col = "green")
  lines(density(pre3), col = "cyan")
  lines(density(pre4), col = "purple")
  lines(density(pre5), col = "orange")
  lines(density(pre6), col = "yellow")

  legend(20, 0.08, legend = c("Actual", "Lin1", "Lin2", "PolyLin1", "PolyLin2", "KNN1", "KNN2"),
       col = c("black", "orange", "Yellow", "red", "green", "cyan", "purple"), lty = 1, cex = 0.8)

  plot(m1$dist, m1$dur, xlim = c(0, 10 ^ 5))
  points(m1$dist, pre1, col = "red", cex = 0.1)
  points(m1$dist, pre2, col = "green", cex = 0.1)
  points(m1$dist, pre3, col = "cyan", cex = 0.1)
  points(m1$dist, pre4, col = "purple", cex = 0.1)
  points(m1$dist, pre5, col = "orange", cex = 0.1)
  points(m1$dist, pre6, col = "yellow", cex = 0.1)
}

partD <- function() {
  date_ref <- trainData
  #Convert Unix Timestamp to regular timestamp and extract date, hour, min, sec
  date_ref <- subset(date_ref, select = c(TIMESTAMP))
  test = as.POSIXct(date_ref$TIMESTAMP, origin = "1970-01-01")
  date_ref$DATE <- format(test, format = "%Y-%m-%d")
  #Convert m-d-y to day of year
  date_ref$DAY_OF_YEAR <- as.integer(format(test, format = '%j'))
  date_ref$HOUR <- as.integer(format(test, format = "%H"))
  date_ref$MIN <- as.integer(format(test, format = "%M"))
  date_ref$SEC <- as.integer(format(test, format = "%S"))
  date_ref <- subset(date_ref, select = -c(TIMESTAMP))

  distance <- apply(trainData, 1, get_dist)
  duration <- apply(trainData, 1, getDur)
  speed <- distance / duration
  call <- apply(trainData, 1, function(row) {
    return(as.integer(charToRaw(row[[2]])) - 64)
  })
  #convert hour format to minutes to use in ml
  time <- (date_ref$HOUR * 60 + date_ref$MIN + date_ref$SEC / 60)
  dyear <- data.frame(date_ref$DAY_OF_YEAR + time / 24)

  m1 <- data.frame('dist' = distance, 'dur' = duration)
  m2 <- data.frame('dyear' = dyear, 'speed' = speed, 'dur' = duration)
  m3 <- data.frame('dist' = distance, 'speed' = speed, 'time' = time, 'call' = call, 'dur' = duration)

  drawPlot(m1, m3)
}

trainData <- readData()

partAB()
partC()
partD()
