library(data.table)
library(regtools)

table<- fread("Porto_taxi_data_test_partial_trajectories.csv")


number <- nrow(table)
time = vector(length = number)
trip_duration = table$POLYLINE

for (i in 1:number) {
  test = trip_duration[i]
  char = as.list(strsplit(test, "],")[[1]])
  time[i] = length(char)*15 - 15
}

time <- as.integer(time/60)
print(tim
