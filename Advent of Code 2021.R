# Day 1, Puzzle 1

## Count the number of times a depth measurement increases from the previous measurement.

data <- read.table("Advent of Code 1.txt")
solutions <- rep(0, 50)
counter <- 0

for (i in (1:(length(data$V1)-1))) {
  if (data$V1[i] < data$V1[i+1]) {
    counter <- counter + 1
  }
}

solutions[1] <- counter

# Day 1, Puzzle 2

## Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?

counter <- 0

for (i in (1:(length(data$V1)-2))) {
  if (sum(data$V1[i:(i+2)]) < sum(data$V1[(i+1):(i+3)])) {
    counter <- counter + 1
  }
}

solutions[2] <- counter

# Day 2, Puzzle 1

## What do you get if you multiply your final horizontal position by your final depth?

data <- read.table("Advent of Code 2.txt")
position <- c(0, 0)
counter <- 0

for (i in (1:length(data$V1))) {
  if (data$V1[i] == "forward") {
    position[1] <- position[1] + data$V2[i]
  } else {
    if (data$V1[i] == "down") {
      position[2] <- position[2] + data$V2[i]
    } else {
      position[2] <- position[2] - data$V2[i]
    }
  }
}

solutions[3] <- position[1] * position[2]

# Day 2, Puzzle 2

## ... What do you get if you multiply your final horizontal position by your final depth?

position <- c(0, 0, 0)
counter <- 0

for (i in (1:length(data$V1))) {
  if (data$V1[i] == "forward") {
    position[1] <- position[1] + data$V2[i]
    position[2] <- position[2] + position[3] * data$V2[i]
  } else {
    if (data$V1[i] == "down") {
      position[3] <- position[3] + data$V2[i]
    } else {
      position[3] <- position[3] - data$V2[i]
    }
  }
}

solutions[4] <- position[1] * position[2]

# Day 3, Puzzle 1

## What is the power consumption of the submarine?

data <- read.table("Advent of Code 3.txt")
counter <- c(1:12)*0 # n or 1's per bit position
gamma <- 0
epsilon <- 0

for (i in 12:1) {
  for (j in 1:length(data$V1)) {
    if (data$V1[j] - 10^(i - 1) >= 0) {
      counter[i] <- counter[i] + 1
      data$V1[j] <- data$V1[j] - 10^(i - 1)
    }
  }
  if (counter[i] > 500) {
    counter[i] <- 1
  } else {
    counter[i] <- 0
  }
}

for (i in 12:1) {
  gamma <- gamma + 2^(i - 1) * counter[i]
}

for (i in 1:12) {
  if (counter[i] == 1) {
    counter[i] <- 0
  } else {
    counter[i] <- 1
  }
}

for (i in 12:1) {
  epsilon <- epsilon + 2^(i - 1) * counter[i]
}
  
solutions[5] <- gamma * epsilon

# Day 3, exercise 2

data <- read.table("Advent of Code 3.txt")
OGR <- 0
COSR <- 0
counter <- c(1:12)*0

for (i in 12:1) {
  for (j in 1:length(data$V1)) {
    if (data$V1[j] >= 10^(i - 1)) {
      counter[i] <- counter[i] + 1
    }
  }
  if (counter[i] >= 0.5 * length(data$V1)) {
    counter[i] <- 1
  } else {
    counter[i] <- 0
  }
  if (length(data$V1) > 1) {
    if (counter[i] == 1) {
      data <- subset(data, V1 >= 10^(i - 1))
    } else {
      data <- subset(data, V1 < 10^(i - 1))
    }
  }
  for (j in 1:length(data$V1)) {
    if (data$V1[j] >= 10^(i - 1)) {
      data$V1[j] <- data$V1[j] - 10^(i - 1)
    }
  }
}

print(data) # 806
data <- read.table("Advent of Code 3.txt")

OGR <- strtoi(data$V1[806], 2)

counter <- c(1:12)*0

for (i in 12:1) {
  for (j in 1:length(data$V1)) {
    if (data$V1[j] >= 10^(i - 1)) {
      counter[i] <- counter[i] + 1
    }
  }
  if (counter[i] >= 0.5 * length(data$V1)) {
    counter[i] <- 1 # wenn 1en häufiger oder gleich häufig wie 0en sind
  } else {
    counter[i] <- 0 # wenn 0en häufiger sind
  }
  if (length(data$V1) > 1) {
    if (counter[i] == 1) {
      data <- subset(data, V1 < 10^(i - 1)) # behalte werte mit führender 0
    } else {
      data <- subset(data, V1 >= 10^(i - 1)) # behalte werte mit führender 1
    }
  }
  for (j in 1:length(data$V1)) {
    if (data$V1[j] >= 10^(i - 1)) {
      data$V1[j] <- data$V1[j] - 10^(i - 1)
    }
  }
}

print(data) # 884
data <- read.table("Advent of Code 3.txt")

COSR <- strtoi(data$V1[884], 2)

solutions[6] <- OGR * COSR

# Day 4, Task 1

data_sequence <- read.csv("Advent of Code 4.txt", nrows=1, header = FALSE)
data_sequence <- t(data_sequence)
data_boards <- read.csv("Advent of Code 4.txt", skip=1, sep = "", header = FALSE)
counter <- 0
winning_number <- 0
continue <- TRUE
winning_board <- 0

for (i in 1:100) {
  if (continue == TRUE) {
    for (j in 1:500) {
      for (k in 1:5) {
        if (data_boards[j, k] == data_sequence[i]) {
          data_boards[j, k] <- "x"
        }
      }
    }
    for (j in 1:500) {
      if (data_boards[j, 1] == "x" & data_boards[j, 2] == "x" & data_boards[j, 3] == "x" & data_boards[j, 4] == "x" & data_boards[j, 5] == "x") {
        winning_board <- (j + 4) %/% 5
        winning_number <- i
        continue <- FALSE
      }
    }
    for (j in 1:496) {
      for (k in 1:5) {
        if (data_boards[j, k] == "x" & data_boards[j + 1, k] == "x" & data_boards[j + 2, k] == "x" & data_boards[j + 3, k] == "x" & data_boards[j + 4, k] == "x" & j %% 5 == 1) {
          winning_board <- (j + 4) %/% 5
          winning_number <- i
          continue <- FALSE
        }
      }
    }
  }
}

print(winning_board)
print(data_boards[(winning_board * 5 - 4):(winning_board * 5), ])

for (j in (winning_board * 5 - 4):(winning_board * 5)) {
  for (k in 1:5) {
    if (data_boards[j, k] != "x") {
      counter <- counter + as.numeric(data_boards[j, k])
    }
  }
}

solutions[7] <- counter * data_sequence[winning_number]

# Day 4, Task 2

data_sequence <- read.csv("Advent of Code 4.txt", nrows=1, header = FALSE)
data_sequence <- t(data_sequence)
data_boards <- read.csv("Advent of Code 4.txt", skip=1, sep = "", header = FALSE)
winning_board <- rep(0,100)

for (i in 1:100) {
  for (j in 1:500) {
    for (k in 1:5) {
      if (data_boards[j, k] == data_sequence[i]) {
        data_boards[j, k] <- "x"
      }
    }
  }
  for (j in 1:500) {
    if (data_boards[j, 1] == "x" & data_boards[j, 2] == "x" & data_boards[j, 3] == "x" & data_boards[j, 4] == "x" & data_boards[j, 5] == "x" & winning_board[(j + 4) %/% 5] == 0) {
      winning_board[(j + 4) %/% 5] <- i
    }
  }
  for (j in 1:496) {
    for (k in 1:5) {
      if (data_boards[j, k] == "x" & data_boards[j + 1, k] == "x" & data_boards[j + 2, k] == "x" & data_boards[j + 3, k] == "x" & data_boards[j + 4, k] == "x" & j %% 5 == 1 & winning_board[(j + 4) %/% 5] == 0) {
        winning_board[(j + 4) %/% 5] <- i
      }
    }
  }
}

print(winning_board)

winning_board <- which(winning_board == max(winning_board)) # i = 84 creates the last win with the drawing of number data_sequence[i] = 56 on board 46

data_boards <- read.csv("Advent of Code 4.txt", skip=1, sep = "", header = FALSE)

for (i in 1:84) {
  for (j in (winning_board * 5 - 4):(winning_board * 5)) {
    for (k in 1:5) {
      if (data_boards[j, k] == data_sequence[i]) {
        data_boards[j, k] <- 0
      }
    }
  }
}

solutions[8] <- sum(data_boards[(winning_board * 5 - 4):(winning_board * 5),]) * 56

# Day 5, Task 1

## Data cleaning

data <- read.csv("Advent of Code 5.txt", header = FALSE)
data$V2 <- gsub(" -> ", ',', data$V2)
names(data)[1] <- "x1"
names(data)[3] <- "y2"
library(dplyr)
library(stringr)

for (i in 1:length(data$V2)) {
  data$y1[i] <- str_split(data$V2[i], ",", simplify = TRUE)[,1]
  data$x2[i] <- str_split(data$V2[i], ",", simplify = TRUE)[,2]
}

data$V2 <- NULL
data <- data[, c(1, 4, 3, 2)]
data <- transform(data, x1 = as.numeric(x1))
data <- transform(data, x2 = as.numeric(x2))
data <- transform(data, y1 = as.numeric(y1))
data <- transform(data, y2 = as.numeric(y2))

## Filter for orthogonal lines

data_vertical <- subset(data, x1 == x2)
data_horizontal <- subset(data, y1 == y2)

## Create and fill matrix

map <- matrix(0, nrow = 1000, ncol = 1000)
for (i in 1:length(data_vertical$x1)) {
  for (j in data_vertical$y1[i]:data_vertical$y2[i]) {
    map[j, data_vertical$x1[i]] <- map[j, data_vertical$x1[i]] + 1
  }
}
for (i in 1:length(data_horizontal$y1)) {
  for (j in data_horizontal$x1[i]:data_horizontal$x2[i]) {
    map[data_horizontal$y1[i], j] <- map[data_horizontal$y1[i], j] + 1
  }
}

solutions[9] <- sum(map[map == "2"]/2) + sum(map[map == "3"]/3) + sum(map[map == "4"]/4)

# Day 5, Task 2

data_diagonal <- subset(data, x1 != x2 & y1 != y2)

for (i in 1:length(data_diagonal$x1)) {
  if (data_diagonal$x1[i] <= data_diagonal$x2[i]) {
    data_diagonal$x_dir[i] <- 1
  } else {
    data_diagonal$x_dir[i] <- -1
  }
  if (data_diagonal$y1[i] <= data_diagonal$y2[i]) {
    data_diagonal$y_dir[i] <- 1
  } else {
    data_diagonal$y_dir[i] <- -1
  }
}

for (i in 1:length(data_diagonal$x1)) {
  for (j in 1:length(data_diagonal$x1[i]:data_diagonal$x2[i])) {
    map[data_diagonal$y1[i] + (j - 1) * data_diagonal$y_dir[i], data_diagonal$x1[i] + (j - 1) * data_diagonal$x_dir[i]] <- map[data_diagonal$y1[i] + (j - 1) * data_diagonal$y_dir[i], data_diagonal$x1[i] + (j - 1) * data_diagonal$x_dir[i]] + 1
  }
}

solutions[10] <- sum(map[map == "2"]/2) + sum(map[map == "3"]/3) + sum(map[map == "4"]/4) + sum(map[map == "5"]/5) + sum(map[map == "6"]/6)

