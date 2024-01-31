# Lab 6
# Developing statistics formulas

# upload data set
library(ggplot2)
data(mpg)
head(mpg)

# Calculate the mean of a data set
myMean <- function(dataSet){
  # store the number of elements
  numOfPop <- length(dataSet)
  sum <- 0
  
  # loop through each element adding to the sum
  for (data in dataSet){
    sum <- sum + data
  }
  
  # return the mean
  return(sum / numOfPop)
}

# Calculate the median of a dataset
myMedian <- function(dataSet){
  # order the data and store the length
  orderedData <- sort(c(dataSet), decreasing = FALSE, na.last = NA)
  numOfPop <- length(orderedData)
  median <- 0
  
  # check if its an even or odd data set
  if (numOfPop %% 2 == 0){
    # calculate median if even
    median <- ((orderedData[numOfPop/2] + orderedData[(numOfPop/2) + 1])/2)
  } else {
    # calculate if odd
    median <- orderedData[(numOfPop+1)/2]
  }
  
  # return the median
  return(median)
}

# Calculate the mode of the dataset
myMode <- function(dataSet){
  # order the dataset, create a vector for counting and loop variable
  orderedData<- sort(c(dataSet), decreasing = FALSE, na.last = NA)
  numOfOccurrences <- vector(mode = "numeric", length = length(orderedData))
  i <- 1
  j <- 1
  
  # loop through data set counting repeating numbers
  for (data in orderedData){
    # check if it is repeating
    if (i > 1 && data == orderedData[j]){
      # add to count
      numOfOccurrences[j] <- numOfOccurrences[j] + 1
    } else {
      # reset data we are checking
      j <- i
    }
    # increase i
    i <- i + 1
  }
  
  # return the mode(s) if exist
  if (max(numOfOccurrences) == 0){
    print("There are no modes")
  } else {
    return(orderedData[which(numOfOccurrences == max(numOfOccurrences))])
  }
}

# Calculate the standard deviation of a data set given the mean, 0 for population 1 for sample
myStaDev <- function(dataSet){
  dividend <- 0
  mean <- myMean(dataSet)
  
  # loop to calculate the dividend
  for(data in dataSet){
    dividend <- dividend + ((data - mean) * (data - mean))
  }
  
  # divide deviation of population and sample
  return(c(sqrt(dividend/(length(dataSet)-(1))),sqrt(dividend/(length(dataSet)))))
}

myVar <- function(staDev){
  # return standard deviation squared
  return(staDev * staDev)
}


# find the mean and compare with built in function
myMean(mpg$cty)
mean(mpg[[8]])

# find the median and compare with built in function
myMedian(mpg[[8]])
median(mpg[[8]])

# find the mode
myMode(mpg[[8]])

# find the standard deviation and compare with built in function
myStaDev(mpg[[8]])
sd(mpg[[8]])

# find variation
myVar(myStaDev(mpg[[8]]))

# create a data frame per lab specs
BMI <- data.frame(
  gender = c("Male", "Male", "Female", "Male", "Female", "Female"),
  height = c(81,93,78,100,92,75),
  weight = c(152,171.5,165,140,192.1,180.2),
  Age = c(42,38,26,52,18,23)
)
# print data frame
print(BMI)

# print data frame information
print(paste("Mean of height is", myMean(BMI[[2]])), quote = FALSE)
print(paste("Mean of weight is", myMean(BMI[[3]])), quote = FALSE)
print(paste("Mean of age is", myMean(BMI[[4]])), quote = FALSE)

print(paste("Standard Deviation of height is", myStaDev(BMI[[2]])), quote = FALSE)
print(paste("Standard Deviation of weight is", myStaDev(BMI[[3]])), quote = FALSE)
print(paste("Standard Deviation of age is", myStaDev(BMI[[4]])), quote = FALSE)

print("The probability that height is less than 85 will have a z-score of -0.15 which is 44.04% according to the table", quote = FALSE)
print(paste("Using pnorm", (pnorm(85, mean = 86.5, sd = 9.894443) * 100), "%"), quote = FALSE)

print("The probability that weight is more than 166 will have a z-score of -0.042 which is 51.6% (100 - 48.4) according to the table", quote = FALSE)
print(paste("Using pnorm", (pnorm(166, mean = 166.8, sd = 18.8794, lower.tail = FALSE) * 100), "%"), quote = FALSE)

print("The probability that the age is between 25 and 45 will have a z-scores of -0.63 and 0.91 which is 55.43% (81.86 - 26.43) according to the table", quote = FALSE)
print(paste("Using pnorm", ((pnorm(45, mean = 33.16667, sd = 12.96791) - pnorm(25, mean = 33.16667, sd = 12.96791)) * 100), "%"), quote = FALSE)


