cars93 <- read.csv("Desktop/Stats 480/Data/cars93.txt", 
                   sep = " ", 
                   header = T,
                   na.strings = "NA")

cars93$TYPE_GROUP <- NA
cars93$TYPE_GROUP[cars93$TYPE %in% c("Small", "Sporty", "Compact")] <- "small"
cars93$TYPE_GROUP[cars93$TYPE %in% c("Midsize", "Large", "Van")] <- "large"

sample_data$TYPE_GROUP <- NA
sample_data$TYPE_GROUP[sample_data$TYPE %in% c("Small", "Sporty", "Compact")] <- "small"
sample_data$TYPE_GROUP[sample_data$TYPE %in% c("Midsize", "Large", "Van")] <- "large"


#Select a random sample of cars from this population. Estimate the average city miles
# per gallon (mpg) for these cars, with a bound on the error of estimation.

set.seed(1234)
sample_indices <- sample(1:93, size=50, replace=FALSE)
sample_data <- cars93[sample_indices, ] #selecting random sample

#Create variables for values
N <- length(cars93$MPGCITY)
n <- length(sample_data$MPGCITY)
sampleyhat <- sum(sample_data$MPGCITY)/n
samplevar <- var(sample_data$MPGCITY)

varybar <- (1 - n/N) * (samplevar/n)


#Bound on the error

sampleB <- 2*sqrt(varybar)

#Estimate the proportion of these cars that have at least one air bag, with a bound on
#the error of estimation.

airbag1 <- sample_data$AIRBAGS >= 1

prop_airbag <- sum(airbag1)/n

#bound on error of estimation

var_airbag <- ((1 - n/N) * ((prop_airbag * (1-prop_airbag)) / (n-1)))

B_airbag <- 2*sqrt(var_airbag)

#Using the data from part (a), poststratify on the car type and then estimate the aver-
#age city mpg by this method.


population_counts <- table(sample_data$TYPE_GROUP)
population_proportions <- population_counts / sum(population_counts)

population_proportions


type_counts <- table(cars93$TYPE_GROUP)

result1 <- stmean(dataset = sample_data, yvar = "MPGCITY",
                 stvar = "TYPE_GROUP", popsizes = as.numeric(type_counts))
result1


#Using the data from part (b), poststratify on car type and then estimate the proportion of cars that have at least one 
#air bag by this method. Use stmean again but adjust for proportionality.

sample_data$AIRBAG <- as.numeric(sample_data$AIRBAGS >= 1)

result2 <- stmean(dataset = sample_data, yvar = "AIRBAG", stvar = "TYPE_GROUP", popsizes = as.numeric(type_counts))

result2










