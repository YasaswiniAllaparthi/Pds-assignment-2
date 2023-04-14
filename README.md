# Pds-assignment-2
#PartA
# load data
diabetes <- read.csv("diabetes.csv")

# set seed
set.seed(123)

# take random sample of 25 observations
sample_data <- diabetes[sample(nrow(diabetes), 25, replace=FALSE), ]

# calculate mean and highest glucose values of sample
mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)

# calculate mean and highest glucose values of population
mean_glucose_pop <- mean(diabetes$Glucose)
max_glucose_pop <- max(diabetes$Glucose)

# create bar chart to compare mean glucose values
mean_values <- c(mean_glucose_pop, mean_glucose)
names(mean_values) <- c("Population", "Sample")
barplot(mean_values, main="Comparison of Mean Glucose Values", ylab="Mean Glucose",col="blue")

# create bar chart to compare highest glucose values
max_values <- c(max_glucose_pop, max_glucose)
names(max_values) <- c("Population", "Sample")
barplot(max_values, main="Comparison of Highest Glucose Values", ylab="Highest Glucose",col="violet")

#PartB
# calculate 98th percentile of BMI for sample and population
BMI_98_sample <- quantile(sample_data$BMI, probs=0.98)
BMI_98_pop <- quantile(diabetes$BMI, probs=0.98)

# create boxplot to compare BMI distributions
boxplot(diabetes$BMI, sample_data$BMI, names=c("Population", "Sample"), 
        main="Comparison of BMI Distributions", ylab="BMI", col="green")

#PartC

# bootstrap sampling
library(boot)
set.seed(123)
n_samples <- 500
sample_size <- 152
boot_samples <- boot(diabetes$BloodPressure, function(data, idx) {
  mean_bp <- mean(data[idx])
  sd_bp <- sd(data[idx])
  perc_bp <- quantile(data[idx], probs=0.96)
  return(c(mean_bp, sd_bp, perc_bp))
}, R=n_samples, strata=diabetes$Outcome, sim="ordinary")

# extract bootstrap statistics
boot_means <- boot_samples$t[,1]
boot_sds <- boot_samples$t[,2]
boot_percs <- boot_samples$t[,3]

# calculate population statistics
mean_bp_pop <- mean(diabetes$BloodPressure)
sd_bp_pop <- sd(diabetes$BloodPressure)
perc_bp_pop <- quantile(diabetes$BloodPressure, probs=0.96)

# create histograms to compare distributions of bootstrap statistics and population statistics
par(mfrow=c(1,1))
hist(boot_means, main=" Bootstrap mean", xlab="Mean BloodPressure",col="pink")
abline(v=mean_bp_pop, col="red")
hist(boot_sds, main="Bootstrap Standard Deviations", xlab="Standard",col="orange")
