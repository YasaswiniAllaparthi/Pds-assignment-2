# bootstrap sampling
library(boot)
set.seed(123)
n_samples <- 500
sample_size <- 150
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
