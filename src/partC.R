# getting required libraries for bootstrapping
library(boot)

#setting seed as 5432
set.seed(123)

# number of samples to be created
num_samples <- 500

#number of observations considered 
sample_size <- 150

#Function to calculate mean, standard deviation, and percentile of sample
boot_samples <- boot(diabetes$BloodPressure, function(data, idx) {
  mean_boot <- mean(data[idx])
  sd_boot <- sd(data[idx])
  perc_boot <- quantile(data[idx], probs=0.95)
  return(c(mean_boot, sd_boot, perc_boot))
}, R=num_samples, strata=diabetes$Outcome, sim="ordinary")

#extracting bootstrap statistic values of the bootstrap sample
mean1ofboot <- boot_samples$t[,1]
mean_bootmean <- mean(meanofboot)

boot1stdev <- boot_samples$t[,2]
mean_bootstdev <- mean(bootstdev)

boot2ofquantile <- boot_samples$t[,3]
mean_bootquantile <- mean(bootofquantile)

#calculating bootstrap statistic values of the population
mean_of_BPpop <- mean(diabetes$BloodPressure)
sd_of_BPpop <- sd(diabetes$BloodPressure)
quan_of_BPpop <- quantile(diabetes$BloodPressure, probs=0.95)

#plotting histograms to compare 
quan_compare <- c(Population_quantile=quan_of_BPpop,Sample_quantile=mean_bootquantile)
mean_compare <- c(Population_mean=mean_bp_pop,Sample_mean=mean_bootmean)
stdev_compare <- c(Population_stdev=sd_of_BPpop,Sample_stdev=mean_bootstdev)



barplot(quan_compare, col = c("skyblue","yellow"), space=1,
        main="Comparision of quantile at 95% of population and bootstrap sample of Blood Pressure",xlab="Group",ylab="quantile value")

barplot(mean_compare, col = c("gold","blue"), space=1,
        main="Comparision of mean of population and bootstrap sample of Blood Pressure",xlab="Group",ylab="mean value")

barplot(stdev_compare, col = c("pink","black"), space=1,
        main="Comparision of standard deviation of population and bootstrap sample of Blood Pressure",xlab="Group",ylab="standard deviation value")

