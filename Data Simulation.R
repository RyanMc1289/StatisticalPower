# Simulating Experiments #

set.seed(03212015) #for reproducing exact results (if you do NOT set this, your results will be somewhat different!)
library(tidyverse) #for plotting eventual results (use "install.packages("tidyverse")" first if you're new to R)
library(psych) #for describing p-value and effect size distributions (use "install.packages("psych")" first if new to R)
options(scipen = 99) #turns off scientific notation
n_sims <- 100000 #number of simulated experiments (higher numbers take longer to simulate)

# NO EFFECT (Mean Diff = 0; Cohen's d = 0) #

## N = 10 ##

p_noeffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n10 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n10 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 10, mean = 60, sd = 25) #produce 10 simulated participants
  y<-rnorm(n = 10, mean = 60, sd = 25) #produce 10 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n10[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n10[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n10[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 50 ##

p_noeffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n50 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n50 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 60, sd = 25) #produce 50 simulated participants
  y<-rnorm(n = 50, mean = 60, sd = 25) #produce 50 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n50[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n50[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n50[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 100 ##

p_noeffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n100 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n100 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 100, mean = 60, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n100[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n100[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n100[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 200 ##

p_noeffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n200 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n200 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 200, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 200, mean = 60, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n200[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n200[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n200[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 500 ##

p_noeffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n500 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n500 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 500, mean = 60, sd = 25) #produce 500 simulated participants
  y<-rnorm(n = 500, mean = 60, sd = 25) #produce 500 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n500[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n500[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n500[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 1,000 ##

p_noeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_noeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated estimates
d_noeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 1000, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 1000, mean = 60, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_noeffect_n1000[i]<-z$p.value #get the p-value and store it
  diff_noeffect_n1000[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_noeffect_n1000[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## Create datasets for plotting ##

data_noeffect_n10 <- data.frame(p_noeffect_n10)
data_noeffect_n10$n <- rep("N = 10 per group", nrow(data_noeffect_n10))
data_noeffect_n10$p <- data_noeffect_n10$p_noeffect_n10
data_noeffect_n10$diff <- diff_noeffect_n10
data_noeffect_n10$d <- d_noeffect_n10
data_noeffect_n10 <- data_noeffect_n10[, -1]

data_noeffect_n50 <- data.frame(p_noeffect_n50)
data_noeffect_n50$n <- rep("N = 50 per group", nrow(data_noeffect_n50))
data_noeffect_n50$p <- data_noeffect_n50$p_noeffect_n50
data_noeffect_n50$diff <- diff_noeffect_n50
data_noeffect_n50$d <- d_noeffect_n50
data_noeffect_n50 <- data_noeffect_n50[, -1]

data_noeffect_n100 <- data.frame(p_noeffect_n100)
data_noeffect_n100$n <- rep("N = 100 per group", nrow(data_noeffect_n100))
data_noeffect_n100$p <- data_noeffect_n100$p_noeffect_n100
data_noeffect_n100$diff <- diff_noeffect_n100
data_noeffect_n100$d <- d_noeffect_n100
data_noeffect_n100 <- data_noeffect_n100[, -1]

data_noeffect_n200 <- data.frame(p_noeffect_n200)
data_noeffect_n200$n <- rep("N = 200 per group", nrow(data_noeffect_n200))
data_noeffect_n200$p <- data_noeffect_n200$p_noeffect_n200
data_noeffect_n200$diff <- diff_noeffect_n200
data_noeffect_n200$d <- d_noeffect_n200
data_noeffect_n200 <- data_noeffect_n200[, -1]

data_noeffect_n500 <- data.frame(p_noeffect_n500)
data_noeffect_n500$n <- rep("N = 500 per group", nrow(data_noeffect_n500))
data_noeffect_n500$p <- data_noeffect_n500$p_noeffect_n500
data_noeffect_n500$diff <- diff_noeffect_n500
data_noeffect_n500$d <- d_noeffect_n500
data_noeffect_n500 <- data_noeffect_n500[, -1]

data_noeffect_n1000 <- data.frame(p_noeffect_n1000)
data_noeffect_n1000$n <- rep("N = 1000 per group", nrow(data_noeffect_n1000))
data_noeffect_n1000$p <- data_noeffect_n1000$p_noeffect_n1000
data_noeffect_n1000$diff <- diff_noeffect_n1000
data_noeffect_n1000$d <- d_noeffect_n1000
data_noeffect_n1000 <- data_noeffect_n1000[, -1]

alldata_noeffect <- rbind(data_noeffect_n10, data_noeffect_n50, data_noeffect_n100, data_noeffect_n200, data_noeffect_n500, data_noeffect_n1000)

alldata_noeffect$n <- as.factor(alldata_noeffect$n)
alldata_noeffect$n <- ordered(alldata_noeffect$n, levels = c("N = 10 per group", "N = 50 per group", "N = 100 per group",
                                                             "N = 200 per group", "N = 500 per group", "N = 1000 per group"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_noeffect <- ggplot(alldata_noeffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10), 
        axis.text.y = element_text(color = "black", size = 10)))

print(diff_plot_noeffect <- ggplot(alldata_noeffect, aes(x=diff))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0, color = "green") +
        xlab("Observed Mean Difference") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10), 
        axis.text.y = element_text(color = "black", size = 10)))

print(d_plot_noeffect <- ggplot(alldata_noeffect, aes(x=d))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0, color = "green") +
        xlab("Observed Cohen's d") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(color = "black", size = 10), 
        axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed effect sizes #
power_noeffect_n10 <- alldata_noeffect %>%
  filter(n == "N = 10 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n10$diff)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_noeffect_n10$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_noeffect_n50 <- alldata_noeffect %>%
  filter(n == "N = 50 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n50$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_noeffect_n50$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_noeffect_n100 <- alldata_noeffect %>%
  filter(n == "N = 100 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n100$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_noeffect_n100$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_noeffect_n200 <- alldata_noeffect %>%
  filter(n == "N = 200 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n200$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_noeffect_n200$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_noeffect_n500 <- alldata_noeffect %>%
  filter(n == "N = 500 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n500$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_noeffect_n500$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_noeffect_n1000 <- alldata_noeffect %>%
  filter(n == "N = 1000 per group") %>%
  filter(p <= 0.05)
describe(power_noeffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_noeffect_n1000$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_noeffect_n1000$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05


# SMALL EFFECT (Mean Diff = -5; Cohen's d = -0.20) #

## N = 10 ##

p_smalleffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n10 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n10 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 10, mean = 60, sd = 25) #produce 10 simulated participants
  y<-rnorm(n = 10, mean = 65, sd = 25) #produce 10 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n10[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n10[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n10[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 50 ##

p_smalleffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n50 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n50 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 60, sd = 25) #produce 50 simulated participants
  y<-rnorm(n = 50, mean = 65, sd = 25) #produce 50 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n50[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n50[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n50[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 100 ##

p_smalleffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n100 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n100 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 100, mean = 65, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n100[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n100[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n100[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 200 ##

p_smalleffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n200 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n200 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 200, mean = 60, sd = 25) #produce 200 simulated participants
  y<-rnorm(n = 200, mean = 65, sd = 25) #produce 200 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n200[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n200[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n200[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 500 ##

p_smalleffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n500 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n500 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 500, mean = 60, sd = 25) #produce 500 simulated participants
  y<-rnorm(n = 500, mean = 65, sd = 25) #produce 500 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n500[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n500[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n500[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 1,000 ##

p_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all simulated estimates
d_smalleffect_n1000 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 1000, mean = 60, sd = 25) #produce 1000 simulated participants
  y<-rnorm(n = 1000, mean = 65, sd = 25) #produce 1000 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_smalleffect_n1000[i]<-z$p.value #get the p-value and store it
  diff_smalleffect_n1000[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_smalleffect_n1000[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## Create datasets for plotting ##

data_smalleffect_n10 <- data.frame(p_smalleffect_n10)
data_smalleffect_n10$n <- rep("N = 10 per group", nrow(data_smalleffect_n10))
data_smalleffect_n10$p <- data_smalleffect_n10$p_smalleffect_n10
data_smalleffect_n10$diff <- diff_smalleffect_n10
data_smalleffect_n10$d <- d_smalleffect_n10
data_smalleffect_n10 <- data_smalleffect_n10[, -1]

data_smalleffect_n50 <- data.frame(p_smalleffect_n50)
data_smalleffect_n50$n <- rep("N = 50 per group", nrow(data_smalleffect_n50))
data_smalleffect_n50$p <- data_smalleffect_n50$p_smalleffect_n50
data_smalleffect_n50$diff <- diff_smalleffect_n50
data_smalleffect_n50$d <- d_smalleffect_n50
data_smalleffect_n50 <- data_smalleffect_n50[, -1]

data_smalleffect_n100 <- data.frame(p_smalleffect_n100)
data_smalleffect_n100$n <- rep("N = 100 per group", nrow(data_smalleffect_n100))
data_smalleffect_n100$p <- data_smalleffect_n100$p_smalleffect_n100
data_smalleffect_n100$diff <- diff_smalleffect_n100
data_smalleffect_n100$d <- d_smalleffect_n100
data_smalleffect_n100 <- data_smalleffect_n100[, -1]

data_smalleffect_n200 <- data.frame(p_smalleffect_n200)
data_smalleffect_n200$n <- rep("N = 200 per group", nrow(data_smalleffect_n200))
data_smalleffect_n200$p <- data_smalleffect_n200$p_smalleffect_n200
data_smalleffect_n200$diff <- diff_smalleffect_n200
data_smalleffect_n200$d <- d_smalleffect_n200
data_smalleffect_n200 <- data_smalleffect_n200[, -1]

data_smalleffect_n500 <- data.frame(p_smalleffect_n500)
data_smalleffect_n500$n <- rep("N = 500 per group", nrow(data_smalleffect_n500))
data_smalleffect_n500$p <- data_smalleffect_n500$p_smalleffect_n500
data_smalleffect_n500$diff <- diff_smalleffect_n500
data_smalleffect_n500$d <- d_smalleffect_n500
data_smalleffect_n500 <- data_smalleffect_n500[, -1]

data_smalleffect_n1000 <- data.frame(p_smalleffect_n1000)
data_smalleffect_n1000$n <- rep("N = 1000 per group", nrow(data_smalleffect_n1000))
data_smalleffect_n1000$p <- data_smalleffect_n1000$p_smalleffect_n1000
data_smalleffect_n1000$diff <- diff_smalleffect_n1000
data_smalleffect_n1000$d <- d_smalleffect_n1000
data_smalleffect_n1000 <- data_smalleffect_n1000[, -1]

alldata_smalleffect <- rbind(data_smalleffect_n10, data_smalleffect_n50, data_smalleffect_n100, data_smalleffect_n200, data_smalleffect_n500, data_smalleffect_n1000)

alldata_smalleffect$n <- as.factor(alldata_smalleffect$n)
alldata_smalleffect$n <- ordered(alldata_smalleffect$n, levels = c("N = 10 per group", "N = 50 per group", "N = 100 per group",
                                                             "N = 200 per group", "N = 500 per group", "N = 1000 per group"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(diff_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=diff))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -5, color = "green") +
        xlab("Observed Mean Difference") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(d_plot_smalleffect <- ggplot(alldata_smalleffect, aes(x=d))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -.2, color = "green") +
        xlab("Observed Cohen's d") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed effect sizes #
power_smalleffect_n10 <- alldata_smalleffect %>%
  filter(n == "N = 10 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n10$diff)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_smalleffect_n10$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_smalleffect_n50 <- alldata_smalleffect %>%
  filter(n == "N = 50 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n50$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_smalleffect_n50$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_smalleffect_n100 <- alldata_smalleffect %>%
  filter(n == "N = 100 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n100$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_smalleffect_n100$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_smalleffect_n200 <- alldata_smalleffect %>%
  filter(n == "N = 200 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n200$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_smalleffect_n200$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_smalleffect_n500 <- alldata_smalleffect %>%
  filter(n == "N = 500 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n500$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_smalleffect_n500$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_smalleffect_n1000 <- alldata_smalleffect %>%
  filter(n == "N = 1000 per group") %>%
  filter(p <= 0.05)
describe(power_smalleffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_smalleffect_n1000$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_smalleffect_n1000$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05


# MEDIUM EFFECT (Mean Diff = -12.5; Cohen's d = -0.50) #

## N = 10 ##

p_mediumeffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n10 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n10 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 10, mean = 60, sd = 25) #produce 10 simulated participants
  y<-rnorm(n = 10, mean = 72.5, sd = 25) #produce 10 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n10[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n10[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n10[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 50 ##

p_mediumeffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n50 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n50 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 60, sd = 25) #produce 50 simulated participants
  y<-rnorm(n = 50, mean = 72.5, sd = 25) #produce 50 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n50[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n50[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n50[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 100 ##

p_mediumeffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n100 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n100 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 100, mean = 72.5, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n100[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n100[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n100[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 200 ##

p_mediumeffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n200 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n200 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 200, mean = 60, sd = 25) #produce 200 simulated participants
  y<-rnorm(n = 200, mean = 72.5, sd = 25) #produce 200 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n200[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n200[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n200[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 500 ##

p_mediumeffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n500 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n500 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 500, mean = 60, sd = 25) #produce 500 simulated participants
  y<-rnorm(n = 500, mean = 72.5, sd = 25) #produce 500 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n500[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n500[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n500[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 1,000 ##

p_mediumeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_mediumeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated estimates
d_mediumeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 1000, mean = 60, sd = 25) #produce 1000 simulated participants
  y<-rnorm(n = 1000, mean = 72.5, sd = 25) #produce 1000 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_mediumeffect_n1000[i]<-z$p.value #get the p-value and store it
  diff_mediumeffect_n1000[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_mediumeffect_n1000[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## Create datasets for plotting ##

data_mediumeffect_n10 <- data.frame(p_mediumeffect_n10)
data_mediumeffect_n10$n <- rep("N = 10 per group", nrow(data_mediumeffect_n10))
data_mediumeffect_n10$p <- data_mediumeffect_n10$p_mediumeffect_n10
data_mediumeffect_n10$diff <- diff_mediumeffect_n10
data_mediumeffect_n10$d <- d_mediumeffect_n10
data_mediumeffect_n10 <- data_mediumeffect_n10[, -1]

data_mediumeffect_n50 <- data.frame(p_mediumeffect_n50)
data_mediumeffect_n50$n <- rep("N = 50 per group", nrow(data_mediumeffect_n50))
data_mediumeffect_n50$p <- data_mediumeffect_n50$p_mediumeffect_n50
data_mediumeffect_n50$diff <- diff_mediumeffect_n50
data_mediumeffect_n50$d <- d_mediumeffect_n50
data_mediumeffect_n50 <- data_mediumeffect_n50[, -1]

data_mediumeffect_n100 <- data.frame(p_mediumeffect_n100)
data_mediumeffect_n100$n <- rep("N = 100 per group", nrow(data_mediumeffect_n100))
data_mediumeffect_n100$p <- data_mediumeffect_n100$p_mediumeffect_n100
data_mediumeffect_n100$diff <- diff_mediumeffect_n100
data_mediumeffect_n100$d <- d_mediumeffect_n100
data_mediumeffect_n100 <- data_mediumeffect_n100[, -1]

data_mediumeffect_n200 <- data.frame(p_mediumeffect_n200)
data_mediumeffect_n200$n <- rep("N = 200 per group", nrow(data_mediumeffect_n200))
data_mediumeffect_n200$p <- data_mediumeffect_n200$p_mediumeffect_n200
data_mediumeffect_n200$diff <- diff_mediumeffect_n200
data_mediumeffect_n200$d <- d_mediumeffect_n200
data_mediumeffect_n200 <- data_mediumeffect_n200[, -1]

data_mediumeffect_n500 <- data.frame(p_mediumeffect_n500)
data_mediumeffect_n500$n <- rep("N = 500 per group", nrow(data_mediumeffect_n500))
data_mediumeffect_n500$p <- data_mediumeffect_n500$p_mediumeffect_n500
data_mediumeffect_n500$diff <- diff_mediumeffect_n500
data_mediumeffect_n500$d <- d_mediumeffect_n500
data_mediumeffect_n500 <- data_mediumeffect_n500[, -1]

data_mediumeffect_n1000 <- data.frame(p_mediumeffect_n1000)
data_mediumeffect_n1000$n <- rep("N = 1000 per group", nrow(data_mediumeffect_n1000))
data_mediumeffect_n1000$p <- data_mediumeffect_n1000$p_mediumeffect_n1000
data_mediumeffect_n1000$diff <- diff_mediumeffect_n1000
data_mediumeffect_n1000$d <- d_mediumeffect_n1000
data_mediumeffect_n1000 <- data_mediumeffect_n1000[, -1]

alldata_mediumeffect <- rbind(data_mediumeffect_n10, data_mediumeffect_n50, data_mediumeffect_n100, data_mediumeffect_n200, data_mediumeffect_n500, data_mediumeffect_n1000)

alldata_mediumeffect$n <- as.factor(alldata_mediumeffect$n)
alldata_mediumeffect$n <- ordered(alldata_mediumeffect$n, levels = c("N = 10 per group", "N = 50 per group", "N = 100 per group",
                                                                   "N = 200 per group", "N = 500 per group", "N = 1000 per group"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_mediumeffect <- ggplot(alldata_mediumeffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(diff_plot_mediumeffect <- ggplot(alldata_mediumeffect, aes(x=diff))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -12.5, color = "green") +
        xlab("Observed Mean Difference") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(d_plot_mediumeffect <- ggplot(alldata_mediumeffect, aes(x=d))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -.5, color = "green") +
        xlab("Observed Cohen's d") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed effect sizes #
power_mediumeffect_n10 <- alldata_mediumeffect %>%
  filter(n == "N = 10 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n10$diff)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_mediumeffect_n10$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_mediumeffect_n50 <- alldata_mediumeffect %>%
  filter(n == "N = 50 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n50$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_mediumeffect_n50$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_mediumeffect_n100 <- alldata_mediumeffect %>%
  filter(n == "N = 100 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n100$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_mediumeffect_n100$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_mediumeffect_n200 <- alldata_mediumeffect %>%
  filter(n == "N = 200 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n200$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_mediumeffect_n200$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_mediumeffect_n500 <- alldata_mediumeffect %>%
  filter(n == "N = 500 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n500$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_mediumeffect_n500$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_mediumeffect_n1000 <- alldata_mediumeffect %>%
  filter(n == "N = 1000 per group") %>%
  filter(p <= 0.05)
describe(power_mediumeffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_mediumeffect_n1000$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_mediumeffect_n1000$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05


# LARGE EFFECT (Mean Diff = -20; Cohen's d = -0.80) #

## N = 10 ##

p_largeeffect_n10 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n10 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n10 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 10, mean = 60, sd = 25) #produce 10 simulated participants
  y<-rnorm(n = 10, mean = 80, sd = 25) #produce 10 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n10[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n10[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n10[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 50 ##

p_largeeffect_n50 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n50 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n50 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 50, mean = 60, sd = 25) #produce 50 simulated participants
  y<-rnorm(n = 50, mean = 80, sd = 25) #produce 50 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n50[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n50[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n50[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 100 ##

p_largeeffect_n100 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n100 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n100 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 60, sd = 25) #produce 100 simulated participants
  y<-rnorm(n = 100, mean = 80, sd = 25) #produce 100 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n100[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n100[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n100[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 200 ##

p_largeeffect_n200 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n200 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n200 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 200, mean = 60, sd = 25) #produce 200 simulated participants
  y<-rnorm(n = 200, mean = 80, sd = 25) #produce 200 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n200[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n200[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n200[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 500 ##

p_largeeffect_n500 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n500 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n500 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 500, mean = 60, sd = 25) #produce 500 simulated participants
  y<-rnorm(n = 500, mean = 80, sd = 25) #produce 500 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n500[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n500[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n500[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## N = 1,000 ##

p_largeeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated p-values
diff_largeeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated estimates
d_largeeffect_n1000 <- numeric(n_sims) #set up empty container for all simulated Cohen's d estimates

for(i in 1:n_sims){ #for each simulated experiment
  x<-rnorm(n = 1000, mean = 60, sd = 25) #produce 1000 simulated participants
  y<-rnorm(n = 1000, mean = 80, sd = 25) #produce 1000 simulated participants
  z<-t.test(x, y, var.equal = TRUE) #perform student's t-test
  p_largeeffect_n1000[i]<-z$p.value #get the p-value and store it
  diff_largeeffect_n1000[i]<-(z$estimate[1]-z$estimate[2]) #get the estimated mean diff and store it
  d_largeeffect_n1000[i]<-((mean(x)-mean(y)) / sqrt(((sd(x)^2+sd(y)^2))/2)) #get Cohen's d estimate and store it
}

## Create datasets for plotting ##

data_largeeffect_n10 <- data.frame(p_largeeffect_n10)
data_largeeffect_n10$n <- rep("N = 10 per group", nrow(data_largeeffect_n10))
data_largeeffect_n10$p <- data_largeeffect_n10$p_largeeffect_n10
data_largeeffect_n10$diff <- diff_largeeffect_n10
data_largeeffect_n10$d <- d_largeeffect_n10
data_largeeffect_n10 <- data_largeeffect_n10[, -1]

data_largeeffect_n50 <- data.frame(p_largeeffect_n50)
data_largeeffect_n50$n <- rep("N = 50 per group", nrow(data_largeeffect_n50))
data_largeeffect_n50$p <- data_largeeffect_n50$p_largeeffect_n50
data_largeeffect_n50$diff <- diff_largeeffect_n50
data_largeeffect_n50$d <- d_largeeffect_n50
data_largeeffect_n50 <- data_largeeffect_n50[, -1]

data_largeeffect_n100 <- data.frame(p_largeeffect_n100)
data_largeeffect_n100$n <- rep("N = 100 per group", nrow(data_largeeffect_n100))
data_largeeffect_n100$p <- data_largeeffect_n100$p_largeeffect_n100
data_largeeffect_n100$diff <- diff_largeeffect_n100
data_largeeffect_n100$d <- d_largeeffect_n100
data_largeeffect_n100 <- data_largeeffect_n100[, -1]

data_largeeffect_n200 <- data.frame(p_largeeffect_n200)
data_largeeffect_n200$n <- rep("N = 200 per group", nrow(data_largeeffect_n200))
data_largeeffect_n200$p <- data_largeeffect_n200$p_largeeffect_n200
data_largeeffect_n200$diff <- diff_largeeffect_n200
data_largeeffect_n200$d <- d_largeeffect_n200
data_largeeffect_n200 <- data_largeeffect_n200[, -1]

data_largeeffect_n500 <- data.frame(p_largeeffect_n500)
data_largeeffect_n500$n <- rep("N = 500 per group", nrow(data_largeeffect_n500))
data_largeeffect_n500$p <- data_largeeffect_n500$p_largeeffect_n500
data_largeeffect_n500$diff <- diff_largeeffect_n500
data_largeeffect_n500$d <- d_largeeffect_n500
data_largeeffect_n500 <- data_largeeffect_n500[, -1]

data_largeeffect_n1000 <- data.frame(p_largeeffect_n1000)
data_largeeffect_n1000$n <- rep("N = 1000 per group", nrow(data_largeeffect_n1000))
data_largeeffect_n1000$p <- data_largeeffect_n1000$p_largeeffect_n1000
data_largeeffect_n1000$diff <- diff_largeeffect_n1000
data_largeeffect_n1000$d <- d_largeeffect_n1000
data_largeeffect_n1000 <- data_largeeffect_n1000[, -1]

alldata_largeeffect <- rbind(data_largeeffect_n10, data_largeeffect_n50, data_largeeffect_n100, data_largeeffect_n200, data_largeeffect_n500, data_largeeffect_n1000)

alldata_largeeffect$n <- as.factor(alldata_largeeffect$n)
alldata_largeeffect$n <- ordered(alldata_largeeffect$n, levels = c("N = 10 per group", "N = 50 per group", "N = 100 per group",
                                                                     "N = 200 per group", "N = 500 per group", "N = 1000 per group"))

## Plotting P-values and Effect Sizes ##
print(pvalue_plot_largeeffect <- ggplot(alldata_largeeffect, aes(x=p))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = 0.05, color = "red") +
        xlab("Observed p-value") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(diff_plot_largeeffect <- ggplot(alldata_largeeffect, aes(x=diff))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -20, color = "green") +
        xlab("Observed Mean Difference") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

print(d_plot_largeeffect <- ggplot(alldata_largeeffect, aes(x=d))+
        geom_histogram(bins = 100) +
        geom_vline(xintercept = -.8, color = "green") +
        xlab("Observed Cohen's d") +
        ylab("Frequency") +
        theme_classic() +
        facet_wrap(~n) +
        theme(axis.title.x = element_text(size = 12), 
              axis.title.y = element_text(size = 12),
              axis.text.x = element_text(color = "black", size = 10), 
              axis.text.y = element_text(color = "black", size = 10)))

# Count p-values <= 0.05 to determine statistical power / Check average observed effect sizes #
power_largeeffect_n10 <- alldata_largeeffect %>%
  filter(n == "N = 10 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n10$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n10$diff)) # gives descriptive info about estimated differences for p-values <= 0.05
describe(abs(power_largeeffect_n10$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_largeeffect_n50 <- alldata_largeeffect %>%
  filter(n == "N = 50 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n50$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n50$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_largeeffect_n50$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_largeeffect_n100 <- alldata_largeeffect %>%
  filter(n == "N = 100 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n100$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n100$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_largeeffect_n100$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_largeeffect_n200 <- alldata_largeeffect %>%
  filter(n == "N = 200 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n200$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n200$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_largeeffect_n200$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_largeeffect_n500 <- alldata_largeeffect %>%
  filter(n == "N = 500 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n500$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n500$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_largeeffect_n500$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05

power_largeeffect_n1000 <- alldata_largeeffect %>%
  filter(n == "N = 1000 per group") %>%
  filter(p <= 0.05)
describe(power_largeeffect_n1000$p) # gives descriptive info about p-values <= 0.05
describe(abs(power_largeeffect_n1000$diff)) # gives descriptive info about Cohen's d's (absolute value) for p-values <= 0.05
describe(abs(power_largeeffect_n1000$d)) # gives descriptive info about Cohen's d's for p-values <= 0.05
