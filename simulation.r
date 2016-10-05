#### DAISIE model with mainland speciation and immigration ####
# by Mark Wright
# with Alvaro Rodriguez
remove(list=ls())
set.seed(23)



#### Parameters ####
N <- 0 # number of nonendemic species
E <- 0 # number of endemic species
n <- N + E # number of combined nonendemic and endemic species
M <- 1000 # number of mainland species
t <- -10 # present time is now t=0 and perspective is looking backwards in time = t
gamma <- 0.005 # rate of immigration
lambda.c <- 0.05 # rate of cladogenesis
lambda.a <- 0.05 # rate of anagenesis
mu <- 0.012 # rate of extinction
total.rate <- (M-N)*gamma + n*(lambda.c + mu)+ N*lambda.a  # adjusted per capita rates
# values in parentheses are for scenarios with constant area
# rate of immigration (low = 0.001 (0.1), high = 0.1 (0.3))
# rate of cladogenesis (L = 0.00001, H = 0.00005 DI, 0.001 DD)
# rate of anagenesis (L = 0.1, H = 10)
# rate of extinction (L = 1, H = 10 (3))
richness <- data.frame(matrix(ncol = 4)) # convert to matrix
colnames(richness) <- cbind("t", "N", "E", "n")
row <- 1



#### Functions ####
dt <- as.numeric(rexp(1, rate = total.rate))
t <- t + dt # define the time of the first event



#### Simulation ####
while(t <= 0){
  event <- sample(c("immigration", "cladogenesis_N", "cladogenesis_E", "anagenesis", "extinction_N", "extinction_E"),
                  size = 1, replace = T,  prob = c((M-N)*gamma, N*lambda.c, E*lambda.c, N*lambda.a, N*mu, E*mu))
  if(event == "immigration"){
    N <- N + 1
  } else if (event == "cladogenesis_N"){
    N <- N - 1
    E <- E + 2
  } else if (event == "cladogenesis_E"){
    E <- E + 1
  } else if (event == "anagenesis"){
    N <- N - 1
    E <- E + 1
  } else if (event == "extinction_N"){
    N <- N - 1
  } else if (event == "extinction_E"){
    E <- E - 1
  }
  n <- N + E
  for(i in 1:4){richness[row, i] <- c(t, N, E, n)[i]}
  row <- row + 1
  total.rate <- (M-N)*gamma + n*(lambda.c + mu)+ N*lambda.a  # adjusted per capita rates
  dt <- rexp(1, rate = total.rate)
  t <- t + dt
  }



#### Plot the simulation ####
plot(data = richness, n~t, type = "l", lty = 2,
     xlab = "Time (mya)",
     ylab = "Richness")
lines(data = richness, N~t, col = "blue", lwd = 2)
lines(data = richness, E~t, col = "red", lwd = 2)
legend(x = "topleft",
       legend = c("Total, n", "Nonendemic, N", "Endemic, E"),
       text.col = c("black", "blue", "red"))



########################################## THIS IS NOT YET FUNCTIONAL
#### Replicate the simulation ####
# dataframe of median richness at 5 points (t = 0.2, 0.5, 0.9)
# adjust t so that 1.0 is tend instead of arbitrary time value
# reset necessary parameters so that script lines can run consecutively
median.richness <- data.frame(cbind(0,0,0,0,0,0,0,0,0,0))
colnames(median.richness) <- cbind("Rep", "2N", "2E", "2n", "5N", "5E", "5n", "9N", "9E", "9n")
for(i in 1:100){
  t <- dt
  E <- 0
  N <- 0
  n <- 0
  richness <- data.frame(cbind(0,0,0,0))
  colnames(richness) <- cbind("t", "N", "E", "n")
  row <- 1
  while(t <= 1){event <- sample(c("immigration",
                                "cladogenesis",
                                "anagenesis",
                                "extinction"),
                              size = 1, # try changing to n to see what happens, but make sure n = 1 if so
                              replace = T,
                              prob = c(gamma/total.rate,
                                       lambda.c/total.rate,
                                       lambda.a/total.rate,
                                       mu/total.rate))
  if(event == "immigration"){
    N <- N + 1
  } else if (event == "cladogenesis" & N >= 1){
    N <- N - 1
    E <- E + 2
  } else if (event == "anagenesis" & N >= 1){
    N <- N - 1
    E <- E + 1
  } else if (event == "extinction" & N >= 1 & E >= 1){
    extinction <- sample(c("N", "E"),
                         size = 1,
                         prob = c(0.5,0.5))
    if(extinction == "N"){
      N <- N - 1
    } else if(extinction == "E"){
      E <- E - 1
    }
  }
  n <- N + E
  for(j in 1:4){richness[row, j] <- c(t, N, E, n)[j]}
  row <- row + 1
  dt <- rexp(1, rate = 1/total.rate)
  t <- t + dt
  }
  median.richness[i,1] <- i
  median.richness[i,2] <- richness[which.min(abs(richness$t - 0.2)),2]
  median.richness[i,3] <- richness[which.min(abs(richness$t - 0.2)),3]
  median.richness[i,4] <- richness[which.min(abs(richness$t - 0.2)),4]
  median.richness[i,5] <- richness[which.min(abs(richness$t - 0.5)),2]
  median.richness[i,6] <- richness[which.min(abs(richness$t - 0.5)),3]
  median.richness[i,7] <- richness[which.min(abs(richness$t - 0.5)),4]
  median.richness[i,8] <- richness[which.min(abs(richness$t - 0.9)),2]
  median.richness[i,9] <- richness[which.min(abs(richness$t - 0.9)),3]
  median.richness[i,10] <- richness[which.min(abs(richness$t - 0.9)),4]
}



#### Plot the simulation replicated 1000 times ####
mean.median.richness <- data.frame(matrix(0, ncol = 4, nrow = 3))
colnames(mean.median.richness) <- cbind("Time", "N", "E", "n")
mean.median.richness$Time[1] <- 2
mean.median.richness$N[1] <- mean(median.richness$`2N`)
mean.median.richness$E[1] <- mean(median.richness$`2E`)
mean.median.richness$n[1] <- mean(median.richness$`2n`)
mean.median.richness$Time[2] <- 5
mean.median.richness$N[2] <- mean(median.richness$`5N`)
mean.median.richness$E[2] <- mean(median.richness$`5E`)
mean.median.richness$n[2] <- mean(median.richness$`5n`)
mean.median.richness$Time[3] <- 9
mean.median.richness$N[3] <- mean(median.richness$`9N`)
mean.median.richness$E[3] <- mean(median.richness$`9E`)
mean.median.richness$n[3] <- mean(median.richness$`9n`)



plot(data = mean.median.richness, n~Time, type = "l", lty = 2,
     main = "Species Richness",
     xlab = "Time",
     ylab = "Richness",
     ylim = c(0,15))
lines(data = mean.median.richness, N~Time, col = "blue", lwd = 2)
lines(data = mean.median.richness, E~Time, col = "red", lwd = 2)
legend(x = "topleft",
       legend = c("Total, n", "Nonendemic, N", "Endemic, E"),
       text.col = c("black", "blue", "red"))

#
