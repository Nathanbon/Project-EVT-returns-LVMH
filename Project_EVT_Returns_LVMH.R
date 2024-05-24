##########################################################
#IMPORTATION OF LIBRARY AND PACKAGES 
install.packages("quantmod")
install.packages("evir")
install.packages('extRemes')
install.packages("e1071") #for kurtosis
install.packages("extRemes")
library(extRemes)
library(e1071)
library(quantmod)
library(evir)
library(extRemes)

##########################################################
#DOWNLOAD DATA FROM YAHOO FINANCE AND PLOT THE PRICES/RETURNS
getSymbols("MC.PA", src = "yahoo")
prices <- MC.PA$MC.PA.Close
plot(prices, main="LVMH prices from Yahoo-Finance")
returns <- dailyReturn(prices)
plot(returns, main="LVMH daily returns")
hist(returns, breaks = 50, main="Histogram of LVMH returns")

##########################################################
#SELECTION OF NEGATIVE RETURNS AND CONVERT TO ABSOLUE VALUE
negative_returns <- returns[returns < 0]
plot(negative_returns, main="LVMH negative daily returns")
abs_returns <- abs(negative_returns)
plot(abs_returns, main="LVMH negative daily returns in absolute value")

##########################################################
#CALCUL OF SOME STASTISTICS
mean <- mean(returns)
variance <- var(returns)
kurtosis <- kurtosis(returns)
quantile_1 <- quantile(returns, probs = 0.25)
quantile_3 <- quantile(returns, probs = 0.75)
cat("mean value correspond to ", mean, "\n")
cat("the variance correspond to ", variance, "\n")
cat("kurtosis correspond to ", kurtosis, "\n")
cat("quantile à 0,25% =", quantile_1, "\n")
cat("quantile à 0,75% =", quantile_3, "\n")
boxplot(abs_returns, main="LVMH negative daily returns in absolute value")
hist(abs_returns, breaks = 50, main="LVMH negative returns in absolute value")

##########################################################
#BLOC MAXIMA IN 68 BLOCKS
num_blocks <- 68
returns_lengths <- length(returns)
block_size <- as.integer(returns_lengths/num_blocks)
block_maxima <- numeric(num_blocks)

#SELECT THE WORST RETURNS
for (i in 1:num_blocks) {
  start_index <- (i - 1) * block_size + 1
  end_index <- i * block_size
  block_maxima[i] <- min(returns[start_index:end_index])
}
block_maxima =abs(block_maxima)

#PLOT THE BLOCK MAXIMA
plot(block_maxima, main = "Block maxima for trimestrial negative returns", type = "b")
boxplot(block_maxima, main="Block maxima for trimestrial negative returns")

hist(block_maxima, breaks = 15, main="Block maxima for trimestrial negative returns", freq = FALSE)
dens <- density(block_maxima) 
x <- dens$x
lines(x, dens$y, col="blue")

#IDENTIFY THE GEV
gev_block <- gev(block_maxima)
plot(gev_block)
density <- dgev(block_maxima)
plot(density)

#IDENTIFY THE PARAMETERS OF GEV
params <- gev_block$par.ests
hist(block_maxima, breaks = 15, main = "Block maxima for trimestrial negative returns", freq = FALSE)
lines(dgev(x, xi = params[1], mu = params[2], sigma = params[3]), col = "blue", lwd = 2)
hist(block_maxima, freq = FALSE, breaks = 15, main = "Histogram of Maxima", xlab = "Maximum",ylim=c(0,33))

#NOW WE DO THE FIT AND COLLECT THE PARAMETERS (WITH ANOTHER PACKAGES)
Modele_GEV<-gev(block_maxima)
Xi<-Modele_GEV$par.ests[1] 
Sigma<-Modele_GEV$par.ests[2] ## scale
Mu<-Modele_GEV$par.ests[3] ## location parameters
## not exactly means and standard deviation

#PRINT RESULTS
cat("Xi : ",  Xi, " \n")
cat("Mu : ",  Mu, " \n")
cat("Sigma : ",  Sigma, " \n")

#PLOT DENSITY
curve(dgev(x, xi = Xi, mu = Mu, sigma = Sigma),col = "blue", add = TRUE)

#OTHER METHOD
params <- gev_block$par.ests
xi_hat <- params[1]
print(xi_hat)
emplot(block_maxima)
qplot(block_maxima, xi=xi_hat)
fevd <- fevd(block_maxima)
plot(fevd)
params <- fevd$par.models
xi_hat <- params[3]
print(xi_hat)
summary(fevd)

#PARAMETER AND SIMULATED 1000 RAWS ON THIS MODEL
params <- gev_block$par.ests
shape <- params[1]
scale <- params[2]
loc <- params[3]
sim <- rgev(1000, shape, loc, scale)
hist(sim, breaks = 25, main="Simulated samples on the GEV", freq = FALSE, xlim = c(0,0.20))
dens <- density(sim) 
x <- dens$x
lines(x, dens$y, col="blue")

#CALCULATION OF VAR AND ES
p <- 0.95
VaR <- quantile(sim, probs = p)
cat("VaR at", 100*p, "% is:", VaR, "\n")
ES <- mean(sim[sim > VaR])
cat("ES at", 100*p, "% is:", ES, "\n")
hist(sim, breaks = 25, main="Simulated samples and risk measures", freq = FALSE, xlim = c(0,0.15))
dens <- density(sim) 
x <- dens$x
lines(x, dens$y, col="blue")
abline(v = VaR, col = "red")
abline(v = ES, col = "purple")
legend("topright", legend=c("Density", "VaR 95%", "ES 95%"), 
       col=c("blue", "red"), lty=1, cex=1)
##########################################################
#THRESHOLD METHOD
cat(abs_returns)

#RULE OF THUMB
Nb_values <- length(abs_returns)
Nb_90 <- ceiling(0.9 * Nb_values)
Nb_sqrt <- Nb_values - floor(sqrt(Nb_values))
k <- Nb_values - floor(Nb_values^(2/3)/log(log(Nb_values)))
cat("Nb_values :", Nb_values, "\n")
cat("Position quantile 90% :", Nb_90, "\n")
cat("Position racine(n) :", Nb_sqrt, "\n")
cat("Position log :",k, "\n")
Sorted_X <- sort(abs_returns)
Seuil_90    <- Sorted_X[Nb_90]
Seuil_sqrt  <- Sorted_X[Nb_sqrt]
Seuil_log   <- Sorted_X[k]
cat("Seuil quantile 90% :", Seuil_90, "\n")
cat("Seuil racine(n) :", Seuil_sqrt, "\n")
cat("Seuil log :", Seuil_log, "\n")

#MRL PLOT
dev.new()
mrlplot(abs_returns)
dev.new()
mrlplot(abs_returns,u.range=c(5000,10000))

#STABILITY OF PARAMETERS
dev.new()
tcplot(X,u.range=c(1000,20000),nt=200)

#FIND THE GPD MODEL
quantile_90 <- quantile(abs_returns, probs = 0.9)
pot_returns = abs_returns[abs_returns > quantile_90]
hist(pot_returns, breaks = 30, main="Returns over the treshold")
Modele <- gpd(abs_returns, threshold = quantile_90)
print(Modele)

#FIND PARAMETERS OF GPD
Xi <- Modele$par.ests[1]
Beta <- Modele$par.ests[2]
Mu <- quantile_90
cat("Xi :", Xi, "\n")
cat("Beta :", Beta, "\n")
cat("Seuil Log :", quantile_90, "\n")
curve(dgpd(x, xi = Xi, mu = Mu, beta = Beta) ,col = "blue", add = TRUE)
#distribution de pareto à queues lourdes

#SIMULATED 1000 RAWS IN OUR MODEL
sim <- rgev(1000, Xi, Mu, Beta)
hist(sim, breaks = 25, main="Simulated samples on the GPD", freq = FALSE, xlim = c(0,0.20))
dens <- density(sim) 
x <- dens$x
lines(x, dens$y, col="blue")

#CALCULATION OF VAR AND ES 
p <- 0.95
VaR <- quantile(sim, probs = p)
cat("VaR at", 100*p, "% is:", VaR, "\n")
ES <- mean(sim[sim > VaR])
cat("ES at", 100*p, "% is:", ES, "\n")

hist(sim, breaks = 25, main="Simulated samples and risk measures Threshold method", freq = FALSE, xlim = c(0,0.15))
dens <- density(sim) 
x <- dens$x
lines(x, dens$y, col="blue")
abline(v = VaR, col = "red")
abline(v = ES, col = "purple")
legend("topright", legend=c("Density", "VaR 95%", "ES 95%"), 
       col=c("blue", "red"), lty=1, cex=1)





