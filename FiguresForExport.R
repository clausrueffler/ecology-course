

library(deSolve)

predpreyLV <- function(t, y, params) {
  R <- y[1]
  C <- y[2]
  with(as.list(params), {
    dR.dt <- r * R - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC.dt)))
  })
}

# In order to be able to analyze the dynamics described by these equations, we have to choose values for the parameters. You can change these values at any time.

r <- 2
a <- 0.5
e <- 0.5
d <- 0.35

# Before we can start to simulate the dynamics we have to additionally specify the initial population densities of the prey and predator population. Also these values can be changed at any time.

R0 <- 2
C0 <- 6

# Now we are ready simulate the dynamics for the above set of parameters. The following code produces a plot with time on the x-axis and the resource and consumer density on the y-axis. The solid line shows the density of resources and the hatched line the density of consumers.

params1 <- c(r = r, a = a, e = e, d = d)
MaxTime <- 60 # length of the time series, can be changed at any time
Time <- seq(0, MaxTime, by = 0.1) # here we produce a sequence of time points at which we will determine the population densities
LV.out <- ode(c(R0, C0), Time, predpreyLV, params1) # here we call the function "ode" that does the hard work of numerical integration and which is part of the package "deSolve"

matplot(Time, (LV.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)

# Let us run (and plot) the same model as above two more times, but with somewhat different starting densities — everything else is the same. 

R02 <- 2
C02 <- 5
LV.out2 <- ode(c(R02, C02), Time, predpreyLV, params1)
matplot(Time, (LV.out2[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)

R03 <- 2
C03 <- 4
LV.out3 <- ode(c(R03, C03), Time, predpreyLV, params1)
matplot(Time, (LV.out3[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Density", xlab="Time", ylim = c(0,8))
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n", horiz = TRUE)

# What do you observe? How do the different starting conditions affect the dynamics?
# Instead of looking at these simulations as time series, we can also look at them in a phase-plane diagram.

plot(LV.out[, 2], LV.out[, 3], type = "l",  lty = 1, col = "red", ylab = "consumer", xlab = "resource", ylim = c(0, 7))
points(R0, C0, cex = 1.5, pch = 19, col = "red") # adds a dot to show the starting density
lines(LV.out2[, 2], LV.out2[, 3], lty = 1, col = "blue") # adds the second simulation to the plot
points(R02, C02, cex = 1.5 ,pch = 19, col = "blue") # adds a dot to show the starting density
lines(LV.out3[, 2], LV.out3[, 3], lty = 1, col = "green") # adds the third simulation to the plot
points(R03, C03, cex = 1.5, pch = 19, col = "green") # adds a dot to show the starting density

# legend("topright", c(expression("simulation 1"), expression("simulation 2"), expression("simulation 3")), lty = 1, col = c("red","blue","green"), bty = "n")

abline(h = r/a, lty = 2, col = "red") # iso-cline for the resource, which is a horizontal line
abline(v = d/(e * a), lty = 2, col = "blue") # iso-cline for the consumer, which is a vertical line

#### With logistic resource growth

predprey.log.prey <- function(t, y, p) {
  R <- y[1]
  C <- y[2]
  with(as.list(p), {
    dR.dt <- r * R * (1 - R / k) - a * R * C
    dC.dt <- e * a * R * C - d * C
    return(list(c(dR.dt, dC.dt)))
  })
}

# To be able to compare the dynamics of this new model we choose the same parameters as above to which we have to add a value for the one new parameter, the resource carrying capacity *k*.

r <- 2
e <- 0.5
a <- 0.5
d <- 0.35
k <- 10

R0 <- 2
C0 <- 6

#Let us run (and plot) the model.

params.log.prey1 <- c(a = a, r = r, d = d, e = e, k = k)
MaxTime <- 50
Time <- seq(0, MaxTime, by = 0.2)
log.prey.out <- ode(c(R0, C0), Time, predprey.log.prey, params.log.prey1)

matplot(Time, (log.prey.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Size", xlab="Time")
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n")

# Again, let's make a phase plane plot. In which direction does the time series “travel” along the line?

plot(log.prey.out[, 2], log.prey.out[, 3], type = "l",  lty = 1, col = 1, ylab = "consumer", xlab = "resource", ylim = c(0, 7))
abline(a = r/a, b = -r/(a*k), lty = 2, col = "red") # resource iso-cline, a straight line with negative slope
abline(v = d / (e * a), lty = 2, col = "blue") # consumer iso-cline, a vertical line

