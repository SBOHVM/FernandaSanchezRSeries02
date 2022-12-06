#' ---
#' title: "Simple SIS model, changing timesteps manually"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 

#' Load in the functions that do the work
library(RPiR)
source("0201-step-SIS.R")

#' We are going to compare four Susceptible-Infected-Susceptible (SIS) models for E. coli O157 in cattle, with different week timesteps (1, 0.1, 3 and 5). 
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-\beta \times \frac{S(t)\times I(t)}{N} +\sigma \times I(t)$$
#'
#' 2. Infected model
#'
#'    $$I(t + 1) = S(t)+\beta \times \frac{S(t)\times I(t)}{N}-\sigma \times I(t)$$
#'
#' 3. N is a constant for total population
#'
#'    $$N = S(t)+ I(t)$$
#'
#'

#' The same procedure will be done for simulations 1, 2, 3 and 4. 
#' We only change the timesteps. 
#' We used the currently stated transmission and recovery rates for E. coli O157, 1 and 1/3, respectively.
#'
#'

#' Set up the simulation parameters, which will be the same for all simulations.

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.susceptibles <- num.cattle-initial.infecteds

ecoli.transmission<-1
ecoli.recovery<-1/3

#R0 for all simulations
R0<-ecoli.transmission/ecoli.recovery
R0

#Inverse R0
1/R0

# Start and end time of timesteps
start.time <- 0
end.time <- 100


#' ## Timestep of 1 week
#' 
#' 
# Data frame for cattle population
herd.df.1 <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)


#' Setting the timestep to 1. 
#' Inside the sequence we have to add the timestep to the start time and then increment it by the same timestep. 

timestep1<-1

timesteps1 <- seq(from = start.time + timestep1, to = end.time, by=timestep1)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 1).

for (new.time in timesteps1) {
  # Calculate population at next timestep
  next.population.1 <- step_deterministic_SIS(latest = tail(herd.df.1, 1), 
                                              transmission.rate = (ecoli.transmission*timestep1),
                                              recovery.rate = (ecoli.recovery*timestep1))
  # Bind herd and next population
  herd.df.1 <- rbind(herd.df.1, next.population.1)
}


#' **Plot the results for simulation 1.** 
#' 
#' It can be seen that the model shows to be reliable with a timestep of 1 and shows no instabilities. 
herd.df.1$time <- c(start.time, timesteps1)
plot_populations(herd.df.1,col = c("green", "red"))

#' ## Timestep of 0.1 weeks
#' 
#' 

# Data frame for cattle population
herd.df.2 <- data.frame(susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds)

#' Setting the timestep to 0.1. 
#' We use the same sequence as the first simulation, we only changed the timestep. 

timestep2<-0.1
timesteps2 <- seq(from = start.time +timestep2, to = end.time, by=timestep2)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 0.1).
#
for (new.time in timesteps2) {
  # Calculate population at next timestep
  next.population.2 <- step_deterministic_SIS(latest = tail(herd.df.2, 1), 
                                              transmission.rate = (ecoli.transmission*timestep2),
                                              recovery.rate = (ecoli.recovery*timestep2))
  # Bind herd and next population
  herd.df.2 <- rbind(herd.df.2, next.population.2)
}


#' **Plot the results for simulation 2.**
#' 
#' 
#' We can see a similar plot as for simulation 1.
#' 

herd.df.2$time <- c(start.time, timesteps2)
plot_populations(herd.df.2,col = c("green", "red"))

#' **Plots for simulation 1 and 2 together to see differences**
#' 
#' 
#' The plot between a timestep of 1 and 0.1 are not significantly different, which tells us that there is no advantage to reducing the timestep lower than 1. 
#'

herd.df.1$time <- c(start.time, timesteps1)
plot_populations(herd.df.1,col = c("green", "red"), with.legend = FALSE)
herd.df.2$time <- c(start.time, timesteps2)
plot_populations(herd.df.2,col = c("blue", "orange"), new.graph=FALSE, with.legend=FALSE, lty=2)

legend("topright", legend = c("susceptible", "infected", "susceptible", "infected" ),
       col = c("green", "red", "blue", "orange"), lty = c(1, 1, 2,2))


#' ## Timestep of 3 weeks
#' 
#' 
# Data frame for cattle population
herd.df.3 <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#' Setting the timestep to 3. 
#' We use the same sequence as the first simulation, we only changed the timestep. 

timestep3<-3
timesteps3 <- seq(from = start.time + timestep3, to = end.time, by=timestep3)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 3).
#
for (new.time in timesteps3) {
  # Calculate population at next timestep
  next.population.3 <- step_deterministic_SIS(latest = tail(herd.df.3, 1), 
                                              transmission.rate = (ecoli.transmission*timestep3),
                                              recovery.rate = (ecoli.recovery*timestep3))
  # Bind herd and next population
  herd.df.3<- rbind(herd.df.3, next.population.3)
}


#' **Plot the results for simulation 3.**
#' 
#' 
#' The plot looks unstable and less accurate.

herd.df.3$time <- c(start.time, timesteps3)
plot_populations(herd.df.3,col = c("green", "red"))


#' **Plots for simulation 1 and 3 together to see differences**
#' 
#' 
#' It can be seen that with a higher timestep, the model becomes unstable and the simulation is not reliable.
#' It shows erratic curves before the equilibrium, and the are sharp spikes after the equilibrium.
#' This model is not accurate, therefore we shouldn't use timesteps that are higher than 1.

herd.df.1$time <- c(start.time, timesteps1)
plot_populations(herd.df.1,col = c("green", "red"), with.legend = FALSE)
herd.df.3$time <- c(start.time, timesteps3)
plot_populations(herd.df.3,col = c("blue", "orange"), new.graph=FALSE, lty=2)

legend("topright", legend = c("susceptible", "infected", "susceptible", "infected" ),
       col = c("green", "red", "blue", "orange"), lty = c(1, 1, 2,2))



#' ## Timestep of 5 weeks
#' 
#'  
# Data frame for cattle population
herd.df.4 <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#' Setting the timestep to 5. 
#' We use the same sequence as the first simulation, we only changed the timestep. 

timestep4<-5
timesteps4 <- seq(from = start.time + timestep4, to = end.time, by=timestep4)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 5).
#
for (new.time in timesteps4) {
  # Calculate population at next timestep
  next.population.4 <- step_deterministic_SIS(latest = tail(herd.df.4, 1), 
                                              transmission.rate = ecoli.transmission*timestep4,
                                              recovery.rate = ecoli.recovery*timestep4)
  #  Bind herd and next population
  herd.df.4 <- rbind(herd.df.4, next.population.4)
}

#' **Plot the results for simulation 4.**
#' 
#' With a higher timestep the model becomes more unstable

herd.df.4$time <- c(start.time, timesteps4)
plot_populations(herd.df.4,col = c("green", "red"), ylim=c(0,100))

#' **Plots for simulation 1 and 4 together to see differences**
#' 
#' 
#' It can be seen that with an even higher timestep the model is highly unstable. 
#' The simulation ends earlier than expected, that's why we need to specify a y axis limit in the plot to be able to print it.
#' Proving once again that the higher the timestep, the more unreliable and less accurate the model is. 


herd.df.1$time <- c(start.time, timesteps1)
plot_populations(herd.df.1,col = c("green", "red"), with.legend = FALSE)
herd.df.4$time <- c(start.time, timesteps4)
plot_populations(herd.df.4,col = c("blue", "orange"), ylim=c(0,100), new.graph=FALSE, lty=2)

legend("topright", legend = c("susceptible", "infected", "susceptible", "infected" ),
       col = c("green", "red", "blue", "orange"), lty = c(1, 1, 2,2))


