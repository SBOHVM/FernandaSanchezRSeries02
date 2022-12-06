#' ---
#' title: "Simple SIS model, with timestep included"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 

#' Load in the functions that do the work
library(RPiR)
source("0203-deterministic-SIS.R")
source("0201-step-SIS.R")

#' We are going to compare Susceptible-Infected-Susceptible (SIS) models for E. coli O157 in cattle, 
#' to see if the outcome of putting the timestep inside the function is the same as adding timestep later. 
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-\beta \times \frac{S(t)\times I(t)}{N}+\sigma \times I(t)$$
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

#' The same procedure will be done all simulations. 
#' With a default recovery rate of 1/3 and transmission rate of 1.
#'
#'
#' Set up the simulation parameters, which will be the same for all simulations.

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.susceptibles <- num.cattle-initial.infecteds


# Transmission and recovery rate of E. coli O157 
ecoli.transmission <-1
ecoli.recovery <-1/3

#R0 for simulation A. `r ecoli.transmission.a / ecoli.recovery.a`
R0<- ecoli.transmission / ecoli.recovery

#Inverse R0
1/R0

#Start and end time for timestep
start.time <- 0
end.time <- 100

#' # Comparing models with a timestep of 1
#' 
#' It can be seen that both methods work the same and we get the same output. 
#' 
#' 
#' ## Model with timestep outside of function
#' 
# Data frame for cattle population
herd.df.a <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population A. 
next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)

#' Setting the timestep to 1. 
#' Inside the sequence we have to add the timestep to the start time and then increment it by the same timestep. 

timestep.a<-1

timesteps.a <- seq(from = start.time + timestep.a, to = end.time, by=timestep.a)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 1).

for (new.time in timesteps.a) {
  # Calculate population at next timestep
  next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                              transmission.rate = (ecoli.transmission*timestep.a),
                                              recovery.rate = (ecoli.recovery*timestep.a))
  # Bind herd and next population
  herd.df.a <- rbind(herd.df.a, next.population.a)
}

#' Plot the results
herd.df.a$time <- c(start.time, timesteps.a)
plot_populations(herd.df.a,col = c("green", "red"))

#' ## Model with timestep included in function
#' 
#' 

# Indicating timestep
ecoli.timesteps.b <-  1

# Data frame for cattle population in population B, here the dataframe includes time.
herd.df.b<- data.frame(time=start.time, 
                      susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population B. 
next.population.b <- timestep_deterministic_SIS(latest = tail(herd.df.b, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery,
                                            timestep=ecoli.timesteps.b)

#' We use a while loop for the simulation, given that the timestep is already included. 
#' We just need to give the simulation a stopping point, in this case the end.time. 
#'  
latest.population.b<-herd.df.b
while (latest.population.b$time < end.time) {
  latest.population.b <- timestep_deterministic_SIS(latest = latest.population.b,
                                                  transmission.rate = ecoli.transmission, 
                                                  recovery.rate = ecoli.recovery, 
                                                  timestep = ecoli.timesteps.b)
  herd.df.b <- rbind(herd.df.b, latest.population.b) 
}


#' Plot the results: we don't have to include the previous step of adding time into the dataframe given that it's already inside of the function and output.

plot_populations(herd.df.b, col = c("blue", "orange"))

#' **Plots from simulation A and B into one: it can be seen that both plots are almost identical.**
#' 
herd.df.a$time <- c(start.time, timesteps.a)
plot_populations(herd.df.a, col = c("green", "red"), with.legend = FALSE)
plot_populations(herd.df.b, new.graph=FALSE, col = c("blue", "orange"), lty=2, with.legend=FALSE)

legend("topright", legend = c("susceptible", "infected", "susceptible", "infected" ),
       col = c("green", "red", "blue", "orange"), lty = c(1, 1, 2,2))


#' # Comparing models with a timestep of 3
#' 
#' It can be seen that both methods work the same and we get the same output. 
#' 
#' 
#' ## Model with timestep outside of function
#' 
# Data frame for cattle population C
herd.df.c <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population C. 
next.population.c <- step_deterministic_SIS(latest = tail(herd.df.c, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)

#' Setting the timestep to 3, and including it in the function the same way as in simulation A. 
timestep.c<-3

timesteps.c <- seq(from = start.time + timestep.c, to = end.time, by=timestep.c)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 3).

for (new.time in timesteps.c) {
  # Calculate population at next timestep
  next.population.c <- step_deterministic_SIS(latest = tail(herd.df.c, 1), 
                                              transmission.rate = (ecoli.transmission*timestep.c),
                                              recovery.rate = (ecoli.recovery*timestep.c))
  # Bind herd and next population
  herd.df.c <- rbind(herd.df.c, next.population.c)
}

#' Plot the results
herd.df.c$time <- c(start.time, timesteps.c)
plot_populations(herd.df.c,col = c("green", "red"))


#' ## Model with timestep included in function

# Indicating timestep
ecoli.timesteps.d <-  3

# Data frame for cattle population D, with time included.
herd.df.d<- data.frame(time=start.time, 
                       susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population D. 
next.population.d <- timestep_deterministic_SIS(latest = tail(herd.df.d, 1), 
                                                transmission.rate = ecoli.transmission,
                                                recovery.rate = ecoli.recovery,
                                                timestep=ecoli.timesteps.d)

#' We use the same while loop as simulation A, with the proper changes for simulation D. 
#' 
latest.population.d<-herd.df.d
while (latest.population.d$time < end.time) {
  latest.population.d <- timestep_deterministic_SIS(latest = latest.population.d,
                                                    transmission.rate = ecoli.transmission, 
                                                    recovery.rate = ecoli.recovery, 
                                                    timestep = ecoli.timesteps.d)
  herd.df.d <- rbind(herd.df.d, latest.population.d) 
}


#' Plot the results
plot_populations(herd.df.d,col = c("blue", "orange"))

#' **Plots from simulation C and D into one: it can be seen that both plots are almost identical.**
#' 

herd.df.c$time <- c(start.time, timesteps.c)
plot_populations(herd.df.c, new.graph=TRUE, col = c("green", "red"), with.legend = FALSE)
plot_populations(herd.df.d, new.graph= FALSE, col = c("blue", "orange"), lty=2, with.legend=FALSE)

legend("topright", legend = c("susceptible", "infected", "susceptible", "infected" ),
       col = c("green", "red", "blue", "orange"), lty = c(1, 1, 2,2))
