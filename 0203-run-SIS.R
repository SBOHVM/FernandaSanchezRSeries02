#' ---
#' title: "Simple SIS model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 

#' Load in the functions that do the work
library(RPiR)
source("0203-deterministic-SIS.R")
source("0201-step-SIS.R")

#' We are going to compare Susceptible-Infected-Susceptible (SIS) models for E. coli O157 in cattle, to see if the outcome of putting the timestep inside the function is the same as adding timestep later. 
#'
#' 1. Susceptible model
### <b>
#'    $$S(t + 1) = S(t)-\beta \times \frac{S(t)\times I(t)}{N}+\sigma \times I(t)$$
### </b>
#' 2. Infected model
### <b>
#'    $$I(t + 1) = S(t)+\beta \times \frac{S(t)\times I(t)}{N}-\sigma \times I(t)$$
### </b>
#' 3. N is a constant for total population
### <b>
#'    $$N = S(t)+ I(t)$$
### </b>
#'

#'The same procedure will be done for simulation A, B, C and D. 
#'We only change the ecoli.transmission rate and a default recovery rate of (1/3), therefore changing R0. 
#'Timesteps with their start and end time are the same for all simulations. 
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

#Inverted R0
1/R0

#Start and end time for timestep
start.time <- 0
end.time <- 100

#' # Comparing models with a timestep of 1
#' 
#' It can be seen that both methods work the same and we get the same output. 
#' 
#' 
#' ## Model with timestep included in function

# Indicating timestep
ecoli.timesteps.1 <-  1

# Data frame for cattle population in population A
herd.df.1<- data.frame(time=start.time, 
                      susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

herd.df.1

#'Function for SIS model to calculate population dynamics in population A. 
next.population.1 <- timestep_deterministic_SIS(latest = tail(herd.df.1, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery,
                                            timestep=ecoli.timesteps.1)
next.population.1

#Proportion of susceptibles at equilibrium
next.population.1[,c("susceptibles")]/num.cattle

#' Setting start and end time to make a sequence and call it timesteps.
#' This time is in weeks. 


#' Then make a loop with timesteps created above, to calculate susceptible and infected cattle in different weeks.
#' And finally bind the data frames herd.df.a and next.population.a into one. 
#' 
latest.population.1<-herd.df.1
while (latest.population.1$time < end.time) {
  latest.population.1 <- timestep_deterministic_SIS(latest = latest.population.1,
                                                  transmission.rate = ecoli.transmission, 
                                                  recovery.rate = ecoli.recovery, 
                                                  timestep = ecoli.timesteps.1)
  herd.df.1 <- rbind(herd.df.1, latest.population.1) 
}


#' Plot the results with timesteps against the population in the data frame herd.df.a

plot_populations(herd.df.1,col = c("green", "red"))

#' ## Model with timestep outside of function
#' 
# Data frame for cattle population
herd.df.a <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population A. 
next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)
next.population.a

#Proportion of susceptibles at equilibrium
next.population.a[,c("susceptibles")]/num.cattle

#' Setting the timestep to 1. 
#' Inside the sequence we have to add the timestep to the start time and then increment it by the same timestep. 

timestep1<-1

timesteps1 <- seq(from = start.time + timestep1, to = end.time, by=timestep1)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 1).

for (new.time in timesteps1) {
  # Calculate population at next timestep
  next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                              transmission.rate = (ecoli.transmission*timestep1),
                                              recovery.rate = (ecoli.recovery*timestep1))
  # Bind herd and next population
  herd.df.a <- rbind(herd.df.a, next.population.a)
}


#' Plot the results
herd.df.a$time <- c(start.time, timesteps1)
plot_populations(herd.df.a,col = c("green", "red"))


#' # Comparing models with a timestep of 3
#' 
#' It can be seen that both methods work the same and we get the same output. 
#' 
#' 
#' ## Model with timestep included in function

# Indicating timestep
ecoli.timesteps.2 <-  3

# Data frame for cattle population in population A
herd.df.2<- data.frame(time=start.time, 
                       susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

herd.df.2

#'Function for SIS model to calculate population dynamics in population A. 
next.population.2 <- timestep_deterministic_SIS(latest = tail(herd.df.2, 1), 
                                                transmission.rate = ecoli.transmission,
                                                recovery.rate = ecoli.recovery,
                                                timestep=ecoli.timesteps.2)
next.population.2

#Proportion of susceptibles at equilibrium
next.population.2[,c("susceptibles")]/num.cattle

#' Setting start and end time to make a sequence and call it timesteps.
#' This time is in weeks. 


#' Then make a loop with timesteps created above, to calculate susceptible and infected cattle in different weeks.
#' And finally bind the data frames herd.df.a and next.population.a into one. 
#' 
latest.population.2<-herd.df.2
while (latest.population.2$time < end.time) {
  latest.population.2 <- timestep_deterministic_SIS(latest = latest.population.2,
                                                    transmission.rate = ecoli.transmission, 
                                                    recovery.rate = ecoli.recovery, 
                                                    timestep = ecoli.timesteps.2)
  herd.df.2 <- rbind(herd.df.2, latest.population.2) 
}


#' Plot the results with timesteps against the population in the data frame herd.df.a

plot_populations(herd.df.2,col = c("green", "red"))

#' ## Model with timestep outside of function
#' 
# Data frame for cattle population
herd.df.b <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population A. 
next.population.b <- step_deterministic_SIS(latest = tail(herd.df.b, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)
next.population.b

#Proportion of susceptibles at equilibrium
next.population.b[,c("susceptibles")]/num.cattle

#' Setting the timestep to 1. 
#' Inside the sequence we have to add the timestep to the start time and then increment it by the same timestep. 

timestep2<-3

timesteps2 <- seq(from = start.time + timestep2, to = end.time, by=timestep2)

#' Loop created with the sequence timestep above and also multiplying the ecoli.transmission and ecoli.recovery by the timestep of our choice (in this case 1).

for (new.time in timesteps2) {
  # Calculate population at next timestep
  next.population.b <- step_deterministic_SIS(latest = tail(herd.df.b, 1), 
                                              transmission.rate = (ecoli.transmission*timestep2),
                                              recovery.rate = (ecoli.recovery*timestep2))
  # Bind herd and next population
  herd.df.b <- rbind(herd.df.b, next.population.b)
}


#' Plot the results
herd.df.b$time <- c(start.time, timesteps2)
plot_populations(herd.df.b,col = c("green", "red"))
