#' ---
#' title: "Simple SIR model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 

#' Load in the functions that do the work
library(RPiR)
source("0204-deterministic-SIR.R")
source("0203-deterministic-SIS.R")

#' We are going to compare four Susceptible-Infected-Susceptible (SIR) models for E. coli O157 in cattle 
#'
#' 1. Susceptible model
### <b>
#'    $$S(t + 1) = S(t)-\beta \times ((S(t)\times I(t))/N)+\sigma \times I(t)$$
### </b>
#' 2. Infected model
### <b>
#'    $$I(t + 1) = S(t)+\beta \times ((S(t)\times I(t))/N)-\sigma \times I(t)$$
### </b>
#' 3. Recovered model
### <b>
#'    $$R(t + 1) = R(t)+\sigma \times I(t)$$
### </b>
#' 4. N is a constant for total population
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

num.farms<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.recovereds<-0
initial.susceptibles <- num.farms-initial.infecteds-initial.recovereds


# Transmission and recovery rate of E. coli O157 for simulation A
FMD.transmission <-2
FMD.recovery <-1/2
start.time <- 0
end.time <- 50

FMD.timesteps <-  0.5

#R0 for simulation A. `r ecoli.transmission.a / ecoli.recovery.a`
R0<- FMD.transmission / FMD.recovery


#Inverted R0
1/R0

## Data frame for cattle population in population A
farm.df<- data.frame(time=start.time, 
                     susceptibles = initial.susceptibles, 
                     infecteds=initial.infecteds,
                     recovereds=initial.recovereds)

farm.df

#'Function for SIR model to calculate population dynamics in population A. 
next.population <- timestep_deterministic_SIR(latest = tail(farm.df, 1), 
                                              transmission.rate = FMD.transmission,
                                              recovery.rate = FMD.recovery,
                                              timestep=FMD.timesteps)
next.population

#Proportion of susceptibles at equilibrium
next.population[,c("susceptibles")]/num.farms

latest.population<-farm.df
while (latest.population$time < end.time) {
  latest.population <- timestep_deterministic_SIR(latest = latest.population,
                                                  transmission.rate = FMD.transmission, 
                                                  recovery.rate = FMD.recovery, 
                                                  timestep = FMD.timesteps)
  farm.df <- rbind(farm.df, latest.population) 
}

farm.df

#' Plot the results with timesteps against the population in the data frame farm.df

plot_populations(farm.df,col = c("green", "red", "black"))

#' # Comparing SIR done above and SIS models with same R0
#' 
#' 
#' 
FMD.transmission <-2
FMD.recovery <-1/2
start.time <- 0
end.time <- 50

FMD.timesteps <-  0.5

#R0 for simulation A. `r ecoli.transmission.a / ecoli.recovery.a`
R0<- FMD.transmission / FMD.recovery


farm.df.SIS<- data.frame(time=start.time, 
                       susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

farm.df.SIS

#'Function for SIS model to calculate population dynamics in population A. 
next.population.SIS <- timestep_deterministic_SIS(latest = tail(farm.df.SIS, 1), 
                                                transmission.rate = FMD.transmission,
                                                recovery.rate = FMD.recovery,
                                                timestep=FMD.timesteps)
next.population.SIS

#Proportion of susceptibles at equilibrium
next.population.SIS[,c("susceptibles")]/num.farms

#' Setting start and end time to make a sequence and call it timesteps.
#' This time is in weeks. 


#' Then make a loop with timesteps created above, to calculate susceptible and infected cattle in different weeks.
#' And finally bind the data frames herd.df.a and next.population.a into one. 
#' 
latest.population.SIS<-farm.df.SIS
while (latest.population.SIS$time < end.time) {
  latest.population.SIS <- timestep_deterministic_SIS(latest = latest.population.SIS,
                                                    transmission.rate = FMD.transmission, 
                                                    recovery.rate = FMD.recovery, 
                                                    timestep = FMD.timesteps)
  farm.df.SIS <- rbind(farm.df.SIS, latest.population.SIS) 
}


#' Plot the results with timesteps against the population in the data frame herd.df.a

plot_populations(farm.df.SIS,col = c("green", "red"))

#' Plotting SIS and SIR model together to see the differences
#' 
#' 
plot_populations(farm.df,col = c("green", "red", "black"))
plot_populations(farm.df.SIS, new.graph=FALSE, col = c("green", "red"))

#' # Comparing SIR done above with a different R0
#' 
#' 
# Transmission and recovery rate of E. coli O157 for simulation A
FMD.transmission.2 <-1/4
FMD.recovery.2 <-1/2
start.time <- 0
end.time <- 50
          
FMD.timesteps <-  0.5
          
#R0 for simulation A. `r ecoli.transmission.a / ecoli.recovery.a`
R0.2<- FMD.transmission / FMD.recovery
          
#Inverted R0
1/R0.2
          
## Data frame for cattle population in population A
farm.df.2<- data.frame(time=start.time, 
                      susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds,
                      recovereds=initial.recovereds)
          
farm.df.2
        
#'Function for SIR model to calculate population dynamics in population A. 
next.population.2 <- timestep_deterministic_SIR(latest = tail(farm.df.2, 1), 
                                                        transmission.rate = FMD.transmission.2,
                                                        recovery.rate = FMD.recovery.2,
                                                        timestep=FMD.timesteps)
next.population.2
          
#Proportion of susceptibles at equilibrium
next.population.2[,c("susceptibles")]/num.farms
          
latest.population.2<-farm.df.2
while (latest.population.2$time < end.time) {
      latest.population.2 <- timestep_deterministic_SIR(latest = latest.population.2,
                                                          transmission.rate = FMD.transmission.2, 
                                                          recovery.rate = FMD.recovery.2, 
                                                          timestep = FMD.timesteps)
        farm.df.2 <- rbind(farm.df.2, latest.population.2) 
      }
          
      farm.df.2
          
#' Plot the results with timesteps against the population in the data frame farm.df
plot_populations(farm.df.2,new.graph=TRUE,col = c("green", "red", "black"))     
plot_populations(farm.df,new.graph=FALSE,col = c("green", "red", "black"))
