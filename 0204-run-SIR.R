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

#' We are going to compare Susceptible-Infected-Susceptible (SIR) models for foot-and-mouth disease (FMD) in farms with different R0. 
#' And compare SIR and SIS models with the same R0. 
#'
#'
#'SIR Model
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


#' Set up the simulation parameters, which will be the same for all simulations.

# Starting population size

num.farms<-100

# Set the initial number of infected and susceptible  individuals

initial.infecteds <- 2
initial.recovereds<-0
initial.susceptibles <- num.farms-initial.infecteds-initial.recovereds

#Set start and end time
start.time <- 0
end.time <- 30

#Set timesteps to 0.5 weeks
FMD.timesteps <-  0.2

#' # Compare SIR and SIS models with the same R0 
#' 
#' ## SIR Model
# Transmission and recovery rate of E. coli O157 for simulation.
FMD.transmission <-2
FMD.recovery <-1/2

#R0 for simulation. 
R0<- FMD.transmission / FMD.recovery
R0

#Inverse R0
1/R0

# Data frame for farm
farm.df.SIR.1<- data.frame(time=start.time, 
                     susceptibles = initial.susceptibles, 
                     infecteds=initial.infecteds,
                     recovereds=initial.recovereds)

farm.df.SIR.1

#'Function for SIR model to calculate population dynamics in farms. 
next.population.SIR.1 <- timestep_deterministic_SIR(latest = tail(farm.df.SIR.1, 1), 
                                              transmission.rate = FMD.transmission,
                                              recovery.rate = FMD.recovery,
                                              timestep=FMD.timesteps)
next.population.SIR.1

#Proportion of susceptibles at equilibrium
next.population.SIR.1[,c("susceptibles")]/num.farms

#' We use a while loop for the simulation, given that the timestep is already included. 
#' We just need to give the simulation a stopping point, in this case the end.time. 
latest.population.SIR.1<-farm.df.SIR.1
while (latest.population.SIR.1$time < end.time) {
  latest.population.SIR.1 <- timestep_deterministic_SIR(latest = latest.population.SIR.1,
                                                  transmission.rate = FMD.transmission, 
                                                  recovery.rate = FMD.recovery, 
                                                  timestep = FMD.timesteps)
  farm.df.SIR.1 <- rbind(farm.df.SIR.1, latest.population.SIR.1) 
}


#' Plot the results 
plot_populations(farm.df.SIR.1,col = c("green", "red", "black"))

#' ## SIS model
#' 
#' 
#' 

# Dataframe for SIS model (doesn't have the recovered population)
farm.df.SIS<- data.frame(time=start.time, 
                       susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

farm.df.SIS

#'Function for SIS model to calculate population. 
next.population.SIS <- timestep_deterministic_SIS(latest = tail(farm.df.SIS, 1), 
                                                transmission.rate = FMD.transmission,
                                                recovery.rate = FMD.recovery,
                                                timestep=FMD.timesteps)
next.population.SIS

#Proportion of susceptibles at equilibrium
next.population.SIS[,c("susceptibles")]/num.farms


#' We use the same while loop as the SIR model above, with the proper changes for an SIS model.
#' 
latest.population.SIS<-farm.df.SIS
while (latest.population.SIS$time < end.time) {
  latest.population.SIS <- timestep_deterministic_SIS(latest = latest.population.SIS,
                                                    transmission.rate = FMD.transmission, 
                                                    recovery.rate = FMD.recovery, 
                                                    timestep = FMD.timesteps)
  farm.df.SIS <- rbind(farm.df.SIS, latest.population.SIS) 
}


#' Plot the results 
plot_populations(farm.df.SIS,col = c("green", "red"))

#' Plotting SIS and SIR model together to see the differences
#' 
#' 
plot_populations(farm.df.SIR.1,col = c("green", "red", "black"))
plot_populations(farm.df.SIS, new.graph=FALSE, col = c("green", "red"), lty=2)

#' # Comparing SIR done above with a different R0
#' 
#' 
# Transmission and recovery rate of E. coli O157 for simulation A
FMD.transmission.2 <-1/4
FMD.recovery.2 <-1/2
          
#R0 for simulation A. `r ecoli.transmission.a / ecoli.recovery.a`
R0.2<- FMD.transmission / FMD.recovery
R0.2
          
#Inverted R0
1/R0.2
          
## Data frame for cattle population in population A
farm.df.SIR.2<- data.frame(time=start.time, 
                      susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds,
                      recovereds=initial.recovereds)
          
farm.df.SIR.2
        
#'Function for SIR model to calculate population dynamics in population A. 
next.population.SIR.2 <- timestep_deterministic_SIR(latest = tail(farm.df.SIR.2, 1), 
                                                        transmission.rate = FMD.transmission.2,
                                                        recovery.rate = FMD.recovery.2,
                                                        timestep=FMD.timesteps)
next.population.SIR.2
          
#Proportion of susceptibles at equilibrium
next.population.SIR.2[,c("susceptibles")]/num.farms
          
latest.population.SIR.2<-farm.df.SIR.2
while (latest.population.SIR.2$time < end.time) {
      latest.population.SIR.2 <- timestep_deterministic_SIR(latest = latest.population.SIR.2,
                                                          transmission.rate = FMD.transmission.2, 
                                                          recovery.rate = FMD.recovery.2, 
                                                          timestep = FMD.timesteps)
        farm.df.SIR.2 <- rbind(farm.df.SIR.2, latest.population.SIR.2) 
      }
        
  
#' Plot the results
plot_populations(farm.df.SIR.1,new.graph=TRUE,col = c("green", "red", "black"))     
      
              
#' Plot the results of both SIR models.
plot_populations(farm.df.SIR.1,new.graph=TRUE,col = c("green", "red", "black"))     
plot_populations(farm.df.SIR.2,new.graph=FALSE,col = c("green", "red", "black"), lty=2)
