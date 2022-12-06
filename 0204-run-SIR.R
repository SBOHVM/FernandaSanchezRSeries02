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

#' We are going to compare Susceptible-Infected-Recovered (SIR) models for foot-and-mouth disease (FMD) in farms with different R0. 
#' And compare SIR and SIS models with the same R0. 
#'
#'
#'SIR Model
#'
#' 1. Susceptible model
#'
#'    $$S(t + 1) = S(t)-\beta \times ((S(t)\times I(t))/N)+\sigma \times I(t)$$
#'
#' 2. Infected model
#'
#'    $$I(t + 1) = S(t)+\beta \times ((S(t)\times I(t))/N)-\sigma \times I(t)$$
#'
#' 3. Recovered model
#'
#'    $$R(t + 1) = R(t)+\sigma \times I(t)$$
#'
#' 4. N is a constant for total population
#'
#'    $$N = S(t)+ I(t)$$
#'
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

#Set timesteps to 0.2 weeks
FMD.timesteps <-  0.2

#' # Compare SIR and SIS models with the same R0 (R0=4)
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

# Data frame for farms
farm.df.SIR.1<- data.frame(time=start.time, 
                     susceptibles = initial.susceptibles, 
                     infecteds=initial.infecteds,
                     recovereds=initial.recovereds)

#'Function for SIR model to calculate population dynamics in farms. 
next.population.SIR.1 <- timestep_deterministic_SIR(latest = tail(farm.df.SIR.1, 1), 
                                              transmission.rate = FMD.transmission,
                                              recovery.rate = FMD.recovery,
                                              timestep=FMD.timesteps)


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

#' Proportion of susceptible farms at equilibrium
prop.farm.SIR.1<-tail(farm.df.SIR.1,1)
prop.farm.SIR.1[,c("susceptibles")]/num.farms

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

#'Function for SIS model to calculate population. 
next.population.SIS <- timestep_deterministic_SIS(latest = tail(farm.df.SIS, 1), 
                                                transmission.rate = FMD.transmission,
                                                recovery.rate = FMD.recovery,
                                                timestep=FMD.timesteps)

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
#' Proportion of susceptible farms at equilibrium
prop.farm.SIS<-tail(farm.df.SIS,1)
prop.farm.SIS[,c("susceptibles")]/num.farms

#' Plot the results 
plot_populations(farm.df.SIS,col = c("blue", "orange"))

#' **Plotting SIS and SIR model together to see the differences**
#' 
#' Comparing both plots, we can see the proportion of susceptible farms at equilibrium left after a few weeks is close to zero with the SIR model
#' given that the recovered population is assumed to not get infected again in this model. 
#' And with the SIS model, there is an equilibrium when there are 25% of susceptible farms left.   
#' 
#' 
#' In the SIS model, we can see that the infected population is greater than the susceptible, meaning that under these conditions, there would be an outbreak with this type of model.
#' But the SIR model, by considering that the recovered population won't get infected again,
#' the susceptible population is close to zero after the initial infections seen between week 5 and 10. 
#' Meaning that under this type of model, there won't be an outbreak with these conditions.
#' 
plot_populations(farm.df.SIR.1,col = c("green", "red", "black"), with.legend = FALSE)
plot_populations(farm.df.SIS, new.graph=FALSE, col = c("blue", "orange"), lty=2, with.legend = FALSE)

legend("topright", legend = c("SIR= susceptible", "SIR= infected", "SIR=recovered", "SIS= susceptible", "SIS=infected" ),
       col = c("green", "red", "black" ,"blue", "orange"), lty = c(1, 1,1, 2,2))


#' # Comparing SIR done above with a different R0
#' 
#' ## SIR simulation with R=0.5
# Transmission and recovery rate of FMD for the second simulation
FMD.transmission.2 <-1/4
FMD.recovery.2 <-1/2
          
#R0.2
R0.2<- FMD.transmission.2 / FMD.recovery.2
R0.2
          
#Inverse R0.2
1/R0.2
          
# Data frame for farms
farm.df.SIR.2<- data.frame(time=start.time, 
                      susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds,
                      recovereds=initial.recovereds)
          
farm.df.SIR.2
        
#'Function for SIR model to calculate population dynamics
next.population.SIR.2 <- timestep_deterministic_SIR(latest = tail(farm.df.SIR.2, 1), 
                                                        transmission.rate = FMD.transmission.2,
                                                        recovery.rate = FMD.recovery.2,
                                                        timestep=FMD.timesteps)
#' We use the same while loop as for the first simulation.

latest.population.SIR.2<-farm.df.SIR.2
while (latest.population.SIR.2$time < end.time) {
      latest.population.SIR.2 <- timestep_deterministic_SIR(latest = latest.population.SIR.2,
                                                          transmission.rate = FMD.transmission.2, 
                                                          recovery.rate = FMD.recovery.2, 
                                                          timestep = FMD.timesteps)
        farm.df.SIR.2 <- rbind(farm.df.SIR.2, latest.population.SIR.2) 
      }

#' Proportion of susceptible farms at equilibrium
prop.farm.SIR.2<-tail(farm.df.SIR.2,1)
prop.farm.SIR.2[,c("susceptibles")]/num.farms        
  
#' Plot the results
plot_populations(farm.df.SIR.2,new.graph=TRUE,col = c("blue", "orange", "purple"))     
      
              
#' **Plot the results of both SIR model to see differences**
#' 
#' As previously mentioned before, the plot with an R0 of 4, was able to cause infection but when the population started to recover, the disease died out given that there weren't susceptibles left. 
#' 
#' When R0 is lower than 1, we see a similar scenario as with and SIS lower than 1 (as shown in practical 2.1).
#' Because the transmission rate is lower than the recovery rate, the disease dies out and is not able to cause an outbreak.
#' So we see that the susceptible population stays close to 100 and the infected population close to zero. 
#' The recovered population is also close to zero given that most farms didn't get infected, so there wasn't a disease to recover from. 
#' 
plot_populations(farm.df.SIR.1,new.graph=TRUE,col = c("green", "red", "black"), with.legend = FALSE)     
plot_populations(farm.df.SIR.2,new.graph=FALSE,col = c("blue", "orange", "purple"), lty=2, with.legend = FALSE)


legend("topright", legend = c("SIR.1= susceptible", "SIR.1= infected", "SIR.1=recovered", "SIR.2= susceptible", "SIR.2=infected", "SIR.2=recovered" ),
       col = c("green", "red", "black" ,"blue", "orange", "purple"), lty = c(1, 1,1, 2,2,2))
