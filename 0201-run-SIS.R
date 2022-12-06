#' ---
#' title: "Simple SIS model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---
#' 

#' Load in the functions that do the work
library(RPiR)
source("0201-step-SIS.R")

#' We are going to compare four Susceptible-Infected-Susceptible (SIS) models for E. coli O157 in cattle, with different basic reproduction numbers (R0). 
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

#'The same procedure will be done for simulation A, B, C and D. 
#'We only change the ecoli.transmission rate and a default recovery rate of (1/3), therefore changing the basic reproductive number (R0). 
#'Timesteps are in weeks and their start and end time are the same for all simulations. 
#'
#'
#' Set up the simulation parameters, which will be the same for all simulations.

# Starting population size

num.cattle<-100

# Set the initial number of infected and susceptible individuals

initial.infecteds <- 2
initial.susceptibles <- num.cattle-initial.infecteds

# Setting start and end time to make a sequence and call it timesteps.
start.time <- 0
end.time <- 100

timesteps <- seq(from = start.time + 1, to = end.time)

#' ## Simulation A, where R0>1 (R=2)
#' 
# Transmission and recovery rate of E. coli O157 for simulation A
ecoli.transmission.a <-2/3
ecoli.recovery.a <-1/3

# R0 for simulation A.
R0.a <- ecoli.transmission.a / ecoli.recovery.a
R0.a

#Inverse R0.a
1/R0.a

# Data frame for cattle population A
herd.df.a<- data.frame(susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population A. 
next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                          transmission.rate = ecoli.transmission.a,
                                          recovery.rate = ecoli.recovery.a)

#' Then make a loop with timesteps created above, to calculate susceptible and infected cattle in different weeks.
#' And finally bind the data frames herd.df.a and next.population.a into one. 
#' 
for (new.time in timesteps) {
  next.population.a <- step_deterministic_SIS(latest = tail(herd.df.a, 1), 
                                            transmission.rate = ecoli.transmission.a,
                                            recovery.rate = ecoli.recovery.a)
  herd.df.a <- rbind(herd.df.a, next.population.a)
}

#'Proportion of susceptibles at equilibrium according to the population size: 
#'the disease reaches an equilibrium when half of the population is susceptible.
#'This proportion is the same as the inverted R0, which is also the proportion of susceptibles according to R0. 
#'
prop.population.a<-tail(herd.df.a,1)
prop.population.a[,c("susceptibles")]/num.cattle

#' **Plot of the results**
#' 
#' 
#' We first need to plot the results with timesteps against the population in the data frame herd.df.a
#' 
#' 
#' It can be seen in this plot that there is an equilibrium when 50% of the population are susceptible.
#' At this point, the number of susceptibles and infecteds will be the same.
#' 
herd.df.a$time <- c(start.time, timesteps)
plot_populations(herd.df.a,col = c("green", "red"))


#' ## Simulation B, where R0<1 (R=0.6)

# Transmission and recovery rate of E. coli O157 for simulation B
ecoli.transmission.b<-1/5
ecoli.recovery.b<-1/3

#R0 for simulation B
R0.b<-ecoli.transmission.b/ecoli.recovery.b
R0.b

#Inverse R0.b
1/R0.b

# Data frame for cattle population B.
herd.df.b <- data.frame(susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population B. 
next.population.b <- step_deterministic_SIS(latest = tail(herd.df.b, 1), 
                                          transmission.rate = ecoli.transmission.b,
                                          recovery.rate = ecoli.recovery.b)

#' Same loop with timesteps created in simulation A, but for population B. 
#
for (new.time in timesteps) {
  next.population.b <- step_deterministic_SIS(latest = tail(herd.df.b, 1), 
                                            transmission.rate = ecoli.transmission.b,
                                            recovery.rate = ecoli.recovery.b)
  herd.df.b <- rbind(herd.df.b, next.population.b)
}

#' Proportion susceptible at equilibrium according to the population size:
#' in this case there is an equilibrium, where 100% of the population stays susceptible. 
#' The inverse R0 is greater than 1, which is because R0 is 0.6. But it still means that 100% of the population stays susceptible.
#' Given that R0 is less than 1 and the disease is not able to maintain itself in the population, there will be no outbreak.
#' 
 
prop.population.b<-tail(herd.df.b,1)
prop.population.b[,c("susceptibles")]/num.cattle

#' **Plot of the results**
#' 
#' 
#' As mentioned before, the disease is not able to cause an outbreak given that the transmission rate is lower than the recovery rate. 
#' Therefore, the infected population decreases very quickly and after a few weeks, the entire population is susceptible again.
herd.df.b$time <- c(start.time, timesteps)
plot_populations(herd.df.b,col = c("green", "red"))


#' ## Simulation C, where R=1
#'
#'
# Transmission and recovery rate of E. coli O157 for simulation C
ecoli.transmission.c<-1/3
ecoli.recovery.c<-1/3

#R0 for simulation C
R0.c<-ecoli.transmission.c/ecoli.recovery.c
R0.c

#Inverse R0.c
1/R0.c

# Data frame for cattle population C.
herd.df.c <- data.frame(susceptibles = initial.susceptibles, 
                       infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population C. 
next.population.c <- step_deterministic_SIS(latest = tail(herd.df.c, 1), 
                                           transmission.rate = ecoli.transmission.c,
                                           recovery.rate = ecoli.recovery.c)

#' Same loop with timesteps created in simulation A, but for population C. 
#

for (new.time in timesteps) {
  next.population.c <- step_deterministic_SIS(latest = tail(herd.df.c, 1), 
                                             transmission.rate = ecoli.transmission.c,
                                             recovery.rate = ecoli.recovery.c)
  herd.df.c <- rbind(herd.df.c, next.population.c)
}

#' Proportion susceptible at equilibrium according to population size:
#' this is very close to 1, which is the same concept as simulation B, where R0 was lower than 1 and the inverted R0 is 1.
#' In this case the susceptible population is close to 100% given that the transmission and recovery rate are the same,
#' meaning that the disease is no able to sustain itself in the population and there will not be an outbreak.
#' 

prop.population.c<-tail(herd.df.c,1)
prop.population.c[,c("susceptibles")]/num.cattle

#' **Plot of the results**
#' 
#' 
#' We have a similar plot as simulation B, meaning that even when R0 is 1 the disease is not able to maintain itself in this population,
#' therefore, there will not be an outbreak of this disease in this scenario.
#' 
herd.df.c$time <- c(start.time, timesteps)
plot_populations(herd.df.c,col = c("green", "red"))


#' ## Simulation D, where R0>1 (R=3)
#'The transmission and recovery rates are the currently stated for E. coli O157 in cattle.
#'
#'
# Transmission and recovery rate of E. coli O157 for simulation D
ecoli.transmission.d<-1
ecoli.recovery.d<-1/3

#R0 for simulation D
R0.d<-ecoli.transmission.d/ecoli.recovery.d
R0.d

#Inverse R0.d
1/R0.d

# Data frame for cattle population D.
herd.df.d <- data.frame(susceptibles = initial.susceptibles, 
                        infecteds=initial.infecteds)

#'Function for SIS model to calculate population dynamics in population D. 
next.population.d <- step_deterministic_SIS(latest = tail(herd.df.d, 1), 
                                            transmission.rate = ecoli.transmission.d,
                                            recovery.rate = ecoli.recovery.d)

#' Same loop with timesteps created in simulation A, but for population D. 
#
for (new.time in timesteps) {
  next.population.d <- step_deterministic_SIS(latest = tail(herd.df.d, 1), 
                                              transmission.rate = ecoli.transmission.d,
                                              recovery.rate = ecoli.recovery.d)
  herd.df.d <- rbind(herd.df.d, next.population.d)
}

#' Proportion susceptible at equilibrium according to population size:
#' this proportion is the same as the inverse R0. 
#' Which means there's an equilibrium when 33% of susceptibles are left in the population. 
#' 

prop.population.d<-tail(herd.df.d,1)
prop.population.d[,c("susceptibles")]/num.cattle

#' **Plot of the results**
#' 
#' 
#' It can be seen, as mentioned before that there's an equilibrium of susceptibles and infecteds when the susceptible population reaches approximately 33.
#' In this case the infected population after a few weeks will be greater than the susceptible population. 
#' This is because the transmission rate is bigger than the recovery rate (making R0 bigger than 1).
#' Therefore, in this case, E. coli is able to cause an outbreak. 
#' 
herd.df.d$time <- c(start.time, timesteps)
plot_populations(herd.df.d,col = c("green", "red"))
