#' ---
#' title: "Simple SIS model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

#' Load in the functions that do the work
library(RPiR)
source("0201-step-SIS.R")

#' Set up the simulation parameters

# Starting population size

num.cattle<-100

# Set the birth and death rates

initial.infecteds <- 2
initial.susceptibles <- num.cattle-initial.infecteds


ecoli.transmission<-2/3
ecoli.recovery<-1/3
  
#' Run the simulation

## Set up the population starting size (at the first timestep)
herd.df <- data.frame(susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds)

#'function
next.population <- step_deterministic_SIS(latest = tail(herd.df, 1), 
                                          transmission.rate = ecoli.transmission,
                                          recovery.rate = ecoli.recovery)

# And setting times
start.time <- 0
end.time <- 100

timesteps <- seq(from = start.time + 1, to = end.time)

for (new.time in timesteps) {
  # Calculate population at next timestep
  next.population <- step_deterministic_SIS(latest = tail(herd.df, 1), 
                                            transmission.rate = ecoli.transmission,
                                            recovery.rate = ecoli.recovery)
  # Add new element onto end of population vector
  herd.df <- rbind(herd.df, next.population)
}


#' And plot the results
herd.df$time <- c(start.time, timesteps)
plot_populations(herd.df,col = c("green", "red"))

R0 <- ecoli.transmission / ecoli.recovery

#R0<1
ecoli.transmission2<-2/5
ecoli.recovery2<-2/3

R0.1<-ecoli.transmission2/ecoli.recovery2


herd.df2 <- data.frame(susceptibles = initial.susceptibles, 
                      infecteds=initial.infecteds)

#'function
next.population2 <- step_deterministic_SIS(latest = tail(herd.df, 1), 
                                          transmission.rate = ecoli.transmission2,
                                          recovery.rate = ecoli.recovery2)

# And setting times
start.time <- 0
end.time <- 100

timesteps <- seq(from = start.time + 1, to = end.time)

for (new.time in timesteps) {
  # Calculate population at next timestep
  next.population2 <- step_deterministic_SIS(latest = tail(herd.df, 1), 
                                            transmission.rate = ecoli.transmission2,
                                            recovery.rate = ecoli.recovery2)
  # Add new element onto end of population vector
  herd.df2 <- rbind(herd.df2, next.population2)
}


#' And plot the results
herd.df2$time <- c(start.time, timesteps)
plot_populations(herd.df2,col = c("green", "red"))