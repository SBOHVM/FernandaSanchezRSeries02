#' ---
#' title: "Susceptible-Infected-Susceptible (SIS) model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

#' A Susceptible-Infected-Susceptible (SIS) model
#'
#'
#' Arguments:
#'
#' - latest -- a data frame containing the latest susceptible and infected population count
#'             (columns are 'susceptibles', 'infecteds' and 'time')
#'
#' - transmission.rate -- the transmission rate
#'
#' - recovery.rate -- the recovery rate
#' 
#' - timestep -- time interval
#'
#' Returns:
#'
#' - a data.frame containing the updated susceptible and infected population, as well as the time in relation to the timestep.

timestep_deterministic_SIS <- function(latest, transmission.rate, recovery.rate, timestep) {
  
  ## Calculate population changes
  effective.transmission.rate<- transmission.rate*timestep
  effective.recovery.rate<-recovery.rate*timestep
  population.size<-latest$susceptibles+ latest$infecteds
  
  new.recovered <- effective.recovery.rate * latest$infecteds
  new.infected <- effective.transmission.rate * ((latest$susceptibles*latest$infecteds)/population.size)
  
  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  
  ## Return data frame containing next population and time related to the timestep
  data.frame(susceptibles = next.susceptibles, 
             infecteds=next.infecteds,
             time=latest$time+timestep)
}






#' #### Does the function works without any external (global) information?

library(codetools)
if (length(findGlobals(timestep_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function timestep_deterministic_SIS() may not use global variable(s): ",
    findGlobals(timestep_deterministic_SIS, merge = FALSE)$variables
  )
}
