#' ---
#' title: "Susceptible-Infected-Susceptible (SIR) model"
#' author: "Fernanda Sánchez"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

#' A Susceptible-Infected-Susceptible (SIR) model
#'
#'
#' Arguments:
#'
#' - latest -- a data frame containing the latest susceptible, recovered and infected population count
#'             (columns are 'susceptibles', 'recovereds' and 'infecteds')
#'
#' - transmission.rate -- the transmission rate
#'
#' - recovery.rate -- the recovery rate
#'
#' Returns:
#'
#' - a data.frame containing the updated susceptible, infected  and recovered population, as well as the timestep in weeks.

timestep_deterministic_SIR <- function(latest, transmission.rate, recovery.rate, timestep) {
  
  ## Calculate population changes
  effective.transmission.rate<- transmission.rate*timestep
  effective.recovery.rate<-recovery.rate*timestep
  
  population.size<-latest$susceptibles+ latest$infecteds+latest$recovereds
  new.recovered <- effective.recovery.rate * latest$infecteds
  new.infected <- effective.transmission.rate * ((latest$susceptibles*latest$infecteds)/population.size)
  
  next.susceptibles <- latest$susceptibles - new.infected
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  next.recovereds<-latest$recovereds+new.recovered
  
  ## Return data frame containing next population count
  data.frame(susceptibles = next.susceptibles, 
             infecteds=next.infecteds,
             recovereds=next.recovereds,
             time=latest$time+timestep)
}


#' #### Does the function works without any external (global) information?

library(codetools)
if (length(findGlobals(timestep_deterministic_SIR,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function timestep_deterministic_SIR() may not use global variable(s): ",
    findGlobals(timestep_deterministic_SIR, merge = FALSE)$variables
  )
}