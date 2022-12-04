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
#'             (columns are 'susceptibles' and 'infecteds')
#'
#' - transmission.rate -- the transmission rate
#'
#' - recovery.rate -- the recovery rate
#'
#' Returns:
#'
#' - a data.frame containing the updated susceptible and infected population

step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate) {

  ## Calculate population changes
  population.size<-latest$susceptibles+ latest$infecteds
  new.recovered <- recovery.rate * latest$infecteds
  new.infected <- transmission.rate * ((latest$susceptibles*latest$infecteds)/population.size)
  
  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  
  ## Return data frame containing next population
  data.frame(susceptibles = next.susceptibles, 
             infecteds=next.infecteds)
}






#' #### Does the function work without any external (global) information?

library(codetools)
if (length(findGlobals(step_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function step_deterministic_SIS() may not use global variable(s): ",
    findGlobals(step_deterministic_SIS, merge = FALSE)$variables
  )
}
