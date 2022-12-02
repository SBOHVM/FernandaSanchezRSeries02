#' ---
#' title: "Simple birth-death difference equation model"
#' author: "Richard Reeve"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


step_deterministic_SIS <- function(latest, transmission.rate, recovery.rate) {
  #' A simple deterministic exponential birth-death model
  #'
  #' Run one step of a simple deterministic exponential birth-death model
  #'
  #' Arguments:
  #'
  #' - latest -- a data frame containing the latest population count
  #'             (column is 'count')
  #'
  #' - birth.rate -- the birth rate
  #'
  #' - death.rate -- the death rate
  #'
  #' Returns:
  #'
  #' - a data.frame containing the updated population
  
  ## Calculate population changes
  population.size<-latest$susceptibles+ latest$infecteds
  new.recovered <- recovery.rate * latest$infected
  new.infected <- transmission.rate * ((latest$susceptibles*latest$infected)/population.size)
  
  next.susceptibles <- latest$susceptibles - new.infected + new.recovered
  next.infecteds <- latest$infecteds + new.infected- new.recovered
  
  ## Return data frame containing next population count
  data.frame(susceptibles = next.susceptibles, 
             infecteds=next.infecteds)
}






#' #### Does the function works without any external (global) information?

library(codetools)
if (length(findGlobals(step_deterministic_SIS,
                       merge = FALSE)$variables) != 0) {
  stop(
    "Function step_deterministic_SIS() may not use global variable(s): ",
    findGlobals(step_deterministic_SIS, merge = FALSE)$variables
  )
}
