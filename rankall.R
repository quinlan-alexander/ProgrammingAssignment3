
validState <- function(state, validStates) {
  if (state  %in% validStates) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

validOutcome <- function(outcome, validOutcomes) {
  if (outcome %in% validOutcomes) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

rankhospital <- function(outcome_data, validOutcomes, state, outcome, num = "best") {

  ## Check that state is valid
  validStates <- unique(outcome_data$State)
  if (!validState(state, validStates)) {
    stop("invalid state")
  }
  
  ## Create a data frame to map outcome to column number
  outcomeColumnDf <- data.frame(Outcome = validOutcomes, 
                                Col = c(11, 17, 23))
  
  ## Return hospital name in that state with lowest 30-day death rate
  ##
  stateDf <- outcome_data[outcome_data$State == state, ]
  colNum <- outcomeColumnDf[outcomeColumnDf$Outcome == outcome, 2]
  stateDf <- stateDf[order(stateDf[, colNum], stateDf$Hospital.Name), ]
  stateDf <- stateDf[complete.cases(stateDf[,colNum]),]
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(stateDf)
  } else {
    if (num > nrow(stateDf)) {
      return("NA")
    }
  }
  
  stateDf[num,]$Hospital.Name
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## setwd("/Users/ba25714/cousera/RProg/Week4")
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Coerce data
  suppressWarnings(outcome_data[, 11] <- as.numeric(outcome_data[, 11]))
  suppressWarnings(outcome_data[, 17] <- as.numeric(outcome_data[, 17]))
  suppressWarnings(outcome_data[, 23] <- as.numeric(outcome_data[, 23]))

  ## Check that outcome is valid
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!validOutcome(outcome, validOutcomes)) {
    stop("invalid outcome")
  }
  
  rankedHospital <- character(0)
  ## Loop over all the states to build a results data frame
  validStates <- unique(outcome_data$State)
  validStates <- sort(validStates)
  for (state in validStates) {
    ## Append hospital name to states character vector
    rankhospital(outcome_data, validOutcomes, state, outcome, num)
    rankedHospital <- c(rankedHospital, rankhospital(outcome_data, validOutcomes, state, outcome, num))
  }
  ## Return data frame (hospital, state)
  return(data.frame(hospital = rankedHospital, state = validStates))
}