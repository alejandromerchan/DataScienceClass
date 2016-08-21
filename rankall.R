rankall = function(outcome, ranking = 1){
        # Read outcome data
        outcomeData = read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character", 
                               na.strings = "Not Available",
                               stringsAsFactors = FALSE)
        
        outcomeData[, 11] = as.numeric(outcomeData[ , 11])
        outcomeData[, 17] = as.numeric(outcomeData[ , 17])
        outcomeData[, 23] = as.numeric(outcomeData[ , 23])
        
        # Create a vector for the states
        states = unique(outcomeData$State)
        
        # check if the outcome is valid
        if (outcome == "heart attack"){
                outcomeData = outcomeData[, c(2, 7, 11)]
        } else if (outcome == "heart failure") {
                outcomeData = outcomeData[, c(2, 7, 17)]
        } else if ( outcome == "pneumonia") {
                outcomeData = outcomeData[, c(2, 7, 23)]
        } else {stop("Invalid outcome")
        }
        
        statesOrdered = data.frame()
        result = c()
        for (str in states){
                if (ranking == "best"){
                        stateOrder = outcomeData[outcomeData$State == str, ]
                        index = with(stateOrder, order(stateOrder[,3], stateOrder[1]))
                        stateOrder = stateOrder[index,]
                        stateOrder$Rank = c(1:nrow(stateOrder))
                        result = c(result, stateOrder[1, 1])

                } else if (ranking == "worst") {
                        stateOrder = outcomeData[outcomeData$State == str, ]
                        index = with(stateOrder, order(stateOrder[,3], stateOrder[1]))
                        stateOrder = stateOrder[index,]
                        stateOrder$Rank = c(1:nrow(stateOrder))
                        stateOrder = na.omit(stateOrder)
                        result = c(result, stateOrder[stateOrder$Rank == nrow(stateOrder), 1])
                } 
                else {                    
                        stateOrder = outcomeData[outcomeData$State == str, ]
                        index = with(stateOrder, order(stateOrder[,3], stateOrder[1]))
                        stateOrder = stateOrder[index,]
                        stateOrder$Rank = c(1:nrow(stateOrder))
                        stateOrder = na.omit(stateOrder)
                        result = c(result, stateOrder[ranking, 1])
                }
        }
        final.ranking = data.frame(cbind(hospital = result, state = states))
        index = with(final.ranking, order(final.ranking[,2]))
        final.ranking = final.ranking[index,]
        final.ranking
}