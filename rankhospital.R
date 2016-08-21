rankhospital = function(state, outcome, ranking = 1){
        # Read outcome data
        outcomeData = read.csv("outcome-of-care-measures.csv", 
                               colClasses = "character", 
                               na.strings = "Not Available",
                               stringsAsFactors = FALSE)
        
        outcomeData[, 11] = as.numeric(outcomeData[ , 11])
        outcomeData[, 17] = as.numeric(outcomeData[ , 17])
        outcomeData[, 23] = as.numeric(outcomeData[ , 23])
        
        if (outcome == "heart attack"){
                outcomeData = outcomeData[, c(2, 7, 11)]
        } else if (outcome == "heart failure") {
                outcomeData = outcomeData[, c(2, 7, 17)]
        } else if ( outcome == "pneumonia") {
                outcomeData = outcomeData[, c(2, 7, 23)]
        } else {stop("Invalid outcome")
        }
        
        # Get list of states from document
        states = unique(outcomeData$State)
        
        if(state %in% states){
                stateList = data.frame()
                for(i in 1:nrow(outcomeData)){
                        if(state == outcomeData$State[i]){
                                stateList = rbind(stateList, outcomeData[i,])
                        }
                }
        } else {stop("Invalid state") }
        
        
        stateList = na.omit(stateList)
        index = with(stateList, order(stateList[,3], stateList[1]))
        stateList = stateList[index,]
        stateList$Rank = c(1:nrow(stateList))
        if (ranking == "best"){
                stateList[1, 1]
        } else if (ranking == "worst") {
                stateList[nrow(stateList), 1]
        } else {stateList[ranking, 1]}
        
}