# Function created for R Programming class in Coursera as part of the Data Science Specialization.
# This function was part of programming homework for Week 4 and data was provided by instructors

best = function(state, outcome){
        # Read outcome data
        outcomeData = read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character", 
                           na.strings = "Not Available",
                           stringsAsFactors = FALSE)
        
        # Data starts out as factor, needs to be transformed to numeric
        outcomeData[, 11] = as.numeric(outcomeData[ , 11])
        outcomeData[, 17] = as.numeric(outcomeData[ , 17])
        outcomeData[, 23] = as.numeric(outcomeData[ , 23])
        
        # Check for wrong conditions
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
        
        # Creates a list of hospitals for each state
        if(state %in% states){
                stateList = data.frame()
                for(i in 1:nrow(outcomeData)){
                        if(state == outcomeData$State[i]){
                                stateList = rbind(stateList, outcomeData[i,])
                        }
                }
         # Check for wrong state in case error on input
        } else {stop("Invalid state") }

        # Clean final data frame
        stateList = na.omit(stateList)
        # Organizes the data frame by smaller number of deaths
        index = with(stateList, order(stateList[,3], stateList[1]))
        # Output is the name of the best hospital
        stateList[index,] [1,1]
        
}
