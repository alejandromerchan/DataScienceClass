corr = function(directory, threshold = 0) {
        DF = complete(directory)
        
        # Transform create filenmaes based on id
        files = c()
        for (i in 1:nrow(DF)){
                if (i < 10){
                        filename = paste("00", i, ".csv", sep = "")
                }
                else if (i >=10 & i < 100){
                        filename = paste("0", i, ".csv", sep = "" )
                }
                else{
                        filename = paste(i, ".csv", sep = "")
                }
                
                files = c(files, filename)
        }

        
        location = paste(getwd(), "/", directory, sep = "")

        correlations = c()
        
        for (i in 1:nrow(DF)){
                if (DF$nobs[i] > threshold){
                       loc = files[i]
                        DF1 = read.csv(paste(location, "/", loc, 
                                             sep = ""))
                        DF1 = na.omit(DF1)
                        correlations = c(correlations, cor(DF1$sulfate, 
                                                           DF1$nitrate))
                }
        }
        correlations
}