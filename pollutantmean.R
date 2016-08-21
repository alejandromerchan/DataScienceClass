pollutantmean = function(directory, pollutant, id = 1:332) {
        WD = getwd()
        location = paste(WD, "/", directory, sep = "")

        # Transform create filenmaes based on id
        files = list()
        for (i in id){
                if (i < 10){
                        filename = paste("00", i, ".csv", sep = "")
                }
                else if (i >=10 & i < 100){
                        filename = paste("0", i, ".csv", sep = "" )
                }
                else{
                        filename = paste(i, ".csv", sep = "")
                }
                
                files[[i]] = filename
        }
        
        # get pollutant
        allData = c()
        for (i in id){
                DF = read.csv(paste(location, "/", files[[i]], sep = ""))
                #print(head(DF))
                if (pollutant == "sulfate"){
                        data = c(DF$sulfate)
                }
                else{
                        data = c(DF$nitrate)
                }
                #print(head(data))
                allData = c(allData, data)
        }
        mean(allData, na.rm = T)
}