complete = function(directory, id = 1:332){
        WD = getwd()
        location = paste(WD, "/", directory, sep = "")
        
        # Transform create filenmaes based on id
        files = c()
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
                
                files = c(files, filename)
        }
        
        # get pollutant
        ids = c()
        nobs = c()
        for (i in 1:length(id)){
                DF = read.csv(paste(location, "/", files[i], sep = ""))
                ids = c(ids, i)
                nobs = c(nobs, sum(complete.cases(DF)))
        }
        total = data.frame(cbind(ids, nobs))
        total
}