knitr::opts_chunk$set(eval = FALSE)
library(ctmm)


setwd("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/HR_bias_birds/scripts")
source("HR_Function.R")


FILES <- list.files("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Bird_Data",
                    full.names = TRUE)

#Then walk through each of them sequentially
for(i in 40:length(FILES)){
  
  #Load in the tracking data
  DF <- get(load((FILES[i])))
  DATA <- as.telemetry(DF)
  
  #Store the species binomial
  
  BINOMIAL <- gsub("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Bird_Data/", "", gsub(".rda","", FILES[i]), fixed = TRUE)
  
  
  message("Working on the tracking data on ", BINOMIAL)

  #Create the paths to the file directory
  dir.create(paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, sep = ""))
  dir.create(paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/Fits", sep = ""))
  Model_path = paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/Fits", sep = "")
  
  dir.create(paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/UDs", sep = ""))
  UD_path = paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/UDs", sep = "")
  
  dir.create(paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/Figs", sep = ""))
  Fig_Path = paste("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/", BINOMIAL, "/Figs", sep = "")
  
  
  #Then walk through each individual sequentially
  for(j in 1:length(DATA)){
    
    cat("Working on individual ", j, " of ", length(DATA), "\n")
    
    #Extract the current individual
    
    if(class(DATA) == "list") {cilla <- DATA[[j]]} else {
      
      cilla <- DATA
    }

    
          
    #RESULTS <- tryCatch(
        
        #{
          
          RESULTS <- AKDE_HR(cilla,
                             Model_path = Model_path,
                             UD_path = UD_path,
                             Fig_Path = Fig_Path)
        
        
        #}, error=function(err) {
         # message(cat("Home range estimation failed, returning NAs \n"))
          
         # RESULTS <- as.data.frame(t(unlist(c(BINOMIAL,
                                              #cilla@info[1],
                                             #rep(NA, 32)))))
          
         # return(RESULTS)
       # }
     # )
      
      

      
      
    if(i ==1 && j == 1){
      
      write.table(RESULTS,
                  file = "C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/HR_birds_data.csv",
                  row.names=FALSE,
                  col.names=TRUE,
                  sep=",",
                  append=TRUE)
      
    } else {
      
      write.table(RESULTS,
                  file = "C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final/HR_birds_data.csv",
                  row.names=FALSE,
                  col.names=FALSE,
                  sep=",",
                  append=TRUE)
      
    }
    
    
    
    
  }#Closes the loop that runs over the as.telemetry object
  
}#Closes the loop that runs over the individual datasets



