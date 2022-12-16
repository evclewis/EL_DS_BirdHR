setwd("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files")

FILES <- list.files("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/cleaned/ciconia_ciconia",
                    full.names = TRUE)

DF <- list()

#Then walk through each of them sequentially
for(i in 1:length(FILES)){
  
  DATA <- read.csv(FILES[i])
  
  DATA <- DATA[,which(names(DATA) != "color_bin_start")]
  

  #Load in the tracking data
  DF[[i]] <- DATA
}

data <- do.call(rbind, DF)

test <- as.telemetry(data)

ciconia_ciconia <- data

save(ciconia_ciconia, file = "../DS_Files/Bird_Data/ciconia_ciconia.rda")




