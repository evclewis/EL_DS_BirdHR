
# This script is a function for fitting movement models to relocation data
# It uses the best fit model to then estimate a an AKDE HR
# The fitted model, and UD are then exported for future use, and the area estimate +/- CIs is returned
# Finally, a 4 panel figure is returned for diagnostic purposes


#It requires as inputs: i) tracking data on 1 individual, as a telemetry object
#                       ii) A path to the location where the movement models will be saved
#                       iii) A path to the location where the UDs will be saved
#                       iv) A path to the location where the figures will be saved



#Written by Michael Noonan

#Last updated: Feb 7th 2019



library(ctmm)

#This function is used to fit movement models to gps data and calculate an akde home range estimate
#It requires ctmm

AKDE_HR <- function(cilla,
                    Model_path,
                    UD_path,
                    Fig_Path){
  
  message("Fitting movement model for: ", unlist(cilla@info[1]))

  #First fit the movement model using the usual workflow
  #Generate the variogram
  vg.cilla <- variogram(cilla)
  
  #fit the variogram
  GUESS <- variogram.fit(vg.cilla, interactive= FALSE)
  
  #Fit the movement models
  cilla.mods <- tryCatch(
    {
      cilla.mods <- ctmm.select(cilla,
                                CTMM = GUESS,
                                method = "pHREML",
                                control=list(method="pNewton",
                                             cores = -1),
                                trace = TRUE
                                )
    }, error=function(err) {
      message(paste("Model fitting using pHREML and optim failed, defaulting to ML"))
      
      #If pREML and/or the optimiser fail, this will try it again
      #using the standard ML without any optimisation
      
      cilla.mods <- ctmm.select(cilla,
                                CTMM = GUESS,
                                method = "ML")
      
      #The return here is essential, otherwise results aren't returned and it fails
      return(cilla.mods)
    }
    
  )
  
  
  
  
  #Save the best fit model
  
  #First get the best fit model if more than 1 have been fit
  if(class(cilla.mods) == "list") {FIT <- cilla.mods[[1]]} else {
    
    FIT <- cilla.mods
  }
  
  #Assign the file path
  ctmm.path <- file.path(Model_path, paste("Fits_", cilla@info[1], ".rda", sep = ""))
  
  
  #And save
  save(FIT, file = ctmm.path)
  
  
  
  
  ############################################
  #Calculate the AKDE home range area
  ############################################
  
  
  message("Estimating AKDE range area")

  
  #calculate the akde based on the best fit model
  cilla.akde <- akde(cilla,
                     FIT,
                     res = 50,
                     fast = FALSE)
  
  
  #Assign the file path
  akde.path <- file.path(UD_path, paste("UD_", cilla@info[1], ".rda", sep = ""))
  
  
  #And save
  save(cilla.akde, file = akde.path)
  
  
  
  
  
  ############################################
  #Calculate the KDE home range area
  ############################################
  
  
  message("Estimating KDE range area", "\n")
  
  if("COV.major" %in% names(cilla)){
  IID.FIT <- ctmm.fit(cilla, CTMM = ctmm(error = TRUE))
  } else{
    
    IID.FIT <- ctmm.fit(cilla)
  }
  
  
  #calculate the akde based on the best fit model
  cilla.kde <- akde(cilla,
                    IID.FIT, fast = FALSE)
  
  

  
  ##########################################################################################
  ##########################################################################################
  # Cross-validate the HR estimates
  ##########################################################################################
  ##########################################################################################

    
  cat("Cross-validating the range estimates")
  

  #splitdf function for splitting a dataset into two halves for cross-validation
  splitdf <- function(df, percent) {
    length <- nrow(df)
    train <- df[1:round(length*(percent/100)),]
    test <- df[round(length*(percent/100)):nrow(df),]
    list(train=train,test=test)
  }
  
  
  cilla.split <- splitdf(cilla, 50)
  
  
  #First is to check the assumption of consistent movement between T_1 and T_2
  
  #Fit the model to T_1
  message("\n", "Fitting the movement model to T_1")
  
  GUESS <- ctmm.guess(cilla.split[[1]], interactive = FALSE)
  
  if("COV.major" %in% names(cilla.split[[1]])){
    GUESS$error <- TRUE
  }
  #Turn error on for certain species
  #if(BINOMIAL == "Brachylagus_idahoensis"){
  #  GUESS$error <- TRUE
  #}
  
  
  FITS <- tryCatch(
    {
      FITS <- ctmm.select(cilla.split[[1]],
                          GUESS,
                          control=list(method = "pNewton",
                                       cores = -1),
                          method = "pHREML")
    }, error=function(err) {
      message(paste("Model fitting using pHREML and optim failed"))
      
    }
    
  )
  
  #FITS <- ctmm.select(cilla.split[[1]],
  #                    GUESS,
  #                    control=list(method = "pNewton",
  #                                 cores = -1),
  #                    method = "pHREML")
  
  
  if(class(FITS) == "list") {T_1.FIT <- FITS[[1]]} else {
    
    T_1.FIT <- FITS
  }
  
  #Fit the model to T_2
  message("Fitting the movement model to T_2")
  
  GUESS2 <- ctmm.guess(cilla.split[[2]], interactive = FALSE)
  
  if("COV.major" %in% names(cilla.split[[1]])){
    GUESS2$error <- TRUE
  }
  
  #Turn error on for certain species
  #if(BINOMIAL == "Brachylagus_idahoensis"){
  #  GUESS2$error <- TRUE
  #}
  
  
  T_2.FIT <- tryCatch(
    {
      T_2.FIT <- ctmm.fit(cilla.split[[2]],
                           T_1.FIT)
    }, error=function(err) {
      message(paste("Model fitting using pHREML and optim failed"))
      
    }
    
  )


  
  if(is.null(T_1.FIT) || is.null(T_2.FIT)){same.model <- NA} else{
  OVER <- -log(overlap(list(T_1.FIT, T_2.FIT))$CI[1,2,])
  
  same.model <- OVER[3]
  }
  #if(OVER[3] <= 0.05){
  #same.model <- "Y"} else{same.model <- "N"}
  
  
  #Calculate the HR estimate using the desired estimator
  message("Cross-validating the akde and kde home range estimates", "\n")
  
  if(is.null(T_1.FIT) || is.null(T_2.FIT)){
    
    #Fill in a few NAs for storage in case of failures
    kde.percent <- rep(NA, 3)
    akde.percent <- rep(NA, 3)
    thinned.kde.percent <- rep(NA, 3)
    
    } else {
  ##### AKDE HR estimate on the first half of the data
  AKDE.HR.estimate <- akde(cilla.split[[1]],
                           T_1.FIT,
                           fast = FALSE)#,
  #weights = TRUE,
  #dt = median(diff(data[[1]]$t))/4)
    }
  
  
  
  
  ##### KDE HR estimate on the first half of the data

  if("COV.major" %in% names(cilla.split[[1]])){
    T_1.FIT.IID <- ctmm.fit(cilla.split[[1]], CTMM = ctmm(error = TRUE))
  } else {
    T_1.FIT.IID <- ctmm.fit(cilla.split[[1]])
  }
  
  
  KDE.HR.estimate <- akde(cilla.split[[1]],
                          T_1.FIT.IID, fast = FALSE)
  

  #Calculate how many points are in the AKDE HR estimate
  AKDE.contour <- SpatialPolygonsDataFrame.UD(AKDE.HR.estimate)
  
  
  akde.res <- unname(GISTools:::poly.counts(SpatialPoints.telemetry(cilla.split[[2]]),
                                            AKDE.contour))
  
  akde.percent <- round(akde.res/nrow(cilla.split[[2]])*100, 2)
  
  
  #Calculate how many points are in the KDE HR estimate
  KDE.contour <- SpatialPolygonsDataFrame.UD(KDE.HR.estimate)
  
  
  kde.res <- unname(GISTools:::poly.counts(SpatialPoints.telemetry(cilla.split[[2]]),
                                           KDE.contour))
  
  kde.percent <- round(kde.res/nrow(cilla.split[[2]])*100, 2)
  
  
  
  

 # Removed everything related to thinning here 
  
  ############################################
  #Plot all the results and save them as a pdf
  ############################################
  
  message("\n", "Saving the figures")
  
  #Assign the file path and name for saving the results
  fig.path <- file.path(Fig_Path,
                      paste("ctmm_", cilla@info[1], ".png", sep = ""))
  
  #Save the graphic device's output as a pdf
  png(file=fig.path,
      type="cairo",
      units = "in",
      width = 6.81, height = 8,
      pointsize = 10,
      res = 800) #
  
  #Set the par to plot all on same screen
  par(mfrow=c(3,2),
      mgp = c(1.5, 0.5, 0),
      oma=c(0,0,0,0),
      mar=c(3,3,2,2)) 
  
  
  
  #Plot the zoomed in variogram 
  tryCatch(
    {
      plot(vg.cilla, CTMM=FIT,family="serif", fraction = 0.005) 
      title(main = "a)", family = "serif", cex.main = 1, adj = 0)
      
    }, error=function(err) {
      
      plot(vg.cilla, CTMM=FIT,family="serif", fraction = 0.05) 
      title(main = "a)", family = "serif", cex.main = 1, adj = 0)
      
    }
    
  )
  
  
  
  #Plot the variogram of the full time series
  plot(vg.cilla, CTMM=FIT,family="serif") 
  title(main = "b)", family = "serif", cex.main = 1, adj = 0)
  
  
  #Plot a check for outliers
  OUTLIERS <- outlie(cilla, family="serif")
  title(main = "c)", family = "serif", cex.main = 1, adj = 0)
  
  
  plot(OUTLIERS, family="serif")
  title(main = "d)", family = "serif", cex.main = 1, adj = 0)
  
  
  #Plot the AKDE range estimate, with the relocation data, coloured by time
  #Create a function that scales colours between red and blue
  rbPal <- colorRampPalette(c('#FF0000','#046C9A'))
  #Then create a variable that scales from red to blue between the two times
  cilla$Col <- rbPal(nrow(cilla))[as.numeric(cut(cilla$t,breaks = nrow(cilla)))]
  
  
  #Plot of the range estimate
  plot(cilla,
       UD=cilla.akde,
       col.grid = NA,
       family = "serif",
       pch = 20,
       cex = 0.2,
       col.DF = "#669543",
       col = cilla$Col,
       labels=FALSE)
  
  title(main = "e)", family = "serif", cex.main = 1, adj = 0)
  
  
  if(exists("KDE.HR.estimate")){
  #Plot of the range estimate
  plot(cilla.split,
       UD=list(KDE.HR.estimate, AKDE.HR.estimate),
       col.level = c("purple", "black"),
       col.grid = NA,
       family = "serif",
       pch = 20,
       cex = 0.2,
       col.DF = "#669543",
       col = c('#FF0000','#046C9A'),
       labels=FALSE)
  
  TITLE <- paste("KDE 95% CV:",  round(kde.percent[2], 2), "   -   AKDE 95% CV:",  round(akde.percent[2],2))
  title(main = "f)", family = "serif", cex.main = 1, adj = 0)
  title(main = TITLE, family = "serif", cex.main = 0.8)
  }
  
  dev.off()
  
  
  
  ##########################################################################################
  ##########################################################################################
  # Export all of the results
  ##########################################################################################
  ##########################################################################################
  
  res <- as.data.frame(BINOMIAL)
  
  res$ID <- cilla@info$identity
  
  res$Year <- median(as.numeric(format(as.Date(cilla$timestamp),"%Y")))
  
  res$Lat <- median(cilla$latitude)
  
  res$Long <- median(cilla$longitude)
  
  res$Frequency <- median(diff(cilla$t))/60
  
  res$Duration <- (tail(cilla$t,1) - head(cilla$t,1))/60/60/24
  
  res$n <- nrow(cilla)
  
  res$N_area <- summary(FIT)$DOF["area"]
  
  res$Model <- summary(FIT)$name
  
  #Get tau_p
  res$tau_p <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI[2,2]} else {NA}
  res$tau_p_min <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI[2,1]} else {NA}
  res$tau_p_max <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI[2,3]} else {NA}
  if(grepl("OUf", summary(FIT)$name, fixed = TRUE)){res$tau_p_var <- FIT$COV["tau","tau"]} else if(nrow(summary(FIT, units = FALSE)$CI) >= 2){
    res$tau_p_var <- FIT$COV["tau position","tau position"]} else {NA}
  
  
  
  #Get tau_v
  res$tau_v <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[3,2]} else {NA}
  res$tau_v_min <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[3,1]} else {NA}
  res$tau_v_max <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[3,3]} else {NA}
  res$tau_v_var <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){FIT$COV["tau velocity","tau velocity"]} else {NA}
  
  #Get speed
  res$speed <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[4,2]} else {NA}
  res$speed_min <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[4,1]} else {NA}
  res$speed_max <- if(grepl("OUF", summary(FIT)$name, fixed = TRUE)){summary(FIT, units = FALSE)$CI[4,3]} else {NA}
 
  #Get diffusion
  res$diffusion <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI["diffusion (square meters/second)",2]} else {NA}
  res$diffusion_min <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI["diffusion (square meters/second)",1]} else {NA}
  res$diffusion_max <- if(nrow(summary(FIT, units = FALSE)$CI) >= 2){summary(FIT, units = FALSE)$CI["diffusion (square meters/second)",3]} else {NA}
  
  res$"kde.min" <- summary(cilla.kde, units = FALSE)$CI[1]
  
  res$"kde.ml"<- summary(cilla.kde, units = FALSE)$CI[2]
  
  res$"kde.max" <- summary(cilla.kde, units = FALSE)$CI[3]
  
  res$"akde.min" <- summary(cilla.akde, units = FALSE)$CI[1]
  
  res$"akde.ml"<- summary(cilla.akde, units = FALSE)$CI[2]
  
  res$"akde.max" <- summary(cilla.akde, units = FALSE)$CI[3]
  
  res$same.model <- same.model
  
  res$CV_N_area <- if(is.null(T_1.FIT) || is.null(T_2.FIT)){res$CV_N_area <- NA} else{ summary(T_1.FIT)$DOF[2]}
  
  res$"kde.95CV.min" <- kde.percent[1]
  
  res$"kde.95CV.ml"<- kde.percent[2]
  
  res$"kde.95CV.max" <- kde.percent[3]
  
  res$"akde.95CV.min" <- akde.percent[1]
  
  res$"akde.95CV.ml"<- akde.percent[2]
  
  res$"akde.95CV.max" <- akde.percent[3]
  
  res$"full.n" <- nrow(cilla.split[[1]])
                       
  res

    
}



