loadPODData <- function(rDataFolder){
    podData <- NULL 
    logString <- ""
    load(file.path(rDataFolder,"commonVariable.RData"))
    for (ifile in list.files("../../1_Input/POD_Data")) {
        tryCatch({
            if(file_ext(ifile)=="csv"){
                iFileData <- read.csv(file.path("../../1_Input/POD_Data",ifile), 
                                      header = FALSE, 
                                      stringsAsFactors = FALSE)
                iFileData <- select(iFileData, Tracking.Number=1)
                if (sum(duplicated(iFileData))>0){
                    logString <- paste0(logString,"\r\nPOD Data Error with Duplicated Value --- File: ",ifile,"\r\n")
                    logString <- paste0(logString,"Duplicated Tracking Count: ",sum(duplicated(iFileData$Tracking.Number)),
                                        "/",nrow(iFileData),"\r\n")
                    cat(paste0("\r\nPOD Data Error with Duplicated Value --- File: ",ifile,"\r\n"))
                    cat(paste0("Duplicated Tracking Count: ",sum(duplicated(iFileData$Tracking.Number)),
                               "/",nrow(iFileData),"\r\n"))
                }
                iFileData %<>% mutate(Tracking.Number=as.character(Tracking.Number))
                if (is.null(podData))
                    podData <- iFileData
                else
                    podData <- rbind_list(podData,iFileData)
            }
        },error = function(e){
            cat("\r\n")
            cat(paste0("Error in Loading POD Data: ", ifile,"\r\n"))
            cat(paste0(e,"\r\n"))
        })
    }
    
    write(logString, file.path(outputFolder,"podDataLoadingError.txt"))
    
    podData
}