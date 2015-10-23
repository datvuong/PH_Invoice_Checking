
loadValiadtedTrackingNumber <- function(rDataFolder){
    load(file.path(rDataFolder,"commonVariable.RData"))
    
    logString <- ""
    
    validatedTrackingNumber <- NULL
    # Validated Tracking Number
    for (ifile in list.files("../../1_Input/Validated_Tracking_Number/Before Automation")) {
        if (file_ext(ifile)=="csv"){
            ifileName <- gsub(".csv","",ifile)
            validatedTrackingNumberFile <- read.csv(file.path("../../1_Input/Validated_Tracking_Number/Before Automation/",
                                                              ifile),
                                                    stringsAsFactors = FALSE)
            if(ncol(validatedTrackingNumberFile)>3){
                validatedTrackingNumberFile %<>% select(Tracking.Number=3)
                validatedTrackingNumberFile %<>% 
                    mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                    mutate(invoiceFile=ifileName) %>%
                    filter(!is.na(Tracking.Number))
            } else if(ncol(validatedTrackingNumberFile)==2){
                validatedTrackingNumberFile %<>% mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                    select(Tracking.Number=1,
                           Invoice=2)
            } else if(ncol(validatedTrackingNumberFile)==1){
                validatedTrackingNumberFile %<>% mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                    select(Tracking.Number=1) %>%
                    mutate(Invoice=ifileName)
            }
            validatedTrackingNumberFile %<>% filter(!is.na(Tracking.Number))
            validatedTrackingNumberFile %<>%
                mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                mutate(Invoice=as.character(Invoice))
            if (sum(duplicated(validatedTrackingNumberFile$Tracking.Number))>0){
                logString <- paste0(logString,"\r\nError Validated Tracking Number --- File: ",ifile,"\r\n")
                logString <- paste0(logString,"Duplicated Tracking Count: ",sum(duplicated(validatedTrackingNumberFile$Tracking.Number)),
                                    "/",nrow(validatedTrackingNumberFile),"\r\n")
                cat(paste0("\r\nError Validated Tracking Number --- File: ",ifile,"\r\n"))
                cat(paste0("Duplicated Tracking Count: ",sum(duplicated(validatedTrackingNumberFile$Tracking.Number)),
                           "/",nrow(validatedTrackingNumberFile),"\r\n"))
            }
            if (is.null(validatedTrackingNumber))
                validatedTrackingNumber <- validatedTrackingNumberFile
            else
                validatedTrackingNumber <- rbind_list(validatedTrackingNumber,validatedTrackingNumberFile)
        }
    }
    
    for (ifile in list.files("../../1_Input/Validated_Tracking_Number/After Automation/")) {
        if (file_ext(ifile)=="csv"){
            ifileName <- gsub(".csv","",ifile)
            validatedTrackingNumberFile <- read.csv(file.path("../../1_Input/Validated_Tracking_Number/After Automation/",
                                                              ifile),
                                                    stringsAsFactors = FALSE)
            if(ncol(validatedTrackingNumberFile)>3){
                validatedTrackingNumberFile %<>% select(Tracking.Number=3) %>%
                    mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                    mutate(Invoice=ifileName)
            }else if(ncol(validatedTrackingNumberFile)==2){
                validatedTrackingNumberFile %<>%
                    select(Tracking.Number=1,
                           Invoice=2) %>%
                    mutate(Tracking.Number=as.character(Tracking.Number)) 
            } else if(ncol(validatedTrackingNumberFile)==1){
                validatedTrackingNumberFile %<>% select(Tracking.Number=1) %>%
                    mutate(Tracking.Number=as.character(Tracking.Number)) %>%
                    mutate(Invoice=ifileName)
            }
            validatedTrackingNumberFile %<>% filter(!is.na(Tracking.Number))
            if (sum(duplicated(validatedTrackingNumberFile$Tracking.Number))>0){
                logString <- paste0(logString,"\r\nError Validated Tracking Number --- File: ",ifile,"\r\n")
                logString <- paste0(logString,"Duplicated Tracking Count: ",sum(duplicated(validatedTrackingNumberFile$Tracking.Number)),
                                    "/",nrow(validatedTrackingNumberFile),"\r\n")
                cat(paste0("\r\nError Validated Tracking Number --- File: ",ifile,"\r\n"))
                cat(paste0("Duplicated Tracking Count: ",sum(duplicated(validatedTrackingNumberFile$Tracking.Number)),
                           "/",nrow(validatedTrackingNumberFile),"\r\n"))
            }
            if (is.null(validatedTrackingNumber))
                validatedTrackingNumber <- validatedTrackingNumberFile
            else
                validatedTrackingNumber <- rbind_list(validatedTrackingNumber,validatedTrackingNumberFile)
        }
        
    }
    validatedTrackingNumber %<>% select(Tracking.No=Tracking.Number,
                                        OldInvoice=Invoice)
    
    write(logString, file.path(outputFolder,"validatedTrackingNumberLoadingError.txt"))
    
    validatedTrackingNumber
}