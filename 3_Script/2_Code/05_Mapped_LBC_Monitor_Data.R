
loadLBCMonitoringData <- function(rDataFolder){
    
    setClass('lbcDate')
    setAs("character","lbcDate", function(from) as.POSIXct(from, format =  "%d/%m/%Y", tz = "Asia/Manila"))
    
    load(file.path(rDataFolder,"commonVariable.RData"))
    # Load LBC Monitoring Data
    lbc_monitorData <- NULL 
    for (ifile in list.files("../../1_Input/LBC")) {
        if (file_ext(ifile)=="csv"){
            iFileLBCMonitorData <- read.csv(file.path("../../1_Input/LBC",ifile), header = TRUE,
                                            col.names = c("Tracking.Number","First_Attempt_Status",
                                                          "FINAL.STATUS","Final.Status.Date"),
                                            colClasses = c("character","character","character",
                                                           "character"),
                                            stringsAsFactors = FALSE)
            if (is.null(lbc_monitorData))
                lbc_monitorData <- iFileLBCMonitorData
            else
                lbc_monitorData <- rbind_list(lbc_monitorData,iFileLBCMonitorData)
        }
    }
    
    lbc_monitorData %<>% mutate(Tracking.Number=trimws(Tracking.Number))
    
    lbc_monitorDataNarrow <- lbc_monitorData %>%
        arrange(Final.Status.Date) %>%
        group_by(Tracking.Number) %>%
        summarize(First_Attempt_Status=last(First_Attempt_Status),
                  FINAL.STATUS=last(FINAL.STATUS),
                  Final.Status.Date=last(Final.Status.Date))
    lbc_monitorDataNarrow %<>% mutate(Tracking.Number=trimws(as.character(Tracking.Number)))
    lbc_monitorDataNarrow
}