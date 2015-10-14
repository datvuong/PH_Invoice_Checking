library(methods)
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(reshape2)))
suppressWarnings(suppressMessages(library(tools)))

insertRow <- function(existingDF, newrow, r) {
    require(dplyr)
    existingDF <- rbind(existingDF,newrow)
    existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
    row.names(existingDF) <- 1:nrow(existingDF)
    existingDF <- filter(existingDF, !is.na(JobName))
    return(existingDF)  
}

trackingLog <- function(trackingLogDF, jobName, runID, step, activity, description){
    newRecord <- data.frame(jobName, runID, step, activity, Sys.time(), description)
    colnames(newRecord) <- c("JobName", "RunID","Step","Activity","Timestamp","Description")
    trackingLogDF <- insertRow(trackingLogDF, newRecord, 1)
    
    save(trackingLogDF, file = paste0("../3_Log/",jobName,"_",runID,".RData"))
    
    trackingLogDF
}

jobName <- "PH_Invoice_Check_Automation"
runID <- format(Sys.time(),"%Y%m%d%H%M")

trackingLogDF <- data.frame(JobName=character(),
                            RunID=character(),
                            Step=character(),
                            Activity=character(),
                            Timestamp=as.POSIXct(character()),
                            Description=character())

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "Initial_Setup", "Start", "Step Note")

dateFolder <- readChar("../../1_Input/RunningFolder.txt",
                       file.info("../../1_Input/RunningFolder.txt")$size)
cat("\r\nRunning Folder: ",dateFolder,"\r\n")
inputFolder <- file.path("../../1_Input", dateFolder)
outputFolder <- file.path("../../2_Output", dateFolder)
dir.create(outputFolder, showWarnings = FALSE)
rDataFolder <- file.path("../4_RData",dateFolder)
dir.create(rDataFolder, showWarnings = FALSE)

save(dateFolder,inputFolder,outputFolder,rDataFolder,
     file = file.path(rDataFolder,"commonVariable.RData"))

source("../2_Code/01_Loading_Manual_Data.R")
source("../2_Code/02_processOMSData.R")
source("../2_Code/03_RateCardVerified.R")
source("../2_Code/03_RateCardVerified_2.R")
source("../2_Code/04_Duplicated_Invoice_Check.R")
source("../2_Code/05_Mapped_LBC_Monitor_Data.R")
source("../2_Code/06_POD_Data.R")
source("../2_Code/07_SummaryData.R")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "Initial_Setup", "End", "Step Note")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "01_Load_OMS_Data", "Start", "Step Note")
cat("Loading OMS Data...\r\n")
dateUpdate <- NULL
for (file in list.files("../../1_Input/OMS")){
    dateModify <- file.mtime(file.path("../../1_Input/OMS",file))
    if (is.null(dateUpdate)){
        dateUpdate <- dateModify
    } else if (dateUpdate < dateModify) dateUpdate <- dateModify
}
if(file.exists(file.path("../4_RData", "omsTrackingData.RData")) &
   file.mtime(file.path("../4_RData", "omsTrackingData.RData")) > dateUpdate){
    load(file.path("../4_RData", "omsTrackingData.RData"))
}else{
    omsTrackingData <- loadOMSData(rDataFolder)
    save(omsTrackingData, file=file.path("../4_RData", "omsTrackingData.RData"))
}
trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "01_Load_OMS_Data", "End", "Step Note")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "02_Load_Validated_TrackingNumber", "Start", "Step Note")
cat("Loading Validated Tracking Number...\r\n")
validatedTrackingNumber <- loadValiadtedTrackingNumber(rDataFolder)
trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "02_Load_Validated_TrackingNumber", "End", "Step Note")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "03_Load_LBC_Data", "Start", "Step Note")
cat("Loading LBC Data...\r\n")
lbcMonitoringData <- loadLBCMonitoringData(rDataFolder)
trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "03_Load_LBC_Data", "End", "Step Note")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "04_Load_POD_Data", "Start", "Step Note")
cat("Loading POD Data...\r\n")
podData <- loadPODData(rDataFolder)
trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "04_Load_POD_Data", "End", "Step Note")

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "04_Processing_Invoice", "Start", "Step Note")
cat("Processing Invoices...\r\n")
rateCards <- c("Ratecard_Dropship_Cebu", "Ratecard_New", "Ratecard_Old", "Ratecard_Old_Regular")
totalInvoice <- 0
for (iRateCard in rateCards){
    iInvoiceFolder <- file.path(inputFolder,"Invoices",iRateCard)
    totalInvoice <- totalInvoice + sum(grepl(".csv",list.files(file.path(iInvoiceFolder))))
}
i <- 0
totalStep <- totalInvoice
pb <- txtProgressBar(min=0, max=totalStep, style = 3)
setTxtProgressBar(pb, i)
manualCheck <- 0
totalCheck <- 0
totalVariance <- 0

inputParams <- read.csv(file.path(inputFolder,"Running_params.csv"), stringsAsFactors = FALSE,
                        row.names = 1)
total_Amount_Percentage_Threshold <- inputParams["Total_Amount_Percentage_Variance",]
Total_Amount_Variance_Threshold <- inputParams["Total_Amount_Variance",]

OldMethod <- "No"
if (file.exists(file.path(inputFolder,"Old_Running_params.csv"))){
    OldParams <- read.csv(file.path(inputFolder,"Old_Running_params.csv"), stringsAsFactors = FALSE,
                          row.names = 1)
    OldMethod <- OldParams["Old Method",]
    if (OldMethod=="Yes"){
        Weight_different_Threshold <- as.numeric(OldParams["Weight_different",])
        Absolute_weight_threshold <- as.numeric(OldParams["Absolute_Weight_Threshold",])
        Total_Amount_Threshold <- as.numeric(OldParams["Total_Amount_Variance",])
        Total_Amount_Variance_Threshold <- 0
    }
}

if (OldMethod=="Yes"){
    cat("\r\nCalculating using Old Methodology")
    cat(paste0("\r\nAbsolute_weight_threshold: ",Absolute_weight_threshold))
    cat(paste0("\r\nTotal_Amount_Threshold: ",Total_Amount_Threshold))
    cat(paste0("\r\nWeight_different_Threshold: ",Weight_different_Threshold,"\r\n"))
}else{
    cat("\r\nCalculating using New Methodology")
    cat(paste0("\r\nTotal_Amount_Percentage_Threshold: ",total_Amount_Percentage_Threshold))
    cat(paste0("\r\nTotal_Amount_Variance_Threshold: ",Total_Amount_Variance_Threshold,"\r\n"))
}

for (iRateCard in rateCards){
    iInvoiceFolder <- file.path(inputFolder,"Invoices",iRateCard)
    for (iFile in list.files(file.path(iInvoiceFolder))){
        if (file_ext(iFile)=="csv"){
            iFileName <- gsub(".csv", "", iFile)
            dir.create(file.path(outputFolder,iRateCard), showWarnings = FALSE)
            
            invoiceData <- loadInvoiceData(file.path(iInvoiceFolder, iFile))
            invoiceData %<>% mutate(Tracking.No=as.character(Tracking.No))
            InvoiceDataOMSMapped <- left_join(invoiceData, omsTrackingData,
                                              by=(c("Tracking.No"="Tracking.Number")))
            InvoiceDataOMSMapped %<>%
                mutate(OMSIslandMissing=ifelse(is.na(Island),"Yes","No"))
            if(OldMethod=="Yes"){
                InvoiceDataRateCalculated <- rateCardCalcuateOld(rDataFolder,InvoiceDataOMSMapped, iRateCard,
                                                                 Weight_different_Threshold,Absolute_weight_threshold, Total_Amount_Threshold)
            }else{
                InvoiceDataRateCalculated <- rateCardCalcuate(rDataFolder,InvoiceDataOMSMapped, iRateCard,
                                                              total_Amount_Percentage_Threshold, Total_Amount_Variance_Threshold)
            }
            
            save(InvoiceDataRateCalculated, file=file.path(rDataFolder,"SampleOutput.RData"))
            
            InvoiceDataRateCalculated <- left_join(InvoiceDataRateCalculated, validatedTrackingNumber,
                                                   by=("Tracking.No"))
            InvoiceDataRateCalculated %<>%
                mutate(Duplicated=ifelse(!is.na(OldInvoice),
                                         "Yes","No"))
            
            InvoiceDataRateCalculated <- left_join(InvoiceDataRateCalculated, lbcMonitoringData,
                                                   by=c("Tracking.No"="Tracking.Number"))
            
            if(OldMethod=="Yes"){
                InvoiceDataRateCalculated %<>%
                    mutate(ManualCheck=ifelse(is.na(Order.Number) | Duplicated=="Yes" |
                                                  (Is.COD=="Yes" & Payment.Method!="CashOnDelivery") |
                                                  (InvalidIsland=="Yes" & Variance>Total_Amount_Variance_Threshold) |
                                                  Variance>Total_Amount_Variance_Threshold,"Yes","No"))
            }else{
                InvoiceDataRateCalculated %<>%
                    mutate(ManualCheck=ifelse(is.na(Order.Number) | Duplicated=="Yes" |
                                                  (Variance>(total_Amount_Percentage_Threshold*TotalAmount_cal) &
                                                       Variance>Total_Amount_Variance_Threshold),"Yes","No"))
            }
            
            Final <- InvoiceDataRateCalculated %>%
                select(Date
                       ,Package.Code
                       ,Tracking.No
                       ,Customer.Name=Consignee
                       ,Customer.Address=Address
                       ,Volume.Description
                       ,Length
                       ,Width
                       ,Height
                       ,DimWeight
                       ,Actual.Weight
                       ,Invoice.Chargeable.Weight
                       ,Declared.Value
                       ,Valuation
                       ,Freight
                       ,ODZ.Fee
                       ,Is.COD
                       ,Collection.Fee
                       ,VAT
                       ,Total.Amount
                       ,Package.Number=Content
                       ,AREACode
                       ,RateCard
                       ,Final.Caluated.Weight
                       ,Valuation
                       ,ValuationFee_cal
                       ,CollectionFee_cal
                       ,FreightCost_cal
                       ,ODZ.Fee_cal
                       ,VAT_cal
                       ,Lazada_TotalAmount_cal=TotalAmount_cal
                       ,Variance
                       ,FinalAmount
                       ,Service.Level
                       ,First_Attempt_Status
                       ,FINAL.STATUS
                       ,Final.Status.Date
                       ,Order.Number
                       ,ShippedDate
                       ,DeliveredDate
                       ,Payment.Method
                       ,Shipping.City
                       ,Island
                       ,SKUs
                       ,SKUs_Description
                       ,TotalSKUWeight
                       ,SKU_MissingDimensionData
                       ,Duplicated
                       ,OldInvoice
                       ,ManualCheck
                       ,InvalidIsland
                       ,OMSIslandMissing)
            
            Final %<>% 
                mutate(POD_Scanned=ifelse(Tracking.No %in% podData$Tracking.Number,
                                          "Yes","No"))
            
            write.csv(Final, file = file.path(outputFolder,iRateCard,paste0(iFileName,".csv")),
                      row.names = FALSE)
            
            i <- i + 1
            setTxtProgressBar(pb, i)
            trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                                         "04_Processing_Invoice", "Running", paste0("Invoice: ", iFileName))
        }
    }
}

trackingLogDF <- trackingLog(trackingLogDF, jobName, runID,
                             "04_Processing_Invoice", "End", "Step Note")

batchSummary(rDataFolder)