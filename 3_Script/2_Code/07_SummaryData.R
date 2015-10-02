batchSummary <- function(rDataFolder){
    insertRow <- function(existingDF, newrow, r) {
        require(dplyr)
        existingDF <- rbind(existingDF,newrow)
        existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
        row.names(existingDF) <- 1:nrow(existingDF)
        existingDF <- filter(existingDF, !is.na(FileName))
        return(existingDF)  
    }
    
    library(dplyr)
    library(tools)
    
    rateCards <- c("Ratecard_Dropship_Cebu", "Ratecard_New", "Ratecard_Old", "Ratecard_Old_Regular")
    
    InvoiceSummary <- data.frame(FileName=character(),
                                 TotalTransaction=character(),
                                 with_Threshold_Transactions=integer(),
                                 excess_Threshold_Check=integer(),
                                 Final_amount_with_Threshold=numeric(),
                                 Final_amount_excess_Threshold=numeric(),
                                 LBC_amount_excess_Threshold=numeric(),
                                 Variance_excess_Threshold=numeric())
    rawData <- NULL
    missingSKU <- 0
    invoiceCount <- 0
    load(file.path(rDataFolder,"commonVariable.RData"))
    for (iRateCard in rateCards){
        iInvoiceFolder <- file.path(inputFolder,"Invoices",iRateCard)
        for (iFile in list.files(file.path(iInvoiceFolder))){
            if(file_ext(iFile)=="csv"){
                fileName <- gsub(".csv","",iFile)
                okFile <- file.path(outputFolder,iRateCard,paste0(fileName,"_ok.csv"))
                
                currentInvoice <- read.csv(file.path(okFile),
                                           stringsAsFactors = FALSE)
                variance <- sum(currentInvoice$Variance)
                No_Manual_Check_count <- nrow(currentInvoice)
                LBC_TotalAmount <- sum(currentInvoice$Total.Amount)
                Final_amount_wThreshold <- sum(currentInvoice$FinalAmount)
                missingSKU <- missingSKU + as.integer(sum(currentInvoice$SKU_MissingDimensionData))
                if (is.null(rawData)) rawData <- currentInvoice
                else rawData <- rbind_list(rawData,currentInvoice)
                
                manualFile <- file.path(outputFolder,iRateCard,paste0(fileName,"_manual_check.csv"))
                currentInvoice <- read.csv(file.path(manualFile),
                                           stringsAsFactors = FALSE)
                variance_manual <- sum(currentInvoice$Variance)
                Manual_check_Count <- nrow(currentInvoice)
                LBC_TotalAmount_eThreshold <- sum(currentInvoice$Total.Amount)
                Variance_eThreshold <- sum(currentInvoice$Variance)
                Final_amount_eThreshold <- sum(currentInvoice$FinalAmount)
                missingSKU <- missingSKU + sum(currentInvoice$SKU_MissingDimensionData)
                newRecord <- data.frame(fileName, No_Manual_Check_count+Manual_check_Count,
                                        No_Manual_Check_count, Manual_check_Count,
                                        Final_amount_wThreshold, Final_amount_eThreshold,
                                        LBC_TotalAmount_eThreshold, Variance_eThreshold)
                colnames(newRecord) <- c("FileName", "TotalTransaction","with_Threshold_Transactions","excess_Threshold_Check",
                                         "Final_amount_with_Threshold","Final_amount_excess_Threshold",
                                         "LBC_amount_excess_Threshold","Variance_excess_Threshold")
                InvoiceSummary <- insertRow(InvoiceSummary, newRecord, 1)
                invoiceCount <- invoiceCount + 1
                if (is.null(rawData)) rawData <- currentInvoice
                else rawData <- rbind_list(rawData,currentInvoice)
            }
        }
    }
    
    write.csv(InvoiceSummary, file = file.path(outputFolder,"Summary.csv"), row.names = FALSE)
}