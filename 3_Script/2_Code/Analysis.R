rateCards <- c("Ratecard_Dropship_Cebu", "Ratecard_New", "Ratecard_Old", "Ratecard_Old_Regular")
variance <- 0
invoiceCount <- 0
variance_manual <- 0
oms_missing_count <- 0
for (iRateCard in rateCards){
    iInvoiceFolder <- file.path("../../2_Output/20150912/",iRateCard)
    for (iFile in list.files(file.path(iInvoiceFolder))){
        if(grepl("_ok.csv", iFile)){
            currentInvoice <- read.csv(file.path(iInvoiceFolder, iFile),
                                       stringsAsFactors = FALSE)
            currentInvoicePositiveVariance <- filter(currentInvoice, Variance>0)
            variance <- variance + sum(currentInvoicePositiveVariance$Variance)
            invoiceCount <- invoiceCount+1
        }
        if(grepl("_manual_check.csv", iFile)){
            currentInvoice <- read.csv(file.path(iInvoiceFolder, iFile),
                                       stringsAsFactors = FALSE)
            currentInvoicePositiveVariance <- filter(currentInvoice, Variance>0)
            variance_manual <- variance_manual + sum(currentInvoicePositiveVariance$Variance)
            oms_missing_count <- oms_missing_count + nrow(filter(currentInvoice, is.na(Order.Number)))
        }
    }
}

variance
variance/46.63
variance_manual
variance_manual/46.63
invoiceCount
