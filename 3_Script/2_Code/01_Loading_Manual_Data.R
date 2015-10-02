loadInvoiceData <- function(filePath){
    
    InvoiceData <- read.csv(filePath, stringsAsFactors = FALSE)
    
    InvoiceData %<>% filter(!is.na(Tracking.No.))
    
    InvoiceData %<>% mutate(Tracking.No.=trimws(Tracking.No.)) %>%
        mutate(Content=trimws(toupper(Content)))
    InvoiceDataTrial <- cbind(InvoiceData,
                              colsplit(InvoiceData$Volume.Description, 
                                       pattern = "(x|X)", names = c("Length", "Width", "Height")))
    
    InvoiceDataTrial %<>%
        mutate(Length=as.numeric(trimws(Length)),
               Width=as.numeric(trimws(Width)),
               Height=as.numeric(trimws(Height))) %>%
        mutate(DimWeight=Length*Width*Height/3500,
               DimWeight=as.numeric(ifelse(is.na(DimWeight),0,DimWeight)),
               Actual.Weight=as.numeric(Actual.Weight),
               FinalWeight=as.numeric(ifelse(Actual.Weight>ceiling(DimWeight),
                                             Actual.Weight,ceiling(DimWeight))))
    
    InvoiceDataRevised <- InvoiceDataTrial %>%
        select(Date
               ,Package.Code=PROD
               ,Tracking.No=Tracking.No.
               ,Consignee=Consignee
               ,Address=ADDRESS
               ,Volume.Description
               ,Length
               ,Width
               ,Height
               ,DimWeight
               ,Actual.Weight
               ,Final.Caluated.Weight=FinalWeight
               ,Invoice.Chargeable.Weight=Chargeable.Weight
               ,Declared.Value
               ,Valuation
               ,Freight
               ,ODZ.Fee=Odz.Fee
               ,Is.COD=IF.COD
               ,Collection.Fee
               ,VAT
               ,Total.Amount=TOTAL.AMOUNT
               ,Content
        )
    
    InvoiceDataRevised
}