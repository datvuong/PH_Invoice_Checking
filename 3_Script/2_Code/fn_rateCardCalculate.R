
rateCardCalculate <- function(rDataFolder,InvoiceDataOrderMapped, rateCard, total_Amount_Percentage_Threshold, Total_Amount_Variance_Threshold){
    load(file.path(rDataFolder,"commonVariable.RData"))
    
    # Load Area Code Data
    AreaCodeMap <- read.csv(file.path(inputFolder,"ReferenceData/AREA_CODE.csv"),
                            stringsAsFactors = FALSE, row.names = 1)
    
    IslandCodeMap <- read.csv(file.path(inputFolder,"ReferenceData/AREA_CODE.csv"),
                            stringsAsFactors = FALSE, row.names = 4)
    
    # Load Rate Card Data
    rateCard <- read.csv(file.path(inputFolder,"ReferenceData", paste0(rateCard,".csv")),
                         stringsAsFactors = FALSE, row.names = 1)
    h <- function(w) if( any( grepl( "In numericnumericConversion", w) ) | any( grepl( "as.numeric", w) ) ) invokeRestart( "muffleWarning" )
    numericConversion <- function(input){
        withCallingHandlers(as.numeric(trimws(gsub(",","",input))), warning = h)
    }
    InvoiceDataOrderMapped %<>%
        mutate(Declared.Value=numericConversion(Declared.Value),
               Valuation=numericConversion(Valuation),
               Collection.Fee=numericConversion(Collection.Fee),
               Freight=numericConversion(Freight),
               ODZ.Fee=numericConversion(ODZ.Fee),
               VAT=numericConversion(VAT),
               Total.Amount=numericConversion(Total.Amount)) %>%
        mutate(Declared.Value=ifelse(is.na(Declared.Value),0,Declared.Value),
               Valuation=ifelse(is.na(Valuation),0,Valuation),
               Collection.Fee=ifelse(is.na(Collection.Fee),0,Collection.Fee),
               Freight=ifelse(is.na(Freight),0,Freight),
               ODZ.Fee=ifelse(is.na(ODZ.Fee),0,ODZ.Fee),
               VAT=ifelse(is.na(VAT),0,VAT),
               Total.Amount=ifelse(is.na(Total.Amount),0,Total.Amount),
               Is.COD=trimws(Is.COD))
    # Apply calculation to verify the Fee provided by 3PL
    InvoiceDataOrderMapped %<>%
        mutate(AREA_index=regexpr("\\-[^\\-]*$", Package.Code)) %>%
        mutate(AREA=trimws(substr(Package.Code, AREA_index+1, AREA_index+10))) %>%
        mutate(AREA=ifelse(AREA=="MMB","GMA",AREA)) %>%
        mutate(AREACode=AreaCodeMap[AREA,]$CODE) %>%
        mutate(IslandCode=ifelse(is.na(Island),AREACode,IslandCodeMap[Island,]$CODE)) %>%
        mutate(InvalidIsland=ifelse(IslandCode!=AREACode,"Yes","No")) %>%
        mutate(AREACode=IslandCode) %>%
        mutate(Package.Code=trimws(Package.Code)) %>%
        mutate(RateCard_Index=regexpr(" ", Package.Code)) %>%
        mutate(RateCard=trimws(substr(Package.Code, RateCard_Index, AREA_index-1))) %>%
        mutate(RateCard=gsub("-", "", RateCard)) %>%
        mutate(ValuationFee_cal=as.numeric(0.005*Declared.Value)) %>%
        mutate(CollectionFee_cal=as.numeric(ifelse(Is.COD!="YES",0,
                                                   ifelse(Declared.Value<=2500,0.01*Declared.Value,
                                                          25+0.0075*(Declared.Value-2500))))) %>%
        mutate(Final.Caluated.Weight=ceiling(TotalSKUWeight)) %>%
        mutate(Final.Caluated.Weight=ifelse(is.na(Final.Caluated.Weight),
                                            ifelse(Actual.Weight>ceiling(DimWeight),
                                                   Actual.Weight,ceiling(DimWeight)),Final.Caluated.Weight)) %>%
        mutate(Succeding.Kilos=ifelse(Final.Caluated.Weight>3,Final.Caluated.Weight-3,0)) %>%
        mutate(FreightCost_cal=as.numeric(ifelse(RateCard=="TPS",rateCard["TPS",IslandCode],
                                                 ifelse(RateCard=="TPL",rateCard["TPL",IslandCode],
                                                        rateCard["D1 - 1st 3 kilos",IslandCode] + 
                                                            Succeding.Kilos*rateCard["D1 - Succeding Kilos",IslandCode])))) %>%
        mutate(ODZ.Fee_cal=as.numeric(ifelse(ODZ.Fee==0 | 
                                                 (is.na(Service.Level) | Service.Level=="SA"),0,700))) %>%
        mutate(VAT_cal=0.12*(ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal)) %>%
        mutate(TotalAmount_cal=ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal+VAT_cal) %>%
        mutate(checkTotal=Valuation+Collection.Fee+Freight+ODZ.Fee) %>%
        mutate(TotalCal=ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal) %>%
        mutate(ValuationFee_Check=Valuation-ValuationFee_cal) %>%
        mutate(CollectionFee_Check=Collection.Fee-CollectionFee_cal) %>%
        mutate(FreightCost_Check=Freight-FreightCost_cal) %>%
        mutate(ODZ.Fee_Check=ODZ.Fee_cal-ODZ.Fee_cal) %>%
        mutate(Variance=round(Total.Amount-TotalAmount_cal,2)) %>%
        mutate(FinalAmount=ifelse((Variance<=total_Amount_Percentage_Threshold*TotalAmount_cal) |
                                      (Variance<=Total_Amount_Variance_Threshold),Total.Amount,TotalAmount_cal))
    
    InvoiceDataOrderMapped %<>% mutate(Tracking.No=as.character(Tracking.No))
    InvoiceDataOrderMapped
}