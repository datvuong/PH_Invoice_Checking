load("../4_RData/InvoiceDataRevised.RData")

# Load Area Code Data
AreaCodeMap <- read.csv(file.path(inputFolder,"ReferenceData/AREA_CODE.csv"),
                        stringsAsFactors = FALSE, row.names = 1)

# Load Rate Card Data
rateCard <- read.csv(file.path(inputFolder,"ReferenceData/rateCards.csv"),
                     stringsAsFactors = FALSE, row.names = 1)

# Load Location Tree Data
locationTree <- read.csv(file.path(inputFolder,"ReferenceData/Location_Tree.csv"),
                         stringsAsFactors = FALSE)

# Load OMS Data
omsData <- read.csv(file.path(inputFolder,"OMS/oms_data.csv"),
                    stringsAsFactors = FALSE)

# Load LBC Monitoring Data
lbc_monitorData <- read.csv(file.path(inputFolder,"LBC/LBC_Monitor_data.csv"),
                            stringsAsFactors = FALSE)

# Load SKU Dimension Data
skuDimensionData <- read.csv(file.path(inputFolder,"ReferenceData/SKUDimensions.csv"),
                             stringsAsFactors = FALSE)

# Validated Tracking Number
validateTrackingNumber <- NULL
for (ifile in list.files("../../1_Input/Validated_Tracking_Number/")) {
    validateTrackingNumberFile <- read.csv(file.path("../../1_Input/Validated_Tracking_Number",ifile),
                                           stringsAsFactors = FALSE)
    if (is.null(validateTrackingNumber))
        validateTrackingNumber <- InvoiceDataFile
    else
        validateTrackingNumber <- rbind_list(validateTrackingNumber,InvoiceDataFile)
}


# Mapping OMSData with Location Tree to identify island TRacking Number shipped to
omsDataNew <- omsData
locationTreeNarrow <- locationTree %>% select(2,3,4,11,12)
locationTreeNarrow %<>% mutate(Concatenate.Region.City.Area=gsub("\\+","~",Concatenate.Region.City.Area))
locationTreeNarrow %<>% mutate(Concatenate.Area.Region.City=gsub("\\+","~",Concatenate.Area.Region.City))
locationTreeNarrow %<>% mutate(Concatenate.City.Area.Region=gsub("\\+","~",Concatenate.City.Area.Region))
mappingIsland_1 <- select(locationTreeNarrow
                          ,Concatenate.Region.City.Area
                          ,Island_1=Island
                          ,Service.Level_1=Service.Level)
omsDataNew <- left_join(omsDataNew, mappingIsland_1,
                        by=c("Shipping.City"="Concatenate.Region.City.Area"))
mappingIsland_2 <- select(locationTreeNarrow
                          ,Concatenate.Area.Region.City
                          ,Island_2=Island
                          ,Service.Level_2=Service.Level)
omsDataNew <- left_join(omsDataNew, mappingIsland_2,
                        by=c("Shipping.City"="Concatenate.Area.Region.City"))
mappingIsland_3 <- select(locationTreeNarrow
                          ,Concatenate.City.Area.Region
                          ,Island_3=Island
                          ,Service.Level_3=Service.Level)
omsDataNew <- left_join(omsDataNew, mappingIsland_3,
                        by=c("Shipping.City"="Concatenate.City.Area.Region"))

omsDataNew %<>% 
    mutate(Island=ifelse(!is.na(Island_1), Island_1,
                         ifelse(!is.na(Island_2), Island_2,Island_3))) %>%
    mutate(Service.Level=ifelse(!is.na(Service.Level_1), Service.Level_1,
                                ifelse(!is.na(Service.Level_2), Service.Level_2,Service.Level_3))) %>%
    select(-c(Island_1,Island_2,Island_3,
              Service.Level_1,Service.Level_2,Service.Level_3))


omsData_notmapped <- filter(omsDataNew, is.na(Service.Level))
omsData_mapped <- filter(omsDataNew, !is.na(Service.Level))
write.csv(omsData_notmapped, file = file.path(outputFolder,"omsData_noIslandMapped.csv"),
          row.names = FALSE)

# Extract Tracking Number that weird
omsData_mapped %<>% mutate(Tracking.Number=as.numeric(trimws(Tracking.Number)))
naTrackingNumber <- filter(omsData_mapped,is.na(Tracking.Number))$Order.Number
WeirdTrackingNumber <- filter(omsDataNew,Order.Number %in% naTrackingNumber)
write.csv(WeirdTrackingNumber, file = file.path(outputFolder,"WeirdTrackingNumber.csv"),
          row.names = FALSE)

omsData_mapped <- left_join(omsData_mapped, skuDimensionData,
                            by = )

# Mapping Invoice Data with OMS Data
InvoiceDataRevised %<>% mutate(Tracking.No=as.numeric(Tracking.No))
InvoiceDataRevised <- left_join(InvoiceDataRevised, omsData_mapped,
                                by=(c("Tracking.No"="Tracking.Number")))

# Remove Duplicate as OMS data contain multiple item in one Tracking Number
InvoiceDataRevised %<>% filter(!duplicated(Tracking.No))

# Extract Invoice Data have no Order Mapped
InvoiceDataRevisedNoMappedOrder <- filter(InvoiceDataRevised, is.na(Order.Number))
write.csv(InvoiceDataRevisedNoMappedOrder, file = "../../2_Output/InvoiceNoOderMapped.csv",
          row.names = FALSE)

# Invoice Data with Order Mapped
InvoiceDataRevisedOrderMapped <- filter(InvoiceDataRevised, !is.na(Order.Number))

# Apply calculation to verify the Fee provided by 3PL
InvoiceDataRevisedOrderMapped %<>%
    mutate(AREA_index=regexpr("\\-[^\\-]*$", Package.Code)) %>%
    mutate(AREA=substr(Package.Code, AREA_index+1, AREA_index+10)) %>%
    mutate(AREA=ifelse(AREA=="MMB","GMA",AREA)) %>%
    mutate(AREACode=AreaCodeMap[AREA,]$CODE) %>%
    mutate(RateCard_Index=regexpr(" ", Package.Code)) %>%
    mutate(RateCard=trimws(substr(Package.Code, RateCard_Index, AREA_index-1))) %>%
    mutate(ValuationFee_cal=as.numeric(0.005*Declared.Value)) %>%
    mutate(CollectionFee_cal=as.numeric(ifelse(Is.COD!="YES",0,
                                               ifelse(Declared.Value<=2500,0.01*Declared.Value,
                                                      25+0.0075*(Declared.Value-2500))))) %>%
    mutate(Succeding.Kilos=ifelse(Final.Caluated.Weight>3,Final.Caluated.Weight-3,0)) %>%
    mutate(FreightCost_cal=as.numeric(ifelse(RateCard=="TPS",rateCard["TPS",AREACode],
                                             ifelse(RateCard=="TPL",rateCard["TPL",AREACode],
                                                    rateCard["D1 - 1st 3 kilos",AREACode] + 
                                                        Succeding.Kilos*rateCard["D1 - Succeding Kilos",AREACode])))) %>%
    mutate(ODZ.Fee_cal=as.numeric(ifelse(Service.Level=="ODZ",700,0))) %>%
    mutate(VAT_cal=0.12*(ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal)) %>%
    mutate(TotalAmount_cal=ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal+VAT_cal) %>%
    mutate(checkTotal=Valuation+Collection.Fee+Freight+ODZ.Fee) %>%
    mutate(TotalCal=ValuationFee_cal+CollectionFee_cal+FreightCost_cal+ODZ.Fee_cal) %>%
    mutate(ValuationFee_Check=Valuation-ValuationFee_cal) %>%
    mutate(CollectionFee_Check=Collection.Fee-CollectionFee_cal) %>%
    mutate(FreightCost_Check=Freight-FreightCost_cal) %>%
    mutate(ODZ.Fee_Check=ODZ.Fee_cal-ODZ.Fee_cal) %>%
    mutate(Variance=TotalAmount_cal-Total.Amount)

# Mapping LBC Monitoring Data with calculated Data
lbc_monitorDataNarrow <- select(lbc_monitorData, Tracking.Number,
                                X1st.STATUS,FINAL.STATUS,DATE.1)

NearlyFinal <- left_join(InvoiceDataRevisedOrderMapped, lbc_monitorDataNarrow,
                         by=c("Tracking.No"="Tracking.Number"))

# Built Final Data
Final <- NearlyFinal %>%
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
           ,Package.Number
           ,AREACode
           ,RateCard
           ,Valuation
           ,ValuationFee_cal
           ,CollectionFee_cal
           ,FreightCost_cal
           ,ODZ.Fee_cal
           ,VAT_cal
           ,TotalAmount_cal
           ,Variance
           ,Service.Level
           ,First_attemp=X1st.STATUS
           ,FINAL.STATUS
           ,Final.Status.Date=DATE.1
           ,Shipped.Status=Outbound.Logistics...Shipped.Status
           ,Shipped.Date=Outbound.Logistics...Shipped.Date
           ,Item.Status
           ,Item.Status.Date
           ,Payment.Method
           ,Shipping.City
           ,Island)

sum(InvoiceDataRevisedOrderMapped$Content!=InvoiceDataRevisedOrderMapped$Package.Number)

write.csv(Final, file = "../../2_Output/sampleOutput.csv",
          row.names = FALSE)

