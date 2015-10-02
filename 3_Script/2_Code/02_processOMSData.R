
loadOMSData <- function(rDataFolder){
    
    i <- 0
    totalStep <- length(list.files("../../1_Input/OMS")) + 4 +
        length(list.files("../../1_Input/SKUDimensions"))
    pb <- txtProgressBar(min=0, max=totalStep, style = 3)
    getTxtProgressBar(pb)
    
    load(file.path(rDataFolder,"commonVariable.RData"))
    
    # Load Location Tree Data
    locationTree <- read.csv(file.path(inputFolder,"ReferenceData/Location_Tree.csv"),
                             stringsAsFactors = FALSE)
    
    setClass('myDateTime')
    setAs("character","myDateTime", function(from) as.POSIXct("1900-01-01", format("%Y-%m-%d"), tz = "Asia/Manila") + ddays(as.numeric(from)-2))
    
    i <- i + 1
    setTxtProgressBar(pb, i)
    # Load OMS Data
    omsData <- NULL
    for (iFile in list.files("../../1_Input/OMS")){
        if (file_ext(iFile)=="csv"){
            omsFileData <- read.csv(file.path("../../1_Input/OMS",iFile),
                                    col.names = c("Order.Number","Sales.Order.Item","SKU",
                                                  "Description","Item.Status","Item.Status.Date",
                                                  "Payment.Method","Shipping.City",
                                                  "Outbound.Logistics...Shipped.Date",
                                                  "Outbound.Logistics...Delivered.Date","Tracking.Number",
                                                  "Delivery.Company","Package.Number"),
                                    colClasses = c("character","character","character",
                                                   "character","character","character",
                                                   "factor","character","character",
                                                   "character","character","character","character"),
                                    stringsAsFactors = FALSE)
            if (is.null(omsData))
                omsData <- omsFileData
            else
                omsData <- rbind_list(omsData, omsFileData)
            i <- i + 1
        }
        setTxtProgressBar(pb, i)
    }
    
    save(omsData, file=file.path(rDataFolder,"omsData.RData"))
    
    # Load SKU Dimension Data
    skuDimensionData <- NULL
    for (iFile in list.files("../../1_Input/SKUDimensions")){
        skuFileData <- read.csv(file.path("../../1_Input/SKUDimensions",iFile),
                                stringsAsFactors = FALSE)
        if (is.null(omsData))
            skuDimensionData <- skuFileData
        else
            skuDimensionData <- rbind(skuFileData)
        i <- i + 1
        setTxtProgressBar(pb, i)
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
    
    i <- i + 1
    setTxtProgressBar(pb, i)
    
    omsData_notmapped <- filter(omsDataNew, is.na(Service.Level))
    omsData_mapped <- omsDataNew
    write.csv(omsData_notmapped, file = file.path(outputFolder,"omsData_noIslandMapped.csv"),
              row.names = FALSE)
    
    i <- i + 1
    setTxtProgressBar(pb, i)
    
    # Extract Tracking Number that weird
    omsData_mapped %<>% mutate(Tracking.Number=as.character(trimws(Tracking.Number)))
    WeirdTrackingNumber <- filter(omsData_mapped,nchar(Tracking.Number)<10)
    write.csv(WeirdTrackingNumber, file = file.path(outputFolder,"WeirdTrackingNumber.csv"),
              row.names = FALSE)
    
    # Mapping omsData with SKUDimension Data
    skuDimensionData %<>%
        select(SKU=sku_simple
               ,package_weight
               ,CBM)
    
    omsData_mapped <- left_join(omsData_mapped, skuDimensionData,
                                by = c("SKU"))
    
    #Missing SKU Dimension Data
    missingSKUDimension <- filter(omsData_mapped, is.na(CBM)) %>%
        select(SKU)
    write.csv(missingSKUDimension, file = file.path(outputFolder,"SKUMissingDimensionData.csv"))
    
    save(omsData_mapped, file="../4_RData/omsDataMapped.RData")
    
    omsTrackingData <- omsData_mapped %>%
        mutate(Tracking.Number=as.numeric(trimws(Tracking.Number))) %>%
        filter(!is.na(Tracking.Number)) %>%
        group_by(Tracking.Number) %>%
        summarize(Order.Number=last(Order.Number),
                  ShippedDate=last(Outbound.Logistics...Shipped.Date),
                  DeliveredDate=last(Outbound.Logistics...Delivered.Date),
                  Service.Level=last(Service.Level),
                  Payment.Method=last(Payment.Method),
                  Package.Number=last(Package.Number),
                  Shipping.City=last(Shipping.City),
                  Island=last(Island),
                  SKUs=paste(SKU,collapse = "/"),
                  SKUs_Description=paste(Description,collapse = "///"),
                  TotalSKUActualWeight=ceiling(sum(package_weight, na.rm=TRUE)),
                  TotalSKUDimWeight=sum(CBM, na.rm=TRUE),
                  TotalSKUWeight=ifelse(TotalSKUDimWeight>TotalSKUActualWeight,TotalSKUDimWeight,TotalSKUActualWeight),
                  SKU_MissingDimensionData=sum(ifelse(is.na(CBM) | CBM==0,1,0)))
    
    omsTrackingData %<>% mutate(Tracking.Number=as.character(Tracking.Number))
    
    i <- i + 1
    setTxtProgressBar(pb, i)
    
    omsTrackingData
}