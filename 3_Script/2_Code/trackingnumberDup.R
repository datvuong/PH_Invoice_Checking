trackingNumber <- read.csv("../../4_Reference/sampe.csv",
                           stringsAsFactors = FALSE)

trackingNumber %<>%
    mutate(Month=ifelse(Month=="January","201501",
                        ifelse(Month=="February","201502",
                               ifelse(Month=="March" | Month=="MARCH 2015","201503",
                                      ifelse(Month=="April", "201504",
                                             ifelse(Month=="May" | Month=="MAY" | Month=="MAY 2015","201505",
                                                    ifelse(Month=="June","201506",
                                                           ifelse(Month=="July","201507",Month))))))))

months <- unique(trackingNumber$Month)

for (iMonth in c(months)) {
    trackingNumberMonth <- filter(trackingNumber, Month==iMonth)
    write.csv(trackingNumberMonth, 
              file.path("../../1_Input/Validated_Tracking_Number",
                        paste0(iMonth,".csv")),
              row.names = FALSE)
}

