library(methods)
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(library(magrittr))
suppressWarnings(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(tools))

dateFolder <- readChar("../../1_Input/RunningFolder.txt",
                       file.info("../../1_Input/RunningFolder.txt")$size)
inputFolder <- file.path("../../1_Input", dateFolder)
outputFolder <- file.path("../../2_Output", dateFolder)
dir.create(outputFolder, showWarnings = FALSE)
rDataFolder <- file.path("../4_RData",dateFolder)
dir.create(rDataFolder, showWarnings = FALSE)

save(dateFolder,inputFolder,outputFolder,rDataFolder,
     file = file.path("../4_RData/commonVariable.RData"))

source("../2_Code/02_processOMSData.R")
omsTrackingData <- loadOMSData()
save(omsTrackingData, file=file.path("../4_RData", "omsTrackingData.RData"))