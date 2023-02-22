#automatically determine what datasets for Holocene abrupt change


TS <- extractTs(DS)
sTS <- lipdR::splitInterpretationByScope(TS)
qcId <- "1u4vWrSsXv_6O16juL4WCgaJdFMxBqwerBi6BGhb3IgQ"
project <- "HoloceneAbruptChange"

qc <- createQCdataFrame(sTS,templateId = qcId,compilationName = project,compVersion = "0_11_0",ageOrYear = "age")


library(tidyverse)

qc$maxYear <- as.numeric(qc$maxYear)
qc$minYear <- as.numeric(qc$minYear)
qc$medianHoloceneResolution <- as.numeric(qc$medianHoloceneResolution)
qc$holoceneDuration <- map2_dbl(qc$maxYear,qc$minYear,\(x,y)
                                max(min(x,12000,na.rm = TRUE) - y,0,na.rm = TRUE))

  ac <- qc %>%
  filter(climateVariable != "",
         medianHoloceneResolution < 500,
         primaryTimeseries != FALSE,
         holoceneDuration >= 2000)



write_sheet_retry(ac,qcId,sheet = 1,timeout = 60)


library(ggplot2)
ggplot(ac) + geom_histogram(aes(x = medianHoloceneResolution),bins = 50)
length(unique(ac$datasetId))
ggplot(ac) + geom_histogram(aes(x = holoceneDuration),bins = 50)
