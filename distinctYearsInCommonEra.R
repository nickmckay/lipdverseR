#distinct years in the common era
distinctYearsInCommonEra <- function(year){
  year <- floor(na.omit(year))
  g <- which(dplyr::between(year,0,2025))
  ny <- length(unique(year[g]))
  return(ny)
}
distinctAgesInCommonEra <- function(age){
  age <- floor(na.omit(age))
  g <- which(dplyr::between(age,-75,1950))
  ny <- length(unique(age[g]))
  return(ny)
}

distinctTimeInCommonEra <- function(year,age,...){
  if(!is.null(year)){
    if(!all(is.na(year))){
      return(distinctYearsInCommonEra(year))
    }
  }
  if(!is.null(age)){
    if(!all(is.na(age))){
      return(distinctAgesInCommonEra(age))
    }
  }
  return(NA)
}

#load in
h2dsn <- read_sheet_retry("1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8",sheet = "datasetsInCompilation") |>
  filter(inComp != "FALSE")

af <- file.path("~/Dropbox/lipdverse/database/",paste0(h2dsn$dsn,".lpd"))
H2K <- readLipd(af)
ts <- as.lipdTsTibble(H2K)

h2qc <- read_sheet_retry("1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8")


ts$distinctYearsInCommonEra <- pmap_dbl(ts,distinctTimeInCommonEra,.progress = TRUE)

unique(ts$distinctYearsInCommonEra)


small <- select(ts,TSid = paleoData_TSid,distinctYearsInCommonEra)

#remove old column
newQC <- left_join(select(h2qc,-distinctYearsInCommonEra),small,by = "TSid") |>
  relocate(distinctYearsInCommonEra, .after = maxYear)

#write_sheet_retry(newQC,"1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8",sheet = "QC")
write_csv(newQC,"~/Downloads/distinctyears.csv")
