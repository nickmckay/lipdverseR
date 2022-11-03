
#' Calculate derived metadata from a tibble ts
#'
#' @param age
#' @param year
#' @param ...
#'
#' @return
derivedMetadata <- function(age,year,...){
  if(all(is.na(age)) & !all(is.na(year))){
    age <- geoChronR::convertAD2BP(year)
  }

  out <- tibble::tibble(
    minAge = min(age,na.rm = TRUE),
    maxAge = max(age,na.rm = TRUE),
    medianResolution = median(diff(na.omit(age)),na.rm = TRUE)
  )
  return(out)
}



# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
#' Title
#'
#' @param points
#'
#' @import sp
#' @importFrom rworldmap getMap
#' @return
#' @export
coords2country = function(points)
{
  countriesSP <- rworldmap::getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # convert our list of points to a SpatialPoints object

  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string=sp::CRS(sp::proj4string(countriesSP)))


  # use 'over' to get indices of the Polygons object containing each point
  indices = sp::over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  #indices$ADMIN
  #indices$ISO3 # returns the ISO3 code
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)

  return(list("country"=indices$ADMIN,
              "continent"=indices$REGION))
}

#' Update the lipdverse query sheet with updated data
#'
#' @param D  a multiLipd object
#' @return a data.frame for querying
#' @export
createQueryCsv <- function(D){
  TS <- extractTs(D)
  mric <- getMostRecentInCompilationsTs(TS)
  tibdg <- ts2tibble(TS)
  tibdg$paleoData_mostRecentCompilations <- mric
  #calculate some derived metadata
  #minAge
  #maxAge
  #medianRes
  derived <- purrr::pmap_dfr(tibdg,derivedMetadata)

  tibdg <- dplyr::bind_cols(tibdg,derived)

  tibdg <- dplyr::select_if(tibdg,~ !is.list(.x))

  geogNames <- coords2country(data.frame(lon=tibdg$geo_longitude,
                                      lat=tibdg$geo_latitude))

  tibdg$country <- geogNames$country
  tibdg$continent <- geogNames$continent

  #add var tags to help with searching
  allVars <- unique(tibdg$paleoData_variableName)
  tibdg$varTags <- NA

  fuzzyVars <- c("18o", "13c", "d2h")
  #brGDGT
  #leaf wax
  #lake levels
  #salinity
  #Mg-Ca
  #TEX86

  fTab <- matrix(NA,nrow = nrow(tibdg),ncol = length(fuzzyVars))
  for (i in 1:length(fuzzyVars)){
    fvar <- allVars[grep(fuzzyVars[i], tolower(allVars))]
    findex <- which(tibdg$paleoData_variableName %in% fvar)
    ftags <- rep(NA, nrow(tibdg))
    fTab[findex,i] <- fuzzyVars[i]
  }



  tibdg$varTags <- apply(fTab, 1, function(x) paste(x[!is.na(x)], collapse = " "))
  tibdg$varTags[tibdg$varTags == ""] <- NA



  #condense author columns
  authorCols <- colnames(tibdg)[c(grep("pub", colnames(tibdg)), grep("contributor", colnames(tibdg)))]

  allPub <- apply(tibdg, 1, function(x) unname(unlist(x[authorCols])))
  allPub <- apply(allPub, 2, function(x) unlist(x[!is.na(x)]))
  tibdg$auth <- purrr::map_chr(allPub, function(x) paste0(unlist(x[!is.na(x)]), collapse = '|'))

  #condense interpretation variables
  ivc <- which(stringr::str_detect(names(tibdg),"interpretation\\d{1,}_variable$"))
  tibdg$interp_Vars <- apply(tibdg, 1, function(x) unname(unlist(x[ivc]))) %>%
    apply(2, function(x) unlist(x[!is.na(x)])) %>%
    purrr::map_chr(function(x) paste0(unlist(x[!is.na(x)]), collapse = '|'))

  ivcd <- which(stringr::str_detect(names(tibdg),"interpretation\\d{1,}_variableDetail$"))
  tibdg$interp_Details <-  apply(tibdg, 1, function(x) unname(unlist(x[ivcd]))) %>%
    apply(2, function(x) unlist(x[!is.na(x)])) %>%
    purrr::map_chr(function(x) paste0(unlist(x[!is.na(x)]), collapse = '|'))


  keeps <- c("archiveType", "paleoData_variableName", "paleoData_units","paleoData_proxy", "geo_latitude", "geo_longitude","geo_elevation", "minAge", "maxAge","medianResolution", "auth", "datasetId", "dataSetName", "country", "continent","varTags", "interp_Vars", "interp_Details", "paleoData_mostRecentCompilations")

  tibdg <- dplyr::select(tibdg,!!keeps)



  return(tibble::as_tibble(tibdg))
}

#' Update the lipdverse query sheet with updated data
#'
#' @param D  a multiLipd object
#'
#' @export
updateQueryCsv <- function(D){
  newQCSV <- createQueryCsv(D) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  #get the old one
  oldQCSV <- readr::read_csv(file = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  #update the old with the new
  #remove updated columns
  up <- oldQCSV %>%
    dplyr::filter(!paleoData_TSid %in% newQCSV$paleoData_TSid) %>%
    dplyr::bind_rows(newQCSV)

  #write and update
  readr::write_csv(up,file = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv")
zip(files = "/Users/nicholas/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv",
    zipfile = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.zip",extras = '-j')
  tools::md5sum("~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.zip") %>%
  readr::write_file("~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.md5")
}
