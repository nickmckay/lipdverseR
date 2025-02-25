
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
  if(all(is.na(age))){
    out <- tibble::tibble(
      minAge = NA,
      maxAge = NA,
      medianResolution = NA)
  }else{
    out <- tibble::tibble(
      minAge = min(age,na.rm = TRUE),
      maxAge = max(age,na.rm = TRUE),
      medianResolution = median(abs(diff(age)),na.rm = TRUE)
    )
  }
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



  dvlu <- data.frame(dataSetName = purrr::map_chr(D,pluck,"dataSetName"),
                     datasetVersion = purrr::map_chr(D,getVersion))

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
  names(tibdg)[ivcd]


  step1 <- apply(tibdg, 1, function(x) unname(unlist(x[ivcd])))

  if(NCOL(step1) > 1){
  step2 <- step1 %>%
    apply(2, function(x) unlist(x[!is.na(x)]))
  }else if(NCOL(step1) == 1){
    step2 <- step1
  }
  tibdg$interp_Details <- purrr::map_chr(step2,function(x) paste0(unlist(x[!is.na(x)]), collapse = '|'))




  keeps <- c("paleoData_TSid","archiveType", "paleoData_variableName", "paleoData_units","paleoData_proxy",
             "geo_latitude", "geo_longitude","geo_elevation", "minAge", "maxAge",
             "medianResolution", "auth", "datasetId", "dataSetName","country",
             "continent", "interp_Vars", "interp_Details",
             "paleoData_mostRecentCompilations", "interpretation1_seasonality","paleoData_hasTimeTsid")


  keeps <- intersect(keeps,names(tibdg)) #ignore any that aren't in there


  tibdg <- dplyr::select(tibdg,!!keeps)

  tibdg <- dplyr::left_join(tibdg,dvlu,by = "dataSetName")


  return(tibble::as_tibble(tibdg))
}

#' Update the lipdverse query sheet with updated data
#'
#' @param D  a multiLipd object
#' @param append append the updated query sheet to existing? This will not replace non-updated files. Default = TRUE, only use FALSE if you want to overwrite the entire database with this multilipd object
#'
#' @export
updateQueryCsv <- function(D,append = TRUE){
  newQCSV <- createQueryCsv(D) %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  #get the old one
  oldQCSV <- readr::read_csv(file = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  if(append){#update the old with the new
  #remove updated columns
  up <- oldQCSV %>%
    dplyr::filter(!paleoData_TSid %in% newQCSV$paleoData_TSid) %>%
    dplyr::bind_rows(newQCSV)

  }else{#overwrite with this one
    up <- newQCSV
  }

  #write and update
  readr::write_csv(up,file = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv")
  zip(files = "/Users/nicholas/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.csv",
      zipfile = "~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.zip",extras = '-j')
  tools::md5sum("~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.zip") %>%
    readr::write_file("~/Dropbox/lipdverse/html/lipdverse/lipdverseQuery.md5")

  updateSqlQuery(queryTable = up)
}



#devtools::load_all()
#update_queryTable()
# dim(queryTable)
# colnames(queryTable)
# length(unique(queryTable$dataSetName))
# length(unique(queryTable$datasetId))

#firstMatches <- match(unique(queryTable$datasetId), queryTable$datasetId)
datasetIDcollapse <- function(x){stringr::str_remove(paste0(na.omit(unique(x)), collapse = ","),pattern = "^,|,$")}



updateSqlQuery <- function(queryTable){

    df1 <- queryTable |>
    dplyr::group_by(datasetId) |>
    dplyr::summarise(dataSetName = datasetIDcollapse(dataSetName),
                     archiveType = datasetIDcollapse(archiveType),
                     TSid = datasetIDcollapse(paleoData_TSid),
                     paleoData_mostRecentCompilations = datasetIDcollapse(paleoData_mostRecentCompilations),
                     interpretation1_seasonality = datasetIDcollapse(interpretation1_seasonality),
                     country = datasetIDcollapse(country),
                     continent = datasetIDcollapse(continent),
                     medianResolution = max(medianResolution,na.rm = TRUE),
                     interp_Vars = datasetIDcollapse(interp_Vars),
                     paleoData_variableName = datasetIDcollapse(paleoData_variableName),
                     minAge = max(minAge,na.rm = TRUE),
                     maxAge = min(maxAge,na.rm = TRUE),
                     geo_latitude = mean(as.numeric(geo_latitude),na.rm = TRUE),
                     geo_longitude = mean(as.numeric(geo_longitude),na.rm = TRUE),
                     paleoData_proxy = datasetIDcollapse(paleoData_proxy))




  ## Create an sf POINTS object
  points <- data.frame(df1$geo_longitude, df1$geo_latitude)
  pts <- sf::st_as_sf(points, coords=1:2, crs=4326)

  ## Find which points fall over land
  ii <- !is.na(as.numeric(sf::st_intersects(pts, spData::world)))

  ## Check that it worked
  # plot(st_geometry(world))
  # plot(pts, col=1+ii, pch=16, add=TRUE)

  ##Add column for isTerrestrial
  df1 <- cbind.data.frame(df1, isTerrestrial=ii)


  #replace "NA" with NA where this is the unique variable
  df1[df1 == "NA"] <- NA

  #Remove NA where other variables exist
  rmExtraNA <- function(df){
    for(j in 1:nrow(df)){
      for(k in 1:ncol(df)){
        if (grepl(",", df1[j,k])){
          #print(c(j,k))
          #print(strsplit(df[j,k], ","))
          a1 <- unlist(strsplit(df[j,k], ","))
          df[j,k] <- paste0(a1[!a1 == "NA"], collapse = ",")
        }
      }
    }
    df
  }

  df1 <- rmExtraNA(df1)

  #replace dataSet queryTable
  #connection info
  conInf <- readr::read_tsv("sql.secret",col_names = FALSE)

  mysqlconnection = RMySQL::dbConnect(RMySQL::MySQL(),
                                      dbname='lipdverse',
                                      host='143.198.98.66',
                                      port=3306,
                                      user=conInf$X1[[1]],
                                      password=conInf$X1[[2]])


  dbWriteTable(mysqlconnection, "dataSetQuery", df1, overwrite=TRUE)


}
