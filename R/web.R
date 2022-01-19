#create files for new web structure


#create sidebarmeta .html
createSidebarHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

ts <- extractTs(L)

#check for these variables

dsid <- L$datasetId
dsn <- L$dataSetName
vers <- max(sapply(L$changelog,"[[","version"))
vers_ <- str_replace_all(vers,"[.]","_")


#create some strings we'll need

sidebarTitle <- glue::glue("{dsn} - v{vers}")
dsidstr <- paste("Dataset Id:",dsid)
dsPath <- glue("https://lipdverse.org/data/{dsid}/{vers_}/")
lpdPath <- glue("{dsPath}/{dsn}.lpd")
csvName <- glue("{dsPath}/{dsn}.csv")
csvNameChron <- glue("{dsPath}/{dsn}-chron.csv")


#prep and setup widget
sidebar <- '<head>\n
<base target="_PARENT">\n
</head>\n'




#html sidebar
sidebar <- sidebar %>%
  str_c('<div class="sidenav"> \n') %>%
  str_c(glue('<h2>{sidebarTitle}</h2>',sep = "\n")) %>%
  str_c(glue('<h4><a href="{dsPath}">{dsidstr}</a></h4>',sep = "\n")) %>%
  str_c(glue('<p style="margin-left: 0px"><a href="{lpdPath}">Download LiPD file</a>',sep = "\n")) %>%
  str_c("\n") %>%
  str_c("            \n") %>%
  str_c(glue('<p style="margin-left: 0px"><a href="http://lipd.net/playground?source={lpdPath}">Edit LiPD file</a>',sep = "\n")) %>%
  str_c("\n") %>%
  str_c("            \n") %>%
  str_c(glue('<p style="margin-left: 0px"><a href="{csvName}">Download PaleoData only (csv)</a>',sep = "\n")) %>%
  str_c("\n") %>%
  str_c("            \n")


if(length(L$chronData) > 0){
  sidebar <- sidebar %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{csvNameChron}">Download ChronData onle (csv)</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n")
}

sidebar <- sidebar %>%
  str_c('<p style="margin-left: 0px"><a href="https://github.com/nickmckay/LiPDverse/issues">Report an issue (include datasetId and version)</a>',sep = "\n") %>%
  str_c("\n")



#get all the variable names
allNames <- names(ts[[1]])


###########Write root metadata
sidebar <- writeCollapsibleChunks(sidebar,ts,name = "root",vars = c("archiveType", "originalDataUrl","lipdVersion","dataContributor"),open = TRUE)
###########End root metadata

############WRITE PUB###############
#large pub section
pubNames <- allNames[grep("pub*", allNames)]
if(length(pubNames)>1){
  npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
}else{
  npub <- 0
}

if(npub>0){
  bigName <-  "pub"
  vars <-  c("author","citeKey","journal","volume","pages","pubYear","title","DOI")

  sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
    str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


  #write each publication.
  for(p in 1:npub){
    sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
  }

  #close pub setion
  sidebar <- str_c(sidebar,"</details>",sep = "\n")
}
############END PUB###############

##################################Write Funding metadata.................
pubNames <- allNames[grep("funding*", allNames)]
if(length(pubNames)>1){
  npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
}else{
  npub <- 0
}

if(npub>0){
  bigName <-  "funding"
  vars <-  c("agency","grant","country")


  sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
    str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


  #write each publication.
  for(p in 1:npub){
    sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
  }

  #close pub setion
  sidebar <- str_c(sidebar,"</details>",sep = "\n")
}
############END FUNDING###############

###########Write geo metadata
sidebar <- writeCollapsibleChunks(sidebar,ts,name = "geo",vars = c("latitude", "longitude","elevation","region","siteName","pages2kRegion","country","state"),open = TRUE)
###########End root metadata

#PaleoData preamble
sidebar <- str_c(sidebar,"<details open>",sep = "\n") %>%
  str_c(str_c("<summary>PaleoData columns</summary>"),sep = "\n")

##################################Write PaleoData Column metadata.................
##loop through columns and plot them.
hasInterpretation <- sapply(ts,function(x){!is.null(x$interpretation1_variable)})
paleoNum <-  pullTsVariable(ts,"paleoNumber")
tableNum <-  pullTsVariable(ts,"tableNumber")



thisVarNames <- sapply(ts,"[[","paleoData_variableName")
xcol <- which(startsWith(thisVarNames,"age") | startsWith(thisVarNames,"depth") | startsWith(thisVarNames,"year"))

plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
isxcol <- plotOrder %in% xcol
plotOrder <- plotOrder[order(paleoNum,tableNum,-isxcol,-hasInterpretation)]#sort by table Number

for(cc in plotOrder){#for each column..
  #regular metadata
  if(length(ts[[cc]]$paleoData_units) == 0){
    ts[[cc]]$paleoData_units <- "missing"
  }


  if(is.na(ts[[cc]]$paleoData_units)){
    ts[[cc]]$paleoData_units <- "unitless"
  }

  if(max(paleoNum) == 1  & max(tableNum) == 1){
    tvname <- str_c(ts[[cc]]$paleoData_variableName," (",ts[[cc]]$paleoData_units,")")
  }else{
    tvname <- str_c(ts[[cc]]$paleoData_variableName," (",ts[[cc]]$paleoData_units,") [",as.character(ts[[cc]]$paleoNumber),"-",as.character(ts[[cc]]$tableNumber),"]")
  }

  sidebar <- writeCollapsibleChunks(sidebar,ts,name = "paleoData",forceName = tvname,vars = c("TSid", "variableName","units","description","useInGlobalTemperatureAnalysis"),tsi = cc,dontClose = TRUE,indent = 10)




  ###INTERPRETATIONS
  allNames=names(ts[[cc]])
  pubNames <- allNames[grep("interpretation*", allNames)]

  if(length(pubNames)>1){
    npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
  }else{
    npub <- 0
  }

  if(npub>0){
    bigName <-  "interpretation"

    sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
      str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


    #write each interpretation
    for(p in 1:npub){
      vars <- str_extract(pubNames,str_c("(?<=interpretation",as.numeric(p),"_).*$")) #get all.
      sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,tsi = cc,forceName = as.character(p),indent = 20)
    }
    if(is.na(sidebar)){stop("asdas")}

    #close paleo setion
    sidebar <- str_c(sidebar,"</details>",sep = "\n")
  }
  sidebar <- str_c(sidebar,"</details>",sep = "\n")
  ##END INTERPRETATIONS
}




############END PaleoData Column###############
if(length(L$chronData) > 0){
  chronTS <- extractTs(L,mode = "chron")

  #ChronData preamble
  sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
    str_c(str_c("<summary>ChronData columns</summary>"),sep = "\n")
  ##################################Write ChronData Column metadata.................
  ##loop through columns and plot them.
  chronNum <-  pullTsVariable(chronTS,"chronNumber")
  tableNum <-  pullTsVariable(chronTS,"tableNumber")



  thisVarNames <- sapply(chronTS,"[[","chronData_variableName")
  xcol <- which(startsWith(thisVarNames,"depth"))


  plotOrderChron <-  seq(1, length(chronTS)) #generate a sequence to start with
  isxcol <- plotOrderChron %in% xcol
  plotOrderChron <- plotOrderChron[order(chronNum,tableNum,-isxcol)]#sort by table Number

  for(cc in plotOrderChron){#for each column..
    #regular metadata

    if(max(chronNum) == 1  & max(tableNum) == 1){
      tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,")")
    }else{
      tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,") [",as.character(chronTS[[cc]]$chronNumber),"-",as.character(chronTS[[cc]]$tableNumber),"]")
    }

    sidebar <- writeCollapsibleChunks(sidebar,thisTS = chronTS,name = "chronData",forceName = tvname,vars = c("TSid", "variableName","units","description"),tsi = cc,dontClose = TRUE,indent = 10)
    #close each column setion
    sidebar <- str_c(sidebar,"</details>",sep = "\n")
  }

  #######END CHRONDATA!!!!######################
}

######END METADATA!!!!############################
sidebar <- sidebar %>%
  str_c("</div>")


if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
  dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
}
write_file(sidebar,file = file.path(webdir,"data",dsid,vers_,"sidebar.html"))



}

# create project map .html

createProjectMapHtml <- function(TS,project,projVersion,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  projDir <- file.path(webdir,project,projVersion)
  if(!dir.exists(projDir)){
    dir.create(projDir,recursive = TRUE)
  }
  #create map meta

  #All datasets
  dsn <- lipdR::pullTsVariable(TS,"dataSetName")
  ui <- which(!duplicated(dsn))
  udsn <- dsn[ui]
  lat <- lipdR::pullTsVariable(TS,"geo_latitude")[ui]
  lon <- lipdR::pullTsVariable(TS,"geo_longitude")[ui]
  archiveType <- lipdR::pullTsVariable(TS,"archiveType")[ui]
  link <- paste0(udsn,".html") %>%
    str_replace_all("'","_")

  #Organize metadata for map
  map.meta <- data.frame(dataSetName = udsn, #datasetname
                         lat = lat,#lat
                         lon = lon,#lon
                         archiveType = factor(archiveType),#archiveType
                         link = link)#Link

#save for Rmd
  save(map.meta,file = file.path(projDir,"mapmeta.Rdata"))


  #create RMd
  file.copy(file.path(webdir,"mapOnly.Rmd"),to = file.path(projDir,"map.Rmd"),overwrite = TRUE)

  rmarkdown::render(file.path(projDir,"map.Rmd"))

  maphtml <- readr::read_file(file.path(projDir,"map.html"))
  #find head
  maphtml <- stringr::str_replace(maphtml,"<head>\n",'<head>\n<base target="_parent">\n')

   readr::write_file(maphtml,file.path(projDir,"map.html"))


}


createLipdverseMapHtml <- function(TS,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  #All datasets
  dsn <- lipdR::pullTsVariable(TS,"dataSetName")
  dsid <- lipdR::pullTsVariable(TS,"datasetId")
  ui <- which(!duplicated(dsid))
  udsn <- dsn[ui]
  udsid <- dsid[ui]
  lat <- lipdR::pullTsVariable(TS,"geo_latitude")[ui]
  lon <- lipdR::pullTsVariable(TS,"geo_longitude")[ui]
  archiveType <- lipdR::pullTsVariable(TS,"archiveType")[ui]
  link <- glue::glue("../data/{udsid}/")

  #Organize metadata for map
  map.meta <- data.frame(dataSetName = udsn, #datasetname
                         lat = lat,#lat
                         lon = lon,#lon
                         archiveType = factor(archiveType),#archiveType
                         link = link)#Link

  #save for Rmd
  save(map.meta,file = file.path(webdir,"data","mapmeta.Rdata"))


  #create RMd
  file.copy(file.path(webdir,"mapOnly.Rmd"),to = file.path(webdir,"data","map.Rmd"),overwrite = TRUE)

  rmarkdown::render(file.path(webdir,"data","map.Rmd"))

  maphtml <- readr::read_file(file.path(webdir,"data","map.html"))
  #find head
  maphtml <- stringr::str_replace(maphtml,"<head>\n",'<head>\n<base target="_parent">\n')

  readr::write_file(maphtml,file.path(webdir,"data","map.html"))

}


#create paleodata plots .html
createPaleoDataPlotHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  ts <- extractTs(L)

  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- max(sapply(L$changelog,"[[","version"))
  vers_ <- str_replace_all(vers,"[.]","_")


thisVarNames <- sapply(ts,"[[","paleoData_variableName")

#graph order
xcol <- which(startsWith(thisVarNames,"age") | startsWith(thisVarNames,"depth") | startsWith(thisVarNames,"year"))

hasInterpretation <- sapply(ts,function(x){!is.null(x$interpretation1_variable)})
paleoNum <-  pullTsVariable(ts,"paleoNumber")
tableNum <-  pullTsVariable(ts,"tableNumber")

thisVarNames <- sapply(ts,"[[","paleoData_variableName")

plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
isxcol <- plotOrder %in% xcol
plotOrder <- plotOrder[order(paleoNum,tableNum,-isxcol,-hasInterpretation)]#sort by table Number


##create csv output for download
lengths <- sapply(ts,function(x){length(x$paleoData_values)})+1
outdf <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))

for(cc in 1:length(plotOrder)){#for each column..
  outdf[1,cc] <- ts[[plotOrder[cc]]]$paleoData_TSid
  outdf[2:lengths[plotOrder[cc]],cc] <- ts[[plotOrder[cc]]]$paleoData_values
  if(max(paleoNum) == 1  & max(tableNum) == 1){
    names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$paleoData_variableName," (",ts[[plotOrder[cc]]]$paleoData_units,")")
  }else{
    names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$paleoData_variableName," (",ts[[plotOrder[cc]]]$paleoData_units,") [",as.character(ts[[plotOrder[cc]]]$paleoNumber),"-",as.character(ts[[plotOrder[cc]]]$tableNumber),"]")
  }

  #check seasonality
  thisSeason <- ts[[plotOrder[cc]]]$interpretation1_seasonality
  if(!is.null(thisSeason)){
    names(outdf)[cc] <- str_c(names(outdf)[cc]," - ",thisSeason)
  }
  print(names(outdf)[cc])
}

if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
  dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
}

readr::write_csv(outdf,file.path(webdir,"data",dsid,vers_,paste0(dsn,".csv")))

if(any(plotOrder %in% xcol)){#then remove those
  graphOrder = plotOrder[-which(plotOrder %in% xcol)]
  graphNames = names(outdf)[-which(plotOrder %in% xcol)]
}else{
  graphOrder = plotOrder
  graphNames = names(outdf)
}


#check for non-numeric columns. Don't plot those.
nonnumeric <- which(purrr::map_lgl(ts,~ !is.numeric(.x$paleoData_values)))


if(any(plotOrder %in% xcol)){#then remove those
  graphOrder = plotOrder[-which(plotOrder %in% xcol)]
  graphNames = names(outdf)[-which(plotOrder %in% xcol)]
}else{
  graphOrder = plotOrder
  graphNames = names(outdf)
}

if(any(graphOrder == nonnumeric)){
  gind <- -which(graphOrder == nonnumeric)
  graphOrder <- graphOrder[gind]
  graphNames  <- graphNames[gind]
}


save(ts,file = file.path(webdir,"thisTs.Rdata"))

#Initialize Rmd
thisRmd <- "---" %>%
  str_c("output: html_document",sep = "\n") %>%
  str_c("---",sep = "\n") %>%
  str_c("\n")


#write load data piece
thisRmd <- thisRmd %>%
  str_c("```{r, echo = FALSE, warning = FALSE,message = FALSE}",sep = "\n") %>%
  str_c(glue('load("{file.path(webdir,"thisTs.Rdata")}")'),sep = "\n") %>%
          str_c("```",sep = "\n")

thisRmd <- str_c(thisRmd,"PaleoData {.tabset .tabset-fade}",sep = "\n") %>%
  str_c("-----------------------------------------------------------------------",sep = "\n") %>%
  str_c("\n")


for(cc in 1:length(graphOrder)){#for each column..
  #setup a new column in the markdown
  thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>%
    str_c("```{r, echo = FALSE,warning = FALSE,message = FALSE}",sep = "\n") %>%
    str_c(str_c("lipdverseR::plotCol(ts,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>%
    str_c("```",sep = "\n")  %>%
    str_c("\n")
}



#write out the Rmd
write_file(thisRmd,path = file.path(webdir,"data",dsid,vers_,"paleoPlots.Rmd"))

check <- try(rmarkdown::render(file.path(webdir,"data",dsid,vers_,"paleoPlots.Rmd")),silent = TRUE)
if(class(check) ==  "try-error"){
  readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"paleoPlots.html"))
  return("Paleo plots error")
}


}

#create chrondata plots .html

createChronDataPlotHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- max(sapply(L$changelog,"[[","version"))
  vers_ <- str_replace_all(vers,"[.]","_")

  if(length(L$chronData) < 1){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"chronPlots.html"))
    return("No chron")
  }

  ts <- extractTs(L,mode = "chron")

  thisVarNames <- sapply(ts,"[[","chronData_variableName")

  #graph order
  xcol <- which(startsWith(thisVarNames,"depth"))

  chronNum <-  pullTsVariable(ts,"chronNumber")
  tableNum <-  pullTsVariable(ts,"tableNumber")

  plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
  isxcol <- plotOrder %in% xcol
  plotOrder <- plotOrder[order(chronNum,tableNum,-isxcol)]#sort by table Number


  ##create csv output for download
  lengths <- sapply(ts,function(x){length(x$chronData_values)})+1
  outdf <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))

  for(cc in 1:length(plotOrder)){#for each column..
    outdf[1,cc] <- ts[[plotOrder[cc]]]$chronData_TSid
    outdf[2:lengths[plotOrder[cc]],cc] <- ts[[plotOrder[cc]]]$chronData_values
    if(max(chronNum) == 1  & max(tableNum) == 1){
      names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$chronData_variableName," (",ts[[plotOrder[cc]]]$chronData_units,")")
    }else{
      names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$chronData_variableName," (",ts[[plotOrder[cc]]]$chronData_units,") [",as.character(ts[[plotOrder[cc]]]$chronNumber),"-",as.character(ts[[plotOrder[cc]]]$tableNumber),"]")
    }

    print(names(outdf)[cc])
  }

  if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
    dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
  }

  readr::write_csv(outdf,file.path(webdir,"data",dsid,vers_,paste0(dsn,"-chron.csv")))


  #check for non-numeric columns. Don't plot those.
  nonnumeric <- which(purrr::map_lgl(ts,~ !is.numeric(.x$chronData_values)))


  if(any(plotOrder %in% xcol)){#then remove those
    graphOrder = plotOrder[-which(plotOrder %in% xcol)]
    graphNames = names(outdf)[-which(plotOrder %in% xcol)]
  }else{
    graphOrder = plotOrder
    graphNames = names(outdf)
  }

  if(any(graphOrder == nonnumeric)){
    gind <- -which(graphOrder == nonnumeric)
    graphOrder <- graphOrder[gind]
    graphNames  <- graphNames[gind]
  }


  save(ts,file = file.path(webdir,"thisChronTs.Rdata"))

  #Initialize Rmd
  thisRmd <- "---" %>%
    str_c("output: html_document",sep = "\n") %>%
    str_c("---",sep = "\n") %>%
    str_c("\n")


  #write load data piece
  thisRmd <- thisRmd %>%
    str_c("```{r, echo = FALSE, warning = FALSE,message = FALSE}",sep = "\n") %>%
    str_c(glue('load("{file.path(webdir,"thisChronTs.Rdata")}")'),sep = "\n") %>%
    str_c("```",sep = "\n")

  thisRmd <- str_c(thisRmd,"ChronData {.tabset .tabset-fade}",sep = "\n") %>%
    str_c("-----------------------------------------------------------------------",sep = "\n") %>%
    str_c("\n")


  for(cc in 1:length(graphOrder)){#for each column..
    #setup a new column in the markdown
    thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>%
      str_c("```{r, echo = FALSE,warning = FALSE,message = FALSE}",sep = "\n") %>%
      str_c(str_c("lipdverseR::plotCol(ts,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>%
      str_c("```",sep = "\n")  %>%
      str_c("\n")
  }



  #write out the Rmd
  write_file(thisRmd,path = file.path(webdir,"data",dsid,vers_,"chronPlots.Rmd"))

  check <- try(rmarkdown::render(file.path(webdir,"data",dsid,vers_,"chronPlots.Rmd")),silent = TRUE)
  if(class(check) ==  "try-error"){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"chronPlots.html"))
    return("Chron error")
  }

}



#wrapper to create all for a LiPD file
createWebComponents <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  createSidebarHtml(L,webdir = webdir)
  createPaleoDataPlotHtml(L,webdir = webdir)
  try(createChronDataPlotHtml(L,webdir = webdir))
}


#create iframe html for data page (or just lipdverse)
createDataWebPage <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- max(sapply(L$changelog,"[[","version"))
  vers_ <- str_replace_all(vers,"[.]","_")

  dsPath <- glue("https://lipdverse.org/data/{dsid}/{vers_}/")

  sidebarUrl <- "sidebar.html"
  mapUrl <- "../../map.html"
  paleoUrl <- "paleoPlots.html"
  chronUrl <- "chronPlots.html"

#create the components
  createWebComponents(L,webdir = webdir)


  #load in template iframe
  template <- readr::read_file(file.path(webdir,"iframeTemplate.html"))

  #replace terms and links
  template <- template %>%
    str_replace("datasetIdHere",dsid) %>%
    str_replace("DatasetNameHere",dsn) %>%
    str_replace("sideBarUrlHere",sidebarUrl) %>%
    str_replace("mapUrlHere",mapUrl) %>%
    str_replace("paleoDataGraphsUrlHere",paleoUrl) %>%
    str_replace("chronDataGraphsUrlHere",chronUrl)

  readr::write_file(template,file = file.path(webdir,"data",dsid,vers_,paste0(dsn,".html")))

  writeLipd(L,path = file.path(webdir,"data",dsid,vers_))

  addJsonldToHtml(htmlpath = file.path(webdir,"data",dsid,vers_,paste0(dsn,".html")),webdir = webdir)

  #copy to index
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,paste0(dsn,'.html'))} {file.path(webdir,'data',dsid,vers_)}/index.html"))

  #write dsid redirect html
  redirect <- readr::read_file(file.path(webdir,"redirectTemplate.html")) %>%
    str_replace("redirectUrlHere",dsPath)

  readr::write_file(redirect,file = file.path(webdir,"data",dsid,"index.html"))

#copy lipd to generic
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}.lpd {file.path(webdir,'data',dsid,vers_)}/lipd.lpd"))

}




