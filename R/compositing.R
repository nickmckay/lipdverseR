#
# notEqual = c()
# composited = c()
# onlyone = c()
# for(i in 1:length(D)){
# L <- D[[i]]
#   ts <- extractTs(L)
#   sts <- splitInterpretationByScope(ts)
#
#   sg <- try(lipdR::pullTsVariable(sts,"climateInterpretation1_seasonalityGeneral"),silent = TRUE)
#   if(class(sg)=="try-error"){
#     next
#   }
#
#   tc <- which(str_detect(sg,"combine"))
#
#   if(length(tc) > 1){
#     print(paste("compositing",L$dataSetName))
#     nts <- try(composite_sameSamples(ts,tc,minOverlap = 6))
#     if(class(nts)=="try-error"){
#       notEqual = c(notEqual,L$dataSetName)
#     }else{
#     composited = c(composited,L$dataSetName)
#     L <- collapseTs(nts)
#     writeLipd(L,"~/Dropbox/HoloceneLiPDLibrary/masterDatabase/")
#     }
#   }else if(length(tc)==1){
#     onlyone <- c(onlyone,L$dataSetName)
#   }
# }


#composite on same depth scale
composite_sameSamples <- function(ts, tc, minOverlap = 10, removeMean = TRUE, scaleVariance = FALSE){

if(length(tc) <= 1){
  stop("Not enough timeseries to composite")
}

vn <- lipdR::pullTsVariable(ts,"paleoData_variableName")
if(any(grepl("Composite",vn))){
  hc <- which(grepl("Composite",vn))
  for(h in hc){
    if(stringr::str_detect(ts[[h]]$paleoData_QCnotes,"this timeseries was composited programmatically, with the assumption that all values are measured at equivalent levels")){
      stop("already composited!")
    }
  }

}


#check to make sure they're the same length
al <- map_int(ts[tc],function(x) length(x$paleoData_values))

if(length(unique(al))!=1){#diff length
  stop("not all the series are the same lengths")
}

#build matrix
tcm <- matrix(NA, nrow = al[1],ncol = length(al))

for(i in 1:length(tc)){
  tcm[,i] <- ts[[tc[i]]]$paleoData_value
}

#find rows for compositing
fullrow <- which(rowSums(is.na(tcm))==0)

if(length(fullrow)<minOverlap){
  stop("not enough rows to composite. Consider changing the minOverlap parameter")
}

#center and scale the matrix in preparation
if(removeMean & !scaleVariance){
sm <- scale(tcm,center = apply(tcm[fullrow,],2,mean),scale = FALSE)
}else if(!removeMean & !scaleVariance){
  sm <- tcm
}else if(!removeMean & scaleVariance){
  sm <- scale(tcm,center = FALSE, scale = apply(tcm[fullrow,],2,sd))
}else{
  sm <- scale(tcm,center = apply(tcm[fullrow,],2,mean), scale = apply(tcm[fullrow,],2,sd))
}

#calculate the composite
comp <- rowMeans(sm,na.rm = TRUE)

#build new TS entry
toignore <- c("paleoData_TSid","paleoData_hasMaxValue","paleoData_hasMinValue","paleoData_hasMeanValue","paleoData_hasMedianValue","paleoData_useInGlobalTemperatureAnalysis")
ci <- ts[[tc[1]]]
for(i in 1:length(ci)){
  if(length(ci[[i]])==1 & !names(ci)[i] %in% toignore){#only look at single instance things
  vars <- lipdR::pullTsVariable(ts[tc],names(ci)[i])
  vars <- vars[which(!is.na(vars))]

  uvars <- unique(vars)
  if(length(uvars) > 1){#then keep going
    print(paste(names(ci)[i],"=",paste(uvars, collapse = "; ")))
    ci[i] <- paste(uvars, collapse = "; ")
  }
  }
}

#handle specials
ci$paleoData_TSid <- lipdR::createTSid()
ci$paleoData_values <- comp
ci$paleoData_useInGlobalTemperatureAnalysis <- "TRUE"
ci$paleoData_inCompilation <- "temp12k"
ci$paleoData_variableName <- paste0(ci$paleoData_variableName,"Composite")
if(scaleVariance){
  ci$paleoData_units <- "unitless"
}
ci$paleoData_QCnotes <- paste(ci$paleoData_QCnotes,"; this timeseries was composited programmatically, with the assumption that all values are measured at equivalent levels")

#remove
nas <- matrix(NA,nrow = length(ts))
ts <- pushTsVariable(ts,"paleoData_inCompilation",nas,createNew = TRUE)

#put back into ts
ts[[length(ts)+1]] <- ci



return(ts)
}
