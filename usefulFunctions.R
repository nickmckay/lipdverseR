getAllTsNames <- function(TS){
return(sort(unique(unlist(purrr::map(TS,names)))))
}

an <- getAllTsNames(sTS)

iw <- an[stringr::str_detect(pattern = "isotopeInterpretation[0-9]_variable",string = an)]
iw <- c("isotopeInterpretation1_variable","isotopeInterpretation2_variable","isotopeInterpretation3_variable")

#ii <- iw %>% 
  #purrr::map(pullTsVariable(TS = sTS,variable = .x))
uw <- vector(mode = "list",length = length(iw))
for(i in 1:length(iw)){
uw[[i]]  <- unique(pullTsVariable(TS = sTS,variable = iw[i]))
}
unique(unlist(uw))
lipdverseR::createNewQCSheet(data.frame(infMat = unique(unlist(uw))),qcName = "uniqueInterpretation")
