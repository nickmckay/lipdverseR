#uncertainty estimation
# library(geoChronR)
# library(tidyverse)
#
# minError <- 0.1
# maxError <- 6
# ee <- NA

meanUncertainty <- function(L,minError = 0.1,maxError = 6){
  tts <- lipdR::extractTs(L)
  otts <- tts
  proxy <- try(lipdR::pullTsVariable(tts,"paleoData_proxy"))
  if(class(proxy) == "try-error"){
    proxy <- matrix(NA,nrow = length(tts))
  }
  ee <- NA #if we can't figure it out, its NA

  up <- as.character(na.omit(unique(proxy)))


  for(i in 1:length(up)){


    if(length(up)>1){
      tts <- filterTs(otts,stringr::str_c("paleoData_proxy == ",up[i]))
    }
    vn <- lipdR::pullTsVariable(tts,"paleoData_variableName")


    uvn <- which(grepl("uncertainty",vn))

    if(length(uvn)==2){#good!
      us <- abs(tts[[uvn[1]]]$paleoData_values - tts[[uvn[2]]]$paleoData_values)
      mus <- mean(us,na.rm = TRUE)/2 #half the range
      if(mus >= minError & mus <= maxError){#good!
        ee <- mus
      }else{
        altmus <- mean(c(mean(tts[[uvn[1]]]$paleoData_values,na.rm = TRUE),mean(tts[[uvn[1]]]$paleoData_values,na.rm = TRUE)))
        if(altmus >= minError & altmus <= maxError){
          ee <- altmus
        }
      }
    }else if(length(uvn)==1){#maybe its just one?
      onemus <-  mean(tts[[uvn]]$paleoData_values,na.rm = TRUE)
      if(onemus >= minError & onemus <= maxError){#good!
        ee <- onemus
      }

    }
    tr <- tibble::tibble(dsn = tts[[1]]$dataSetName,proxy = up[i],meanError = ee)
    if(i == 1){
      out <- tr
    }else{
      out <- dplyr::bind_rows(out,tr)
    }
  }
  return(out)
}


# #summarize results
# library(tidyverse)
#
# uncSummary <- test %>%
#   filter(meanError >= 0) %>%
#   group_by(proxy) %>%
#   summarize(meanError = mean(meanError,na.rm = TRUE),  count = n())




