assignPrimaryTimeColumns <- function(L){
  ts <- as.lipdTsTibble(L)

  #how many measurement tables:
  at <- unique(ts$tableNumber)
  ap <- unique(ts$paleoNumber)

  ao <- dplyr::select(ts,paleoNumber,tableNumber,tableType) %>%
    dplyr::filter(tableType == "meas") %>%
    group_by(paleoNumber,tableNumber) %>%
    summarize(count = n()) %>%
    filter(count > 0)

  tt.list <- vector(mode = "list",length = nrow(ao))

  for(i in 1:nrow(ao)){
    t <- ao$tableNumber[i]
    p <- ao$paleoNumber[i]

    #reset TSid
    timeTsid <- NA
    #get this table
    tt <- dplyr::filter(ts,tableNumber == t & paleoNumber == p)


    #check to see if the isPrimary column exists:
    if(all(is.null(tt$paleoData_isPrimary))){
      tt$paleoData_isPrimary <- FALSE
    }

    tt$paleoData_isPrimary[is.na(tt$paleoData_isPrimary)] <- FALSE


      #check for primary age column
      if(!all(is.null(tt$paleoData_primaryAgeColumn))){
        tt$paleoData_primaryAgeColumn[is.na(tt$paleoData_primaryAgeColumn)] <- FALSE
        if(any(tt$paleoData_primaryAgeColumn)){
          ti <- which(tt$paleoData_primaryAgeColumn)
          tt$paleoData_isPrimary[ti] <- TRUE
          timeTsid <- tt$paleoData_TSid[ti[1]]
        }
      }else{#if isPrimary, but not primaryAge column then work through that logic
        tt$paleoData_primaryAgeColumn <- FALSE #create a primary age column

        if(any(tt$paleoData_isPrimary)){

          agei <- which(tt$paleoData_isPrimary & tolower(tt$paleoData_variableName) == "age")
          if(length(agei) >= 1){
            tt$paleoData_primaryAgeColumn[agei] <- TRUE
            timeTsid <- tt$paleoData_TSid[agei[1]]
          }else{
            yeari <- which(tt$paleoData_isPrimary & tolower(tt$paleoData_variableName) == "year")
            if(length(yeari) >= 1){
              tt$paleoData_primaryAgeColumn[yeari] <- TRUE
              timeTsid <- tt$paleoData_TSid[yeari[1]]
            }
          }
        }
      }

      #if there's still no timeTsid at this point, lets try figure it out by variableName
      if(is.na(timeTsid)){
        agei <- which(tolower(tt$paleoData_variableName) == "age")
        if(length(agei) >= 1){
          tt$paleoData_primaryAgeColumn[agei] <- TRUE
          tt$paleoData_isPrimary[agei] <- TRUE
          timeTsid <- tt$paleoData_TSid[agei[1]]
        }else{
          yeari <- which(tolower(tt$paleoData_variableName) == "year")
          if(length(yeari) >= 1){
            tt$paleoData_primaryAgeColumn[yeari] <- TRUE
            tt$paleoData_isPrimary[yeari] <- TRUE
            timeTsid <- tt$paleoData_TSid[yeari[1]]
          }
        }
      }

      #ok, we're ready to assign in the TSids, whether or not we have them
      tt$paleoData_hasTimeTsid <- timeTsid

      #now plug back into ts
      tt.list[[i]] <- tt
    }

  tsn <- purrr::list_rbind(tt.list)

  Lnew <- as.lipd(tsn)

  cl <- createChangelog(L,Lnew)
  Lnew <- updateChangelog(Lnew,changelog = cl,notes = "added isPrimary, primaryAgeColumn, and hasTimeTsid")
  return(Lnew)

}
