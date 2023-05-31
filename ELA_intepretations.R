
toAdd <- data.frame(
  stringsAsFactors = FALSE,
              TSid = c("PYTEAD50Q3E","GHb7d87372",
                       "PYTBCO0IORS","PYTU4HH4BH4","PYT2O41G134","PYTJ5STI3CN",
                       "PYTWWYWR3J1","WEB9887226c"),
       dataSetName = c("big_round.Thomas.2010",
                       "LakeAspvatnet_ELA.Bakke.2005","kjennsvatn.Bakke.2010",
                       "jarburvatnet.Nesje.2001","Hallet.McKay.2009",
                       "greyling.Mckay.2009","dravladalsvatn.Bakke.2005","Jenny.Larson.2016")
)


ELA <- D[toAdd$dataSetName]

ts <-extractTs(ELA) %>% ts2tibble()


ev <- which(ts$paleoData_TSid %in% toAdd$TSid)

toAdd$neg <- FALSE
toAdd$neg[c(1,8)] <- TRUE

ts$interpretation2_direction <- ts$interpretation2_variable <- ts$interpretation2_scope <- ts$interpretation2_seasonality <- ts$interpretation3_direction <- ts$interpretation3_variable <- ts$interpretation3_scope <- ts$interpretation3_seasonality <- NA

ts$interpretation2_variable[ev] <- "temperature"
ts$interpretation3_variable[ev] <- "precipitation"

ts$interpretation2_seasonality[ev] <- "summer"
ts$interpretation3_seasonality[ev] <- "winter"

ts$interpretation2_scope[ev] <- "climate"
ts$interpretation3_scope[ev] <- "climate"

ts$interpretation2_direction[ev] <- "positive"
ts$interpretation3_direction[ev] <- "negative"

ts$interpretation2_direction[ev[c(1,8)]] <- "negative"
ts$interpretation3_direction[ev[c(1,8)]] <- "positive"

tst <- as.lipdTsTibble(ts)
fixed <- as.multiLipd(tst)



