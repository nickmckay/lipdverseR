D <- readLipd("~/Dropbox/HoloceneLiPDLibrary/masterDatabase/")
TS <- extractTs(D)
py <- geoChronR::pullTsVariable(TS,"pub1_pubYear")
y <- geoChronR::pullTsVariable(TS,"pub1_year")


nTS <- fix_pubYear(TS)
py2 <- geoChronR::pullTsVariable(nTS,"pub1_pubYear")
y2 <- geoChronR::pullTsVariable(nTS,"pub1_year")

nD <- collapseTs(nTS)
TS2 <- extractTs(nD)
py3 <- geoChronR::pullTsVariable(TS2,"pub1_pubYear")
y3 <- geoChronR::pullTsVariable(TS2,"pub1_year")


#smaller test
L <- D$`18287-3.Kienast.2001`
ts <- extractTs(L)
L2 <- collapseTs(ts)

L$pub[[1]]$year
L2$pub[[1]]$year


nts <- fix_pubYear(ts)
nts[[1]]
l <- collapseTs(nts)

qcB <- getGoogleQCSheet("1kvc01LmYHfTjBxcdyJjGuUB4WFCeva7EZ_U6wF7Y7ig")
readr::write_csv(qcB,path = "~/Downloads/lastUpdate.csv")
