library(lipdR)
D <- readLipd("~/Dropbox/HoloceneLiPDLibrary/masterDatabase/")
TS <- extractTs(D)

dsn <- pullTsVariable(TS,"dataSetName")
ic <- pullTsVariable(TS,"paleoData_inCompilation")

temp12k <- which(ic == "Temp12k")
temp12kTverse <- which(ic == "Temp12k" | ic == "Tverse")

dsn12k <- unique(dsn[temp12k])
dsn12kverse <- unique(dsn[temp12kTverse])

D12k <- D[names(D) %in% dsn12k]
D12kverse <-  D[names(D) %in% dsn12kverse]

writeLipd(D12k,path = "~/Downloads/Temp12k/1_0_0/")
createSerializations(D12k,"/Users/npm4/Downloads","Temp12k","1_0_0")

writeLipd(D12kverse,path = "~/Downloads/Temp12kverse/1_0_0/")
createSerializations(D12kverse,"/Users/npm4/Downloads","Temp12kverse","1_0_0")
