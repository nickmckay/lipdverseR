#count instances of all variables
library(purrr)
uf <- sort(unique(unlist(sapply(sTS,names))))

tf <- function(f){sum(!is.na(pullTsVariable(sTS,f)))}


t <- map(uf,tf)

counts <- data.frame(fields = uf, count = unlist(t))


