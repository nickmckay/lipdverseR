#fix summary links
library(stringr)
#"../changelogSummary.html" to "changelogSummary.html"


for(i in allIndex){
  ad <- readLines(i)

  if(any(str_detect(ad,pattern = "../changelogSummary.html" ))){
    adf <- str_replace_all(ad,pattern = "../changelogSummary.html",replacement = "changelogSummary.html")
    print(i)
    writeLines(adf,i)
  }

}
