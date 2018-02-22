# Compile Rmd files

## Install rmarkdown package and related packages
if(!require("rmarkdown")) install.packages("rmarkdown")

## list all rmd files
rmdfiles = list.files(pattern = "*.Rmd")

lapply(rmdfiles, function(rmdFileName) {
  rmarkdown::render(rmdFileName)
  # All html files should be in docs, to move files use rfunction file.rename
  # file.rename(from= blabla, to = blabla
  # otherwise just copy and paste the new generated file to ./docs directory
})

