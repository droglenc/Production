## Get file info for dataPrepperLogs
tmp <- file.info(list.files("data/prepped/dataPrepper_logs/",".txt",full.names=TRUE))
tmp <- tmp[order(tmp$mtime,decreasing=TRUE),]

## Read two most recent
new <- readLines(rownames(tmp)[1])
old <- readLines(rownames(tmp)[2])

# find changed rows (delete first ... it will always be different)
( changed_rows <- which(new!=old)[-1] )

# look at changes
if (length(changed_rows)>0) {
  cat("SOME LINES IN THE DATA PREPPER LOG HAVE CHANGED!!!\n\n")
  cat("For lines that changed, the former file looked like this ...\n")
  writeLines(old[changed_rows])
  cat("\n... but the newer file looks like this ...\n")
  writeLines(new[changed_rows])  
} else print("There were no changes in the data prepper logs!!!")
