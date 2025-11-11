

generateRandomData <- function(nRows, nColumns, normalMean, normalStdev) {
  
  random_matrix <- matrix(
    rnorm(nRows * nColumns, mean = normalMean, sd = normalStdev),
    nrow = nRows,
    ncol = nColumns
  )
  
  df <- as.data.frame(random_matrix)
  
  colnames(df) <- paste0("Stilh", 1:nColumns)
  
  file_name <- paste0(nRows, "x", nColumns, ".csv")
  
  write.csv(df, file = file_name, row.names = FALSE)
  
 
}



generateRandomData(10000, 42, 0, 0.4)

