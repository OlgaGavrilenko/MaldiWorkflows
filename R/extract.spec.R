#'  Function for extracting spectrum into a table for MALDI files
#'  Imports: Cardinal
#'  @file - MALDI file name
#'  example: cer.spec <- extract.spec(cer)



extract.spec <- function(file) {
  mz.500.0000 <- as.vector(head(spectra(file)[1,], n= nrow(coord(file))))
  file.allfeatures <- as.data.frame(mz.500.0000, row.names = NULL, col.names = "500.000")

  loop.length <- 2:length(mz(file))
  for (i in loop.length){

    x <- as.vector(head(spectra(file)[i,], n=nrow(coord(file))))
    file.allfeatures <- cbind(file.allfeatures, x)
  }
  colnames(file.allfeatures) <- mz(file)
  return(file.allfeatures)
}
