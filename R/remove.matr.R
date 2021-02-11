#'  Function for extracting matrix peaks from the sample.
#'  Returns a cropped MALDI file cleaned from the peaks that are more intensive in the matrix
#'  Imports: Cardinal
#'  Imports: tidyr
#'  @file - MALDI file name
#'  @file.skmg - SpatialKMeans2 file for the same dataset as @file
#'  @s - number(s) of cluster(s) referring to the sample
#'  @threshold - threshold for matrix filtering
#'  example: cer.cl <- remove.matr(cer, cer.skmg, s = c(2,3), threshold = 1.5)


remove.matr <- function(file, file.skmg, s, threshold){

  pix <-  file.skmg@resultData@listData[[1]]$cluster %in% s
  file.sample <- file[, pix]
  file.matrix <- file[, !pix]
  file.sample.spec <- extract.spec(file.sample)
  file.matrix.spec <- extract.spec(file.matrix)
  file.sample.spec.means <- colMeans(file.sample.spec)
  file.matrix.spec.means <- colMeans(file.matrix.spec)
  mzlist <- mz(file)
  for (i in 1:length(colnames(file.sample.spec))){

    if (file.sample.spec.means[i] < threshold*file.matrix.spec.means[i]){
      mzlist[i] <- NA
  }
}
    mzlist <- drop_na(mzlist)
    file.sample <- peakAlign(file.sample, ref=mzlist, tolerance= 10, units="ppm")
    file.sample <- process(file.sample)
    return(file.sample)

}

