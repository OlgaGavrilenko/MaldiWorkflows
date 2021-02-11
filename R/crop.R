#'  function for MALDI file cropping
#'  Imports: Cardinal
#'  @file - MALDI file name
#'  @xlim - c(x1, x2) of the cropped file
#'  @ylim - c(y1, y2) of the cropped file
#'  example: cerebellum.crop <- crop(cer, xlim=c(50,100), ylim=c(150,200))


crop <- function(file, xlim, ylim){

  file<- file[, !duplicated(coord(file))]
  file.crop <- file[, coord(file)$x > xlim[1] ]
  file.crop <- file.crop[, coord(file.crop)$x < xlim[2] ]
  file.crop <- file.crop[, coord(file.crop)$y > ylim[1] ]
  file.crop <- file.crop[, coord(file.crop)$y < ylim[2] ]

  return(file.crop)

}



