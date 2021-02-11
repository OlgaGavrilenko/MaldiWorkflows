#'  function combining all MALDI processing steps in one
#'  Imports: Cardinal
#'  @file - MALDI file name
#'  @reference - reference mzlist for alignment
#'  example: cer.proc <- processing(cer, mz(cer2))


processing <- function(file, reference){

  file <-normalize(file, method="tic")
  file <- peakPick(file, method = "mad")
  file <- peakAlign(file, ref=reference, tolerance=10, units="ppm")
  file <- peakFilter(file, freq.min=0.03, rm.zero = TRUE)
  file <- process(file)
  return(file)

}


