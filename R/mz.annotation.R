#'  function mz.annotation by mz
#'  This function uses a string of mz values as an input and generates a list of putative lipids with three most common adducts
#' (H+, Na+, K+) by sendig automated requests to lipidmaps.org and parsing the output data into a table
#'  Imports: RCurl
#'  Imports: stringr
#'  @mzlist - a string of mz values to be annotated
#'  example: mz.values <- c(159.2841, 273.3309, 966.4836)
#'  annot.table <- mz.annotation(mz.values)

mz.annotation <- function(mzlist){

  mz.df <- data.frame(as.numeric(mzlist))
  mz.df[,2] <- mz.df[,1]*0.00001
  mz_ppm <- mz.df
  mz_ppm_add_1 <- mz_ppm
  mz_ppm_add_1[,3] <- "M+H"
  mz_ppm_add_2 <- mz_ppm
  mz_ppm_add_2[,3] <- "M+Na"
  mz_ppm_add_3 <- mz_ppm
  mz_ppm_add_3[,3] <- "M+K"
  mz_ppm_add <- rbind(mz_ppm_add_1, mz_ppm_add_2, mz_ppm_add_3)
  colnames(mz_ppm_add) <- c("mz", "ppm", "adduct")
  mz_ppm_add  <- mz_ppm_add[order(mz_ppm_add$mz),]
  mz <- mz_ppm_add[1,1]
  ppm <- mz_ppm_add[1,2]
  adduct <- mz_ppm_add[1,3]
  RESTURL <- 'http://www.lipidmaps.org/rest/moverz/LIPIDS/'
  query <- paste0(RESTURL, mz, '/', adduct, '/', ppm, '/txt')
  mz_res <-
    hlm_res <- httr::GET(query) %>% httr::content(., 'parsed')
  mz_res_clean <-
    mz_res %>% str_split('\n') %>% .[[1]]  %>% str_split('\t')
  df <- t(data.frame(mz_res_clean[[3]]))
  colnames(df) <- c("Input m/z", "Matched m/z" ,"Delta", "Name", "Formula", "Ion")

  for (i in 2:nrow(mz_ppm_add)){

    mz <- mz_ppm_add[i,1]
    ppm <- mz_ppm_add[i,2]
    adduct <- mz_ppm_add[i,3]

    RESTURL <- 'http://www.lipidmaps.org/rest/moverz/LIPIDS/'

    query <- paste0(RESTURL, mz, '/', adduct, '/', ppm, '/txt')
    mz_res <-
      hlm_res <- httr::GET(query) %>% httr::content(., 'parsed')

    mz_res_clean <-
      mz_res %>% str_split('\n') %>% .[[1]]  %>% str_split('\t')

    if (length(mz_res_clean)==6){
      x <- t(data.frame(mz_res_clean[[3]]))#t(data.frame(df[1,]))
      x[,1] <- mz
      x[,2:6] <- "NA"
      colnames(x) <- c("Input m/z", "Matched m/z" ,"Delta", "Name", "Formula", "Ion")
      df <- rbind(df,x)
    } else{
      for (k in 4:(length(mz_res_clean)-3)){

        x <- mz_res_clean[[k]]
        x <- x[-7]
        x <- t(data.frame(x))
        if (length(x)==1){
          x <- data.frame(df[1,])
          x[,1] <- mz
          x[,2:6] <- "NA"
          colnames(x) <- colnames(df)
          df <- rbind(df,x)
        }else {df <- rbind(df,x)}

      }
    }

  }

  return(df)
}
