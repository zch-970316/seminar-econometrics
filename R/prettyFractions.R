# see [url](https://stackoverflow.com/questions/55858394/r-convert-decimals-to-pretty-fractions)

require(fractional)

prettyFractions <- function(x = NULL, smbl ="", signif = 3){
  humanity <- function(y){
    # y <- t$estPcnt
    if (!is.na(y)){
      d <- signif(y, digits = 3) %/% 1e-1
      
      c <- signif(y, digits = 3) %/% 1e-2
      m <- signif(y, digits = 3) %/% 1e-3
      m1 <- signif(y, digits = 3) %/% 1e-4
      m2 <- signif(y, digits = 3) %/% 1e-5
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( between(d, 1, 10) ) {
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-02, maxConv = 20, sync = TRUE))
      } else if ( between(c, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-03, maxConv = 20, sync = TRUE))
      } else if ( between(m, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-04, maxConv = 20, sync = TRUE))
      }else if( between(m1, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-06, maxConv = 20, sync = TRUE))
      } else {
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-09, maxConv = 20, sync = TRUE))
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x, humanity)
}