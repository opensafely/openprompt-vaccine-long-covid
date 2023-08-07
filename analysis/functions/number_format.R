fmt_numbers_for_plot <- function(x, sig = 2){
  y <- signif(x, sig)
  if(round(y, 2) < 0.1){
    z <- signif(x, 1)
  }else{
    z <- y
  }
  z
}