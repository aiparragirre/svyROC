optimize.criterion <- function(method = c("Youden", "MaxProdSpSe", "ROC01", "MaxEfficiency"),
                               criterion.v, se.v, sp.v, cutoffs){

  if(method %in% c("Youden", "MaxProdSpSe", "MaxEfficiency")){
    c <- cutoffs[which(round(criterion.v,10) == round(max(criterion.v,na.rm=TRUE),10))]
  } else {
    c <- cutoffs[which(round(criterion.v,10) == round(min(criterion.v,na.rm=TRUE),10))]
  }


  position <- which(cutoffs %in% c)

  optimal.se <- se.v[position]
  optimal.sp <- sp.v[position]
  optimal.cutoff <- c
  optimal.criterion <- unique(round(criterion.v[position], 10))

  res <- list(optimal = list(cutoff = optimal.cutoff,
                             Sew = optimal.se,
                             Spw = optimal.sp,
                             criterion = optimal.criterion),
              all = list(cutoff = cutoffs,
                         Sew = se.v,
                         Spw = sp.v,
                         criterion = criterion.v))
  return(res)


}
