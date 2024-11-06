
criterion.function <- function(method = c("Youden", "MaxProdSpSe", "ROC01", "MaxEfficiency"), pop.prev = NULL, se.v, sp.v){

  criterion <- eval(parse(text = paste(method, ".criterion",sep = "")))(pop.prev = pop.prev, se.v = se.v, sp.v = sp.v)
  return(criterion)

}

Youden.criterion <- function(pop.prev = NULL, se.v, sp.v){

  criterion <- se.v + sp.v - 1
  return(criterion)

}

MaxProdSpSe.criterion <- function(pop.prev = NULL, se.v, sp.v){

  criterion <- sp.v * se.v
  return(criterion)

}


ROC01.criterion <- function(pop.prev = NULL, se.v, sp.v){

  criterion <- (sp.v-1)^2+(se.v-1)^2
  return(criterion)

}


MaxEfficiency.criterion <- function(pop.prev, se.v, sp.v){

  criterion <- pop.prev*se.v+(1-pop.prev)*sp.v
  return(criterion)

}
