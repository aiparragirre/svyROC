

# AUC correction methods:

# -------------------------------------------------------------------------
# --> dCV
# -------------------------------------------------------------------------

cor.dCV <- function(data,
                    response.var, weights.var, strata.var, cluster.var, k = 10, R = 1, method = c("average", "pooling"),
                    formula, tag.event, tag.nonevent){

  if(method == "average"){
    dCV.sw.test = FALSE
  }
  if(method == "pooling"){
    dCV.sw.test = TRUE
  }

  rw.data <- svyVarSel::replicate.weights(data, method = "dCV",
                                       cluster = cluster.var, strata = strata.var, weights = weights.var,
                                       k = k, R = R, rw.test = TRUE, dCV.sw.test = dCV.sw.test)

  if(method == "average"){

    test.aucs <- matrix(NA, nrow = R, ncol = k)
    for(rr in 1:R){
      for(kk in 1:k){
        train.weights <- paste0("rw_r_",rr,"_train_",kk)
        test.weights <- paste0("rw_r_",rr,"_test_",kk)
        if(is.null(cluster.var)){
          sampling.design <- survey::svydesign(id=~1, strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
        } else {
          sampling.design <- survey::svydesign(id=~get(cluster.var), strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
        }
        suppressWarnings(train.model <- survey::svyglm(formula, design=sampling.design, family=quasibinomial()))
        phat.var <- as.numeric(predict(train.model, newdata = rw.data, type="response"))
        test.aucs[rr,kk] <- wauc(response.var = rw.data[,response.var], phat.var = phat.var, weights.var = rw.data[,test.weights], tag.event = tag.event, tag.nonevent = tag.nonevent)$AUCw
      }
    }

  }

  if(method == "pooling"){

    test.aucs <- rep(NA, R)

    for(rr in 1:R){

      p.hat <- rep(NA, nrow(data))

      for(kk in 1:k){
        train.weights <- paste0("rw_r_",rr,"_train_",kk)
        test.weights <- paste0("sw_r_",rr,"_test_",kk)
        if(is.null(cluster.var)){
          sampling.design <- survey::svydesign(id=~1, strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
        } else {
          sampling.design <- survey::svydesign(id=~get(cluster.var), strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
        }
        suppressWarnings(train.model <- survey::svyglm(formula, design=sampling.design, family=quasibinomial()))
        p.hat[which(rw.data[,test.weights]!=0)] <- train.model$fitted.values[which(rw.data[,test.weights]!=0)]
      }

      test.aucs[rr] <- wauc(response.var = data[,response.var],
                            phat.var = p.hat,
                            weights.var = data[,weights.var],
                            tag.event = tag.event, tag.nonevent = tag.nonevent)$AUCw

    }

  }

  corrected.AUC <- mean(test.aucs)
  return(corrected.AUC)

}



# -------------------------------------------------------------------------
# --> JKn
# -------------------------------------------------------------------------

cor.JKn <- function(data, response.var, weights.var, strata.var, cluster.var,
                    formula, tag.event, tag.nonevent){

  rw.data <- svyVarSel::replicate.weights(data, method = "JKn",
                                       cluster = cluster.var, strata = strata.var, weights = weights.var,
                                       rw.test = TRUE)

  p.hat <- rep(NA, nrow(data))
  rwtraincols <- grep("rw_r_1_train_", colnames(rw.data))
  k <- length(rwtraincols)

  for(kk in 1:k){
    train.weights <- paste0("rw_r_1_train_",kk)
    test.weights <- paste0("sw_r_1_test_",kk)
    if(is.null(cluster.var)){
      sampling.design <- survey::svydesign(id=~1, strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
    } else {
      sampling.design <- survey::svydesign(id=~get(cluster.var), strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
    }
    suppressWarnings(train.model <- survey::svyglm(formula, design=sampling.design, family=quasibinomial()))
    p.hat[which(rw.data[,test.weights]!=0)] <- train.model$fitted.values[which(rw.data[,test.weights]!=0)]
  }

  corrected.AUC <- wauc(response.var = response.var, phat.var = p.hat, weights.var = weights.var, tag.event = tag.event, tag.nonevent = tag.nonevent, data = data)$AUCw

  return(corrected.AUC)

}


# -------------------------------------------------------------------------
# --> RB
# -------------------------------------------------------------------------

cor.RB <- function(data,
                   response.var, weights.var, strata.var, cluster.var,
                   B = 200,
                   formula,
                   method = c("subbootstrap", "bootstrap"),
                   tag.event, tag.nonevent){

  if(is.null(cluster.var)){
    sampling.design <- survey::svydesign(id=~1, strata=~get(strata.var), weights=~get(weights.var), data=data, nest=TRUE)
  } else {
    sampling.design <- survey::svydesign(id=~get(cluster.var), strata=~get(strata.var), weights=~get(weights.var), data=data, nest=TRUE)
  }
  s.model <- survey::svyglm(formula, design=sampling.design, family=quasibinomial())
  auc.w.app <- wauc(response.var = s.model$y, phat.var = s.model$fitted.values, weights.var = data[,weights.var])$AUCw

  rw.data <- svyVarSel::replicate.weights(data, method = method,
                                       cluster = cluster.var, strata = strata.var, weights = weights.var,
                                       B = B, rw.test = TRUE)

  opt <- rep(NA, B)
  for(b in 1:B){
    # Bootstrap laginetako auc.w.app.b
    train.weights <- paste0("rw_r_1_train_",b)
    if(is.null(cluster.var)){
      b.design <- survey::svydesign(id=~1, strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
    } else {
      b.design <- survey::svydesign(id=~get(cluster.var), strata=~get(strata.var), weights=~get(train.weights), data=rw.data, nest=TRUE)
    }
    suppressWarnings(b.model <- survey::svyglm(formula, design=b.design, family=quasibinomial()))
    if(sum(is.na(b.model$coefficients)) > 0){
      b.model$coefficients[which(is.na(b.model$coefficients))] <- 0
      opt[b] <- NA
    } else {
      phat.var <- as.numeric(predict(b.model, newdata = rw.data, type="response"))
      auc.w.boot.b <- wauc(response.var = rw.data[,response.var], phat.var = phat.var, weights.var = rw.data[,train.weights])$AUCw
      auc.w.boot.s <- wauc(response.var = rw.data[,response.var], phat.var = phat.var, weights.var = rw.data[,weights.var])$AUCw
      opt[b] <- auc.w.boot.b - auc.w.boot.s
    }

  }

  o <- mean(opt)
  auc.RB <- auc.w.app - o

}



