#' Estimation of the ROC curve of logistic regression models with complex survey data
#'
#' Plot the ROC curve of a logistic regression model
#' considering sampling weights with complex survey data.
#' @param x An object of class \code{wroc} obtained by means of the function \code{wroc()}.
#' @param print.auc A logical value. If \code{TRUE}, the value of the area under the ROCw curve (AUCw) is printed (default \code{print.auc = TRUE}).
#' @param print.cutoff A logical value. If \code{TRUE}, the value of the optimal cut-off point, and the corresponding weighted estimates of the sensitivity and specificity parameters are printed (default \code{print.cutoff = TRUE}).
#' @param col.cutoff A character string indicating the color in which the cut-off point is depicted. The default option is \code{col.cutoff = "red"}.
#' @param cex.text A numeric value indicating the size with which the information of the AUCw and optimal cut-off point is printed. The default option is \code{cex.text = 0.75}.
#' @param round.digits A numeric value indicating the number of digits that will be employed when printing the information about the AUCw and optimal cut-off point. The default option is \code{round.digits = 4}.
#'
#' @details
#' More information is given in the documentation of the \code{wroc()}, \code{wauc{}} and \code{wocp()} functions.
#'
#'
#' @return a graph
#'
#' @export
#'
#' @examples
#' data(example_data_wroc)
#'
#' mycurve <- wroc(response.var = "y", phat.var = "phat", weights.var = "weights",
#'                 data = example_data_wroc,
#'                 tag.event = 1, tag.nonevent = 0,
#'                 cutoff.method = "Youden")
#' wroc.plot(x = mycurve, print.auc = TRUE, print.cutoff = TRUE)
#'
wroc.plot <- function(x, print.auc = TRUE, print.cutoff = FALSE,
                      col.cutoff = "red", cex.text = 0.75, round.digits = 4){

  if(!inherits(x, "wroc")){stop("Please, insert an object of class 'wroc', obtained using the function wroc().")}

  if(print.cutoff == TRUE & is.null(x[["optimal.cutoff"]])){
    cat("The optimal cut-off point cannot be printed given that this information has not saved in the object 'x'.")
  }

  # Objects:
  spw.v <- x[["wroc.curve"]][["Spw.values"]]
  sew.v <- x[["wroc.curve"]][["Sew.values"]]

  inv.spw.v <- 1 - spw.v
  plot.df <- data.frame(sew.v, inv.spw.v)

  # Plot the graph:
  plot(x = plot.df$inv.spw.v, y = plot.df$sew.v, type = "l",
       xlab = "1-Spw(c)", ylab = "Sew(c)", main = "ROCw Curve")

  # Print AUCw:
  if(print.auc){
    graphics::text(x = 0.5, y = 0, cex = cex.text,
                   labels = paste0("Area Under the ROCw Curve (AUCw): " ,
                                   round(x[["wauc"]], digits = round.digits)))
  }

  # Plot the cut-off point:
  if(print.cutoff == TRUE & !is.null(x[["optimal.cutoff"]])){

    graphics::points(x = 1-x[["optimal.cutoff"]][["Spw"]],
                     y = x[["optimal.cutoff"]][["Sew"]],
                     col = col.cutoff, pch=19)
    graphics::text(x = 1-x[["optimal.cutoff"]][["Spw"]],
                   y = x[["optimal.cutoff"]][["Sew"]],
                   pos = 4, cex = cex.text,
                   labels = paste0(round(x[["optimal.cutoff"]][["cutoff.value"]], digits = round.digits),
                                   " (Spw: ", round(x[["optimal.cutoff"]][["Spw"]], digits = round.digits),
                                   ", Sew: ", round(x[["optimal.cutoff"]][["Sew"]], digits = round.digits), ")"))

  }

  graphics::abline(a = 0, b = 1, lty = 2, col = "gray")



}
