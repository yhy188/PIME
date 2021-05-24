#using ipw, input x1,x2,x3， surpervised by a;
#output the probility, then weight
#' Case Weighting of the Treatment Assignment for Conducting Experiments
#'
#' This function can be used to randomize the treatment assignment for
#'
#' Randomized-block designs refer to the complete randomization of the
#'
#' @aliases case weight weighting
#' @param datax A data frame containing the observations to which the treatments
#' are randomly assigned with weighting.
#' @param a A numerical or character vector indicating the
#' treatment/control groups. The length of the vector equals the total number
#' of such groups. The default specifies two groups called \dQuote{0} for treated and
#' \dQuote{0} for Control.
#' @param denominator An optional variable name in the data frame or a formula to be
#' used as the confounding variables for randomized-block designs.
#'  \code{x1,x2,x3} is specified the confounding variable vectors.
#' @return A list of weights
#' @author Haiyan Yu
#' \email{yhy188@gmail.com};
#' @keywords case weight
#' @export cweight
#'
caseweight <- function(datax) {
  library("ipw")
  #estimate ipw weights
  # x<-data.frame(x)
  temp <- ipwpoint(
    exposure = a,
    family = "binomial",
    link = "logit",
    numerator = NULL,
    denominator = ~ x1 + x2 + x3,
    data = datax
  )
  cweight <- temp$ipw.weights
  summary(cweight)

  return(cweight)

}
