#' p-Form function (s-Method only, with mode selection)
#'
#' @param input1 Mean value
#' @param input2 Standard deviation
#' @param input3 LSL lower specification limit
#' @param input4 USL upper specification limit
#' @param input5 p*, tabled acceptance criterion
#' @param input6 fs, tabled factor to calculate S_max
#' @param input7 n, sample size
#' @param input8 Mode of operation, must be either "adhoc" or "batch"
#'
#' @return Depending on the mode, returns a detailed string for "adhoc" or numeric values for "batch"
#' @export
#'
#' @examples
#'
#'
#'
#' # Example usage
#' #myPFunction(MeanValue,STD,LSL,USL,pFac,fsFac,n,"mode")
#' # Example Chapter 16.3.2.2
#' myPFunction(3.5, 7.436,-10 , 10, 0.1925, 0.475, 3, "adhoc")
#' myPFunction(3.5, 7.436,-10 , 10, 0.1925, 0.475, 3, "batch")
#' # Example Chapter 16.3.2.3
#' myPFunction(82.5,0.4082,82,84,0.0860,0.365,4, "adhoc")
#' myPFunction(82.5,0.4082,82,84,0.0860,0.365,4, "adhoc")
#'
#' # Example Chapter 16.3.2.4
#' myPFunction(64.223,2.7899,60,70,0.051590,0.274,13, "adhoc")
#' myPFunction(64.223,2.7899,60,70,0.051590,0.274,13, "batch")
#' # Example Chapter 16.3.2.5
#' myPFunction(64.223,2.7899,60,70,0.06466,0.285,13, "adhoc")
#' myPFunction(64.223,2.7899,60,70,0.06466,0.285,13, "batch")
#'
myPFunction <- function(input1, input2, input3, input4, input5, input6, input7, input8) {
  MeanValue <- input1
  STD <- input2
  LSL <- input3
  USL <- input4
  pFac <- input5
  fsFac <- input6
  n <- input7
  mode <- tolower(input8) # Ensure mode is in lower case

  # Check if any inputs are missing, NA, or not numeric, and if mode is valid
  if (any(sapply(list(input1, input2, input3, input4, input5, input6, input7), is.na)) ||
      !all(sapply(list(input1, input2, input3, input4, input5, input6, input7), is.numeric)) ||
      !input8 %in% c("adhoc", "batch")) {
    stop("All inputs must be provided, non-NA, numeric, and mode must be either 'adhoc' or 'batch'.")
  }

  if (USL <= LSL) {
    stop("USL must be larger than LSL.")
  }

  # Calculate q_l, x_l, p_hat_l, q_u, x_u, p_hat_u
  q_l <- (MeanValue - LSL) / STD
  x_l <- 0.5 * (1 - (q_l * sqrt(n)) / (n - 1))
  p_hat_l <- pbeta(x_l, (n-2)/2, (n-2)/2)

  q_u <- (USL - MeanValue) / STD
  x_u <- 0.5 * (1 - (q_u * sqrt(n)) / (n - 1))
  p_hat_u <- pbeta(x_u, (n-2)/2, (n-2)/2)

  p_hat_val <- p_hat_l + p_hat_u

  if (mode == "batch") {
    return(list(STD = (USL-LSL) * fsFac, p_hat_val = round(p_hat_val,digits=6)))
  } else {
    if (STD > (USL-LSL) * fsFac) {
      cat(sprintf("Lot can already be rejected, STD = %f is > than SMAX = %f. But check 2nd calculation!\n", STD, round((USL-LSL)*fsFac, digits=3)))
    }

    cat(sprintf("q_l = %f, x_l = %f, p_hat_l = %f\n", q_l, x_l, p_hat_l))
    cat(sprintf("q_u = %f, x_u = %f, p_hat_u = %f\n", q_u, x_u, p_hat_u))

    if (round(p_hat_val, digits=6) <= pFac) {
      cat(sprintf("The Lot meets the acceptability criterion. p^ = %f is leq than p* = %f! Lot can be accepted!\n", round(p_hat_val, digits=6), pFac))
    } else {
      cat(sprintf("The Lot does not meet the acceptability criterion. p^ = %f is > than p* = %f! Lot shall be rejected\n", round(p_hat_val, digits=6), pFac))
    }
  }
}
