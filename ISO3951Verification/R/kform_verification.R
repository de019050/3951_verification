#' k-Form function (s-Method only, with mode selection)
#'
#' @param input1 Mean value
#' @param input2 Standard deviation
#' @param input3 LSL lower specification limit
#' @param input4 USL upper specification limit
#' @param input5 kFac, tabled acceptance criterion
#' @param input6 Mode of operation, must be either "adhoc" or "batch"
#'
#' @return Depending on the mode, returns a detailed string for "adhoc" or a numeric value for "batch"
#' @export
#'
#' @examples
#'
#' #' #  Example usage
#' #mykFunction(MeanValue,STD,LSL,USL,kFac,mode)
#'
#' # Example ISO 3951-2 Chapter 16.2
#'  myKFunction(6.551,0.3251,4.0,NA,2.580,'batch')
#'  myKFunction(6.551,0.3251,4.0,NA,2.580,'adhoc')
#'
#' # Example ISO 3951-2 Chapter 16.2
#'  myKFunction(54.615385,3.3301267,NA,60,1.426,'batch')
#'  myKFunction(54.615385,3.3301267,NA,60,1.426,'adhoc')
#'
#'
myKFunction <- function(input1, input2, input3 = NULL, input4 = NULL, input5, input6) {
  MeanValue <- input1
  STD <- input2
  LSL <- input3
  USL <- input4
  kFac <- input5
  mode <- tolower(input6) # Ensure mode is in lower case

  # Check if mandatory inputs are missing or NA, including the new mandatory 'input6'
  if (missing(input1) || is.na(input1) ||
      missing(input2) || is.na(input2) ||
      missing(input5) || is.na(input5) ||
      missing(input6) || !input6 %in% c("adhoc", "batch")) {
    stop("Mean Value, Standard Deviation, k-Factor, and Mode ('adhoc' or 'batch') are mandatory and cannot be NA or invalid.")
  }

  # input3_present <- !missing(input3) && !is.na(input3)
  # input4_present <- !missing(input4) && !is.na(input4)
  # changed missing into is.null because batch mode call causes error
  input3_present <- !is.null(input3) && !is.na(input3)
  input4_present <- !is.null(input4) && !is.na(input4)

  #
  if (input3_present && input4_present && input4 <= input3) {
    stop("USL must be larger than LSL.")
  }

  if (!input3_present && !input4_present) {
    stop("At least one of LSL or USL is required.")
  } else {
    if (input4_present && !input3_present) {
      QU <- (USL - MeanValue) / STD
      if (mode == "batch") {
        return(round(QU, digits = 3))
      }
      cat('The Lot meets the acceptability criterion. QU =', round(QU, digits = 3), 'is geq than kfac =', kFac, '\n')
    } else if (input3_present && !input4_present) {
      QL <- (MeanValue - LSL) / STD
      if (mode == "batch") {
        return(round(QL, digits = 3))
      }
      cat('The Lot meets the acceptability criterion. QL =', round(QL, digits = 3), 'is geq than kfac =', kFac, '\n')
    } else {
      print("Two limits! Use p-form!")
    }
  }
}
