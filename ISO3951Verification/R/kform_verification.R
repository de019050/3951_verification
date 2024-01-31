

# Define the function with five parameters
#' k-Form function
#'
#' @param input1 Mean value
#' @param input2 Standard deviation
#' @param input3 LSL lower specification limit
#' @param input4 USL upper specification limit
#' @param input5 kFac,  tabled acceptance criterion
#'
#' @return Result String
#' @export (readr, stats,dplry )
#'
#' @examples
#'
#' # Example ISO 3951-2 Chapter 16.2
#'  myKFunction(6.551,0.3251,4.0,NA,2.580)
#'
#' # Example ISO 3951-2 Chapter 16.2
#'  myKFunction(54.615385,3.3301267,NA,60,1.426)
#'
#'
#'
myKFunction <- function(input1, input2, input3 = NULL, input4 = NULL, input5) {
  MeanValue<-input1
  STD<-input2
  LSL<-input3
  USL<-input4
  kFac<-input5

    # Check if mandatory inputs are missing or NA
  if (missing(input1) || is.na(input1) || missing(input2) || is.na(input2) || missing(input5) || is.na(input5)) {
    stop("Mean Value, Standard Deviation, and k-Factor are mandatory and cannot be NA.")
  }

  # Determine which option to use based on the presence and value of optional inputs
  input3_present = !missing(input3) && !is.na(input3)
  input4_present = !missing(input4) && !is.na(input4)

  # Check if input4 is larger than input3 when both are present
  if (input3_present && input4_present && input4 <= input3) {
      print(input3)
      print(input4)
      stop("USL must be larger than LSL.")
  }

  if (!input3_present && !input4_present) {
    stop("At least one of LSL or USL is required.")
  } else if (input4_present && !input3_present) {
    QU= (USL-MeanValue)/STD
    #print(round(QU,digits=3))
    if (QU >= kFac){
      cat('The Lot meets the acceptability criterion. QU =',round(QU,digits=3),'is geq than kfac =',kFac,'\n')
    } else if (QU < kFac){
      cat('The Lot does not meet the acceptability criterion. QU =',round(QU,digits=3),'is less than kfac =',kFac,'\n')
    } else {
      cat('QU Check inputs')
    }
  } else if (input3_present && !input4_present) {
    QL= (MeanValue-LSL)/STD
    #print(round(QL,digits = 3))
    if (QL >= kFac){
      cat('The Lot meets the acceptability criterion. QL =',round(QL,digits=3),'is geq than kfac =',kFac,'\n')
    } else if (QL < kFac){
      cat('The Lot does not meet the acceptability criterion. QL =',round(QL,digits=3),'is less than kfac =',kFac,'\n')
    } else {
      cat('Check inputs')
      }
    } else {
    # Proceed with option 3
    print("Two limits! Use p-form!")
  }
}

# Example usage
# myFunction(input1, input2, input3, input4, input5)

# Input Data from both examples in Chapter 16.2

# ISO_KFORM_Input
#
# # Example1
# myFunction(54.615385,3.3301267,NA,60,1.426)
#
# # Example2
# myFunction(6.551,0.3251,4.0,NA,2.580)
