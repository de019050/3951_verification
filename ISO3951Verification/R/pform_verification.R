

# My new Verification Pform function
# Define the function with seven parameters
#' Title
#'
#' @param input1
#' @param input2
#' @param input3
#' @param input4
#' @param input5
#' @param input6
#' @param input7
#'
#' @return Result String
#' @export
#'
#' @examples myPFunction(64.223,2.7899,60,70,0.051590,0.274,13)
#'
myPFunction <- function(input1, input2, input3, input4, input5, input6, input7) {
  MeanValue<-input1
  STD<-input2
  LSL<-input3
  USL<-input4
  pFac<-input5
  fsFac<-input6
  n<- input7

  # Check if any inputs are missing, NA, or not numeric
  inputs <- list(input1, input2, input3, input4, input5, input6, input7)
  if (any(sapply(inputs, is.na)) || any(sapply(inputs, is.null)) || !all(sapply(inputs, is.numeric))) {
    stop("All inputs must be provided, non-NA, and numeric.")
  }

  # Check if input4 is larger than input3
  if (USL <= LSL) {
    stop("USL must be larger than LSL.")
  }

  # Perform the operation
  # Example operation: sum of all inputs
  if (STD>(USL-LSL)*fsFac){
    cat ('Lot can already be rejected,  STD = ',STD,' is > than SMAX =',round((USL-LSL)*fsFac,digits=3),' But check 2. calculation!\n')
    q_l = (MeanValue-LSL)/STD
    x_l = 0.5*(1-(q_l*sqrt(n))/(n-1))
    p_hat_l = pbeta(as.numeric(x_l),(n-2)/2,(n-2)/2)

    cat ('q_l,x_l, p_hat_l',q_l, x_l, p_hat_l, '\n')
    q_u=(USL-MeanValue)/STD
    x_u=0.5*(1-(as.numeric(q_u)*sqrt(n))/(n-1))
    p_hat_u=pbeta(x_u,(n-2)/2,(n-2)/2)
    cat ('q_u,x_u, p_hat_u',q_u, x_u, p_hat_u, '\n')
    p_hat_val=p_hat_l+p_hat_u


    if (round(p_hat_val,digit=6) <= pFac){
      cat('The Lot meets the acceptability criterion.criterion. p^ = ',round(p_hat_val,digit=6),' is leq than p* =',pFac,'! Lot can be accepted!\n')
    } else if (round(p_hat_val,digit=6) > pFac){
      cat('The Lot does not meet the acceptability criterion. p^ = ',round(p_hat_val,digit=6),' is > than p* =',pFac,' ! Lot shall be rejected\n')
    }
  }
  #result <- sum(unlist(inputs))
  #return(result)

  q_l = (MeanValue-LSL)/STD
  x_l = 0.5*(1-(q_l*sqrt(n))/(n-1))
  p_hat_l = pbeta(as.numeric(x_l),(n-2)/2,(n-2)/2)

  cat ('q_l,x_l, p_hat_l',q_l, x_l, p_hat_l, '\n')
  q_u=(USL-MeanValue)/STD
  x_u=0.5*(1-(as.numeric(q_u)*sqrt(n))/(n-1))
  p_hat_u=pbeta(x_u,(n-2)/2,(n-2)/2)
  cat ('q_u,x_u, p_hat_u',q_u, x_u, p_hat_u, '\n')
  p_hat_val=p_hat_l+p_hat_u

  if (round(p_hat_val,digit=6) <= pFac){
    cat('The Lot meets the acceptability criterion.criterion. p^ = ',round(p_hat_val,digit=6),' is leq than p* =',pFac,'! Lot can be accepted!\n')
  } else if (round(p_hat_val,digit=6) > pFac){
    cat('The Lot does not meet the acceptability criterion. p^ = ',round(p_hat_val,digit=6),' is > than p* =',pFac,' ! Lot shall be rejected!\n')
  }

}

# # Input Data from  examples in Chapter 16.3.2
#
# ISO_PFORM_Input
# # Example usage
# #myPFunction(MeanValue,STD,LSL,USL,pFac,fsFac,n)
# # Example Chapter 16.3.2.2
# myPFunction(3.5, 7.436,-10 , 10, 0.1925, 0.475, 3)
# # Example Chapter 16.3.2.3
# myPFunction(82.5,0.4082,82,84,0.0860,0.365,4)
#
# # Example Chapter 16.3.2.4
# myPFunction(64.223,2.7899,60,70,0.051590,0.274,13)
# # Example Chapter 16.3.2.5
# myPFunction(64.223,2.7899,60,70,0.06466,0.285,13)
