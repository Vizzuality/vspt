#' Check if out is all of a class
#'
#' @param arg_list Master argument list
#' @param l List to check
#' @param type Class to check
#'
#' @return
#' Logical true if all objects are class type, false if any are not or are missing
#' @export
#'
#' @examples
chk_output <- function(arg_list, l, type){
  tst <- sapply(l, is, class2=type)
  return((length(tst) == length(arg_list$spp_list) & all(tst)))
}
