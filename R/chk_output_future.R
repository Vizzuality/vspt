#' Check if future list is all of a class
#'
#' @param arg_list Master argument list
#' @param l List to check
#' @param type Class to check for
#'
#' @return
#' Logical, true if all objects are class type, false if any are not or are missing
#' @export
#'
#' @examples
chk_output_future <- function(arg_list, l, type){
  sapply(l, function(y){
    sapply(y, function(s){
      return(all(vspt::chk_output(s, arg_list=arg_list, type = type)))
    })
  })
}
