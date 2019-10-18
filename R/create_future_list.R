#' Create a future list object
#'
#' @param arg_list Master arg list
#'
#' @return
#' A nested list with time-intervals and scenarios
#' @export
#'
#' @examples
create_future_list <- function(arg_list){
  return(
    setNames(
      lapply(arg_list$time_interval_list, function(y) {
        setNames(lapply(arg_list$scenario_list, function(s) {
          return(NA)
        }),
        arg_list$scenario_list)
      }),
      arg_list$time_interval_list
    )
    )
}
