censor <- function(x) {
  UseMethod("censor")
}

# @export
censor.ldt <- function(x) {
  attr(x, "censor")
}



risk_table <- function(x) {
  UseMethod("risk_table")
}

# @export
risk_table.ldt <- function(x) {
  attr(x, "risk_table")
}



estimate <- function(x) {
  UseMethod("estimate")
}

# @export
estimate.ldt <- function(x) {
  attr(x, "estimate")
}



get_call <- function(x) {
  UseMethod("get_call")
}

# @export
get_call.ldt <- function(x) {
  attr(x, "call")
}
