#' @importFrom magrittr %>%
#' @export %>%
NULL

#' @importFrom rlang %||%
#' @export %||%
NULL

path_reg <- function(path) fs::path_abs(fs::path_norm(path))

