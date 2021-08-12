#' @importFrom magrittr %>%
#' @export %>%
NULL

#' @importFrom rlang %||%
#' @export %||%
NULL

path_canonical <- function(path) fs::path_abs(fs::path_norm(fs::path_expand(path)))

