#' Edit a list
#'
#' Edit the elements of a list by name
#'
#' `list_edit` is similar to [purrr::list_modify] in
#' `purrr`, but unlike `list_modify`, does not operate recursively
#' on a list. It only edits elements at the top level of hierarchy.
#' Elements can be removed by setting them equal to `NULL`.
#'
#' @param .l List to edit
#' @param ... Named elements to add or delete
#' @param .add Should named elements not existing in the original list be added?
#'
#' @examples
#' list_edit(list(x = 1, y = 2), x = 10, z = 20)
#' list_edit(list(x = 1, y = 2), x = NULL, y = 5)
#' list_edit(list(x = 1, y = 2), !!!list(x = NULL, z = 100))
#' list_edit(list(x = 1, y = 2), x = 10, z = 20, .add = FALSE)
#' @export
list_edit <- function(.l, ..., .add = TRUE) {
  checkmate::assert_list(.l)
  checkmate::assert_flag(.add)
  x <- .l
  y <- rlang::list2(...)
  for (nm in intersect(names(x), names(y))) {
    x[[nm]] <- y[[nm]]
  }
  if (.add) {
    for (nm in setdiff(names(y), names(x))) {
      x[[nm]] <- y[[nm]]
    }
  }
  x
}

