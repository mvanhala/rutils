.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rutils <- list(
    rutils.render.output_basedir = "output"
  )
  toset <- !(names(op.rutils) %in% names(op))
  if (any(toset)) options(op.rutils[toset])
  invisible()
}
