#' Render an R Markdown document
#'
#' `render_doc` renders an R Markdown in a fresh R process using
#' `callr`
#'
#' This function assumes you are working within a project structure,
#' specifically an RStudio project. The output file will be saved at the
#' same location relative to `output_basedir` as `input` is
#' relative to `input_basedir`. For example, using the defaults,
#' if the input file is `code/folder1/folder2/report.Rmd`, the
#' output file will be saved to `output/folder1/folder2/report.Rmd`.
#' The user may select base input and output directories, relative
#' to which the paths are computed.
#'
#' To use this function in a R Markdown document, you can place the following
#' line in the YAML header:
#'
#' `knit: (function(inputFile, encoding) rutils::render_doc(inputFile))`
#'
#' @param input Input file
#' @param input_basedir Base input directory (`code` by default)
#' @param output_basedir Base output directory (`docs` by default)
#' @param output_file Output filename
#' @param params Parameters in the R Markdown document to execute
#' @param open Whether to open the output document using [rstudioapi::viewer]
#' @param ... Additional arguments passed to [rmarkdown::render]
#' @export
render_doc <- function(input,
                       input_basedir = fs::path(rprojroot::find_rstudio_root_file(), "code"),
                       output_basedir = fs::path(rprojroot::find_rstudio_root_file(), "docs"),
                       output_file = fs::path_ext_set(fs::path_file(input), "html"),
                       params = NULL,
                       open = FALSE,
                       ...) {
  checkmate::assert_file_exists(input)
  checkmate::assert_directory_exists(input_basedir)
  checkmate::assert_flag(open)

  input <- path_reg(input)
  input_basedir <- path_reg(input_basedir)
  output_basedir <- path_reg(output_basedir)
  output_file <- fs::path_file(output_file)

  output_dir <- fs::path(output_basedir, fs::path_rel(fs::path_dir(input), input_basedir))
  fs::dir_create(output_dir)
  output_file <- fs::path(output_dir, output_file)

  callr::r(
    function(input, output_file, params, ...) {
      rmarkdown::render(
        input,
        output_file = output_file,
        params = params,
        envir = globalenv(),
        ...
      )
    },
    args = list(
      input = input,
      output_file = output_file,
      params = params,
      ...
    ),
    show = TRUE
  )

  if (open && rstudioapi::isAvailable(version_needed = "1.3", child_ok = TRUE)) {
    rstudioapi::viewer(output_file)
  }
}

