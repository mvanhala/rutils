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
#' output file will be saved to `output/folder1/folder2/report.html`.
#' The user may select base input and output directories, relative
#' to which the paths are computed. The user may also directly
#' specify the output directory if preferred.
#'
#' To use this function in a R Markdown document, you can place the following
#' line in the YAML header:
#'
#' `knit: (function(inputFile, encoding) rutils::render_doc(inputFile))`
#'
#' @param input Input file
#' @param input_basedir Base input directory (optional). If not specified,
#' will be the first subfolder of the input file within the root directory.
#' For example, for input `code/folder1/report.Rmd`, the default `input_basedir`
#' will be `code`.
#' @param output_basedir Base output directory (default takes from the option
#' `rutils.render.output_basedir`, which is `output` by default)
#' @param output_dir Directly specify the output directory.
#' If specified, takes precedence over `output_basedir`.
#' @param output_file Optional, the filename of the output file(s).
#' If not provided, the default is the input filename.
#' Any directory part of `output_file` will be removed.
#' Only the filename part will be used.
#' The output directory is specified using `output_basedir` or `output_dir`.
#' Can be provided with or without an extension.
#' The extension will be removed and replaced with that determined by `output_format`.
#' @param output_format Optional, a vector of output file formats to which to render.
#' If not provided, `html_document` is the default.
#'
#' All available R Markdown
#' [output formats](https://bookdown.org/yihui/rmarkdown/output-formats.html)
#' are supported.
#' In parentheses, the extensions file extensions that will be created are specified.
#' For example, the format `github_document` will be saved `output_file.github.md`;
#' the format `html_document` will be saved as `output_file.html`.
#'
#' * beamer_presentation (.beamer.pdf)
#' * context_document (.context.pdf)
#' * github_document (.github.md)
#' * html_document (.html)
#' * ioslides_presentation (.ioslides.html)
#' * latex_document (.tex)
#' * md_document (.md)
#' * odt_document (.odt)
#' * pdf_document (.pdf)
#' * powerpoint_presentation (.pptx)
#' * rtf_document (.rtf)
#' * slidy_presentation (.slidy.html)
#' * word_document (.docx)

#' @param params Parameters in the R Markdown document to execute
#' @param open Whether to open the output document using [rstudioapi::viewer]
#' @param root Root criterion using a specification from [rprojroot::root_criterion].
#' Determines the root folder.
#' @param ... Additional arguments passed to [rmarkdown::render]
#' @export
render_doc <- function(input,
                       input_basedir = NULL,
                       output_basedir = getOption("rutils.render.output_basedir"),
                       output_dir = NULL,
                       output_file = NULL,
                       output_format = NULL,
                       params = NULL,
                       open = FALSE,
                       root = rprojroot::is_git_root | rprojroot::is_rstudio_project,
                       ...) {
  formats <- c(
    "context_document" = ".context.pdf",
    "github_document" = ".github.md",
    "html_document" = ".html",
    "ioslides_presentation" = ".ioslides.html",
    "latex_document" = ".tex",
    "md_document" = ".md",
    "odt_document" = ".odt",
    "pdf_document" = ".pdf",
    "powerpoint_presentation" = ".pptx",
    "rtf_document" = ".rtf",
    "slidy_presentation" = ".slidy.html",
    "word_document" = ".docx"
  )

  checkmate::assert_string(input)
  checkmate::assert_file_exists(input, extension = "Rmd")

  checkmate::assert_string(input_basedir, null.ok = TRUE)
  if (!is.null(input_basedir)) {
    checkmate::assert_directory_exists(input_basedir)
    checkmate::assert(
      fs::path_has_parent(input, input_basedir),
      .var.name = "input_basedir must be parent of input"
    )
  }

  checkmate::assert_string(output_basedir)
  checkmate::assert_string(output_dir, null.ok = TRUE)
  checkmate::assert_string(output_file, null.ok = TRUE)
  checkmate::assert_character(output_format, unique = TRUE, null.ok = TRUE)
  checkmate::assert_subset(output_format, names(formats))
  checkmate::assert_flag(open)
  checkmate::assert_class(root, "root_criterion")

  input <- path_reg(input)
  input_file <- fs::path_file(input)

  root_dir <- tryCatch(
    rprojroot::find_root(criterion = root, path = input),
    error = function(e) fs::path_dir(input)
  )

  if (is.null(output_dir)) {
    if (is.null(input_basedir)) {
      input_rel_root <- fs::path_rel(input, root_dir)
      input_parts <- fs::path_split(input_rel_root)[[1]]
      if (length(input_parts) > 1) {
        input_basedir <- fs::path(root_dir, input_parts[[1]])
      } else {
        input_basedir <- root_dir
      }
    }
    input_basedir <- path_reg(input_basedir)
    input_rel_base <- fs::path_rel(input, input_basedir)
    output_dir <- fs::path(root_dir, output_basedir, fs::path_dir(input_rel_base))
  }

  output_dir <- path_reg(output_dir)

  if (is.null(output_file)) output_file <- input
  if (is.null(output_format)) output_format <- "html_document"

  output_files <- fs::path_ext_set(
    rep(fs::path_file(output_file), length(output_format)),
    formats[output_format]
  )
  output_files <- fs::path(output_dir, output_files)

  fs::dir_create(output_dir)

  callr::r(
    function(input, output_format, output_file, params, ...) {
      rmarkdown::render(
        input,
        output_format = output_format,
        output_file = output_file,
        params = params,
        envir = globalenv(),
        ...
      )
    },
    args = list(
      input = input,
      output_format = output_format,
      output_file = output_files,
      params = params,
      ...
    ),
    show = TRUE
  )

  if (open &&
      rstudioapi::isAvailable(version_needed = "1.3", child_ok = TRUE) &&
      any(output_format == "html_document")) {
    try(rstudioapi::viewer(output_files[which(output_format == "html_document")]))
  }

  invisible(output_files)
}

