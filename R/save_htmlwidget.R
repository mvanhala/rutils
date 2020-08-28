#' Save htmlwidgets to an html file
#'
#' Convenience wrapper around \code{\link[htmlwidgets]{saveWidget}}
#'
#' There are three ways to save an htmlwidget. If \code{type} is \code{"direct"},
#' a self-contained HTML file is saved as usual. \code{save_widget} helps in this case
#' due to an apparent bug in \code{saveWidget} if attempting to save to a directory
#' other than the current working directory.
#'
#' If \code{type} is \code{"lib"}, a non-self-contained HTML file is created. In this
#' case, \code{"libdir"} may be provided. Also, \code{"libpath"} may be provided as either
#' \code{"relative"} (default) or \code{"absolute"}, which specifies whether paths
#' in the html file to the library files are relative or absolute. In contrast to
#' previous function \code{save_leaflet}, \code{"libdir"} does not necessarily need
#' to be a child of the directory containing \code{path}.
#'
#' If \code{type} is \code{"intermediate"}, a self-contained HTML file is generated, but
#' through an intermediate step of first creating a non-self-contained HTML file, then
#' producing the self-contained file from this by writing a new HTML after copying the
#' HTML dependencies into the HTML file. This can be useful when saving large htmlwidgets,
#' as the attempt to save as a self-contained file using \code{saveWidget} will
#' fail on a pandoc memory error. This approach enables the saving of larger
#' widgets as self-contained HTML files.
#'
#' @param widget htmlwidget
#' @param path Where to save HTMLf ile
#' @param title Title of HTML page
#' @param type One of \code{"direct"}, \code{"lib"}, or \code{"intermediate"}
#' @param libdir Directory for HTML dependencies (if \code{type} is \code{"lib"})
#' @param libpath Specify \code{"relative"} or \code{"absolute"} HTML dependency paths
#' (if \code{type} is \code{"lib"})
#' @export
save_widget <- function(widget, path, title = class(widget)[[1]], type = "direct", ...,
                        libdir = NULL, libpath = "relative") {
  path <- fs::path_abs(path)
  dir <- fs::path_dir(path)
  file <- fs::path_file(path)

  to_save <- structure(
    list(
      widget = widget,
      path = path,
      dir = dir,
      file = file,
      title = title,
      libdir = fs::path_abs(libdir %||% paste0(fs::path_ext_remove(file), "_files")),
      libpath = libpath
    ),
    class = type
  )

  save_any_htmlwidget(to_save, ...)

  invisible(path)
}

save_any_htmlwidget <- function(to_save, ...) {
  UseMethod("save_any_htmlwidget")
}

save_any_htmlwidget.direct <- function(to_save, ...) {
  withr::with_dir(
    to_save$dir,
    htmlwidgets::saveWidget(
      widget = to_save$widget,
      file = to_save$file,
      title = to_save$title,
      ...
    )
  )
}

save_any_htmlwidget.lib <- function(to_save, ...) {
  tmp <- fs::file_temp(ext = "html")
  tmp_files <- paste0(fs::path_ext_remove(tmp), "_files")
  on.exit(fs::file_delete(tmp), add = TRUE)
  on.exit(fs::dir_delete(tmp_files), add = TRUE)

  withr::with_dir(
    fs::path_dir(tmp),
    htmlwidgets::saveWidget(
      widget = to_save$widget,
      file = fs::path_file(tmp),
      title = to_save$title,
      selfcontained = FALSE,
      ...
    )
  )

  dep_files <- fs::dir_info(tmp_files, recurse = TRUE, type = "file") %>%
    dplyr::mutate(
      new_path = purrr::map_chr(
        path,
        function(path) {
          new_path <- fs::path(to_save$libdir, fs::path_rel(path, tmp_files))
          fs::dir_create(fs::path_dir(new_path))
          fs::file_copy(path, new_path, overwrite = TRUE)
          new_path
        }
      )
    )

  html <- xml2::read_html(tmp)

  scripts <- html %>%
    xml2::xml_find_all("head/script")

  purrr::walk(
    scripts,
    function(script) {
      new_path <- xml2::xml_attr(script, "src") %>%
        fs::path_rel(fs::path_file(tmp_files)) %>%
        fs::path(to_save$libdir, .) %>%
        fs::path_rel(to_save$dir)
      if (to_save$libpath == "absolute") new_path <- fs::path_abs(new_path)
      xml2::xml_set_attr(script, "src", new_path)
    }
  )

  links <- html %>%
    xml2::xml_find_all("head/link")

  purrr::walk(
    links,
    function(link) {
      new_path <- xml2::xml_attr(link, "href") %>%
        fs::path_rel(fs::path_file(tmp_files)) %>%
        fs::path(to_save$libdir, .) %>%
        fs::path_rel(to_save$dir)
      if (to_save$libpath == "absolute") new_path <- fs::path_abs(new_path)
      xml2::xml_set_attr(link, "href", new_path)
    }
  )

  xml2::write_html(html, to_save$path)
}

save_any_htmlwidget.intermediate <- function(to_save, ...) {
  tmp <- fs::file_temp(ext = "html")
  tmp_files <- paste0(fs::path_ext_remove(tmp), "_files")
  on.exit(fs::file_delete(tmp), add = TRUE)
  on.exit(fs::dir_delete(tmp_files), add = TRUE)

  withr::with_dir(
    fs::path_dir(tmp),
    htmlwidgets::saveWidget(
      widget = to_save$widget,
      file = fs::path_file(tmp),
      title = to_save$title,
      selfcontained = FALSE,
      ...
    )
  )

  html <- xml2::read_html(tmp)

  scripts <- html %>%
    xml2::xml_find_all("head/script")

  withr::with_dir(
    fs::path_dir(tmp),
    purrr::walk(
      scripts,
      function(script) {
        enc <- xml2::xml_attr(script, "src") %>%
          utils::URLdecode() %>%
          readr::read_file() %>%
          charToRaw() %>%
          base64enc::base64encode() %>%
          paste0("data:application/x-javascript;base64,", .)
        xml2::xml_set_attr(script, "src", enc)
      }
    )
  )

  links <- html %>%
    xml2::xml_find_all("head/link")

  withr::with_dir(
    fs::path_dir(tmp),
    purrr::walk(
      links,
      function(link) {
        txt <- xml2::xml_attr(link, "href") %>%
          utils::URLdecode() %>%
          readr::read_file() %>%
          utils::URLencode(reserved = TRUE) %>%
          paste0("data:text/css;charset=utf-8,", .)
        xml2::xml_set_attr(link, "href", txt)
      }
    )
  )

  xml2::write_html(html, to_save$path)
}

