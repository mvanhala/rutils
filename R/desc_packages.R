#' Update DESCRIPTION file with package dependencies
#'
#' \code{desc_pkgs} will search code in a package for dependencies,
#' using both \code{renv::dependencies} to search R and RMD files,
#' and by detecting dependencies declared with \code{roxygen2} decorators.
#' It will write these
#' package dependencies to the DESCRIPTION file.
#'
#' @param dir Root package directory
#' @param file DESCRIPTION file name
#' @param suggests Path(s) to search for dependencies to put in SUGGESTS (supports regular expressions)
#' @param ignore Path(s) to ignore in getting dependencies (supports regular expressions)
#' @param must_keep List of packages that must be retained even if not a current dependency
#' @param quiet Whether to print messages when \code{renv::dependencies} is called
#' @export
desc_pkgs <- function(dir = ".",
                      file = "DESCRIPTION",
                      suggests = c("^inst/"),
                      ignore = c("^data-raw/"),
                      must_keep = character(0),
                      quiet = TRUE) {
  path <- fs::path_abs(fs::path(dir, file))
  desc <- desc::description$new()
  pkg_name <- desc$get_field("Package")
  pkg_base <- fs::path_abs(dir)
  pkg_files <- fs::dir_ls(pkg_base, regexp = "[.]R$|[.]Rmd$", recurse = TRUE) %>%
    stringr::str_replace_all(paste0(pkg_base, "/"), "")

  roxy_imports <- roxygen2::parse_package(dir, env = NULL) %>%
    purrr::map("tags") %>%
    purrr::flatten() %>%
    purrr::keep(~ .$tag %in% c("importFrom", "import", "importClassesFrom", "importMethodsFrom")) %>%
    purrr::map_chr(~ .$val[[1]])

  imports_ignore <- stringr::str_c(c(ignore, suggests), collapse = "|")
  imports <- pkg_files

  if (length(ignore) + length(suggests) > 0) {
    imports <- purrr::discard(imports, stringr::str_detect, imports_ignore)
  }

  imports <- imports %>%
    fs::path(pkg_base, .) %>%
    purrr::map(renv::dependencies, quiet = quiet) %>%
    purrr::map("Package") %>%
    purrr::flatten_chr() %>%
    c(roxy_imports) %>%
    unique() %>%
    sort() %>%
    setdiff(pkg_name)

  suggests <- pkg_files %>%
    purrr::keep(stringr::str_detect, "^inst/")

  if (length(ignore) > 0) {
    suggests_ignore <- stringr::str_c(ignore, collapse = "|")
    suggests <- purrr::discard(suggests, stringr::str_detect, suggests_ignore)
  }

  suggests <- suggests %>%
    fs::path(pkg_base, .)  %>%
    purrr::map(renv::dependencies, quiet = quiet) %>%
    purrr::map("Package") %>%
    purrr::flatten_chr() %>%
    unique() %>%
    sort() %>%
    setdiff(c(imports, pkg_name))


  current_dep <- desc$get_deps() %>%
    dplyr::distinct(package, version)

  new_dep <- dplyr::bind_rows(
    tibble::tibble(type = "Imports", package = imports),
    tibble::tibble(type = "Suggests", package = suggests)
  ) %>%
    dplyr::left_join(current_dep, by = "package") %>%
    dplyr::mutate(version = dplyr::coalesce(version, "*"))

  other_dep <- desc$get_deps() %>%
    dplyr::anti_join(new_dep, by = "package") %>%
    dplyr::filter(!(type %in% c("Imports", "Suggests")))

  remote_pkgs <- fs::path_file(desc$get_remotes())

  must_dep <- desc$get_deps() %>%
    dplyr::filter(package %in% c(must_keep, remote_pkgs)) %>%
    dplyr::anti_join(other_dep, by = "package") %>%
    dplyr::anti_join(new_dep, by = "package")

  all_dep <- dplyr::bind_rows(new_dep, other_dep, must_dep) %>%
    dplyr::arrange(type, package)

  desc$set_deps(all_dep)
  desc$normalize()
  desc$write()
}

#' Rewrite DESCRIPTION file
#'
#' Wrapper to normalize the DESCRIPTION file in a package by invoking
#' the functions in \code{\link[desc]{description}}
#'
#' @param file Name of the description file
#' @param dir Root package directory
#' @export
desc_rewrite <- function(file = "DESCRIPTION", dir = ".") {
  path <- fs::path_abs(fs::path(dir, file))
  desc <- desc::description$new(path)
  desc$normalize()
  desc$write()
}


