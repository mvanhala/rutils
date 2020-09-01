# rutils

## Installation

```
devtools::install_github("mvanhala/rutils")
```

## Overview

This packages contains a collection of helpful utility functions.

Examples include:

* Rendering R Markdown documents in a clean [`callr`](https://github.com/r-lib/callr) session (`render_doc`).

* More flexible ways to save htmlwidgets (`save_widget`).

* Detecting dependencies in a packages and adding them to the DESCRIPTION
file (`desc_pkgs`).

* A wrapper for around functions in [`desc`](https://github.com/r-lib/desc) 
to standardize and rewrite the DESCRIPTION file of a package.

* A function for modifying the elements of a list by name (`list_edit`). This
function is inspired by `purrr::list_modify`, but only works on the top level of a list
rather than recursively.

