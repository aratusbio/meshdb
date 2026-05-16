#' Get the default MeSH parquet path
#'
#' @return The value of `MESHDB_PARQUET_PATH`, or the package fallback path.
#' @noRd
default_mesh_path <- function() {
  Sys.getenv(
    "MESHDB_PARQUET_PATH",
    unset = "/Users/steve/workspace/data/MeSHDb/v010/parquet"
  )
}

#' Get or set the MeSH parquet directory
#'
#' `mesh_data_path()` returns the directory used by default when reading MeSH
#' parquet files. Set the `MESHDB_PARQUET_PATH` environment variable or call
#' `mesh_data_path(path)` to use a different location.
#'
#' @param path Optional path to a directory containing the MeSH parquet files.
#'
#' @return A normalized directory path.
#' @export
mesh_data_path <- function(path = NULL) {
  if (!is.null(path)) {
    if (length(path) != 1) {
      rlang::abort("`path` must be a single directory path.")
    }
    options(meshdb.parquet_path = normalizePath(path, mustWork = FALSE))
  }

  getOption("meshdb.parquet_path", default_mesh_path())
}

#' List MeSH parquet table paths
#'
#' @param path Directory containing the MeSH parquet files.
#'
#' @return A named character vector of parquet paths.
#' @export
mesh_tables <- function(path = mesh_data_path()) {
  path <- normalizePath(path, mustWork = FALSE)

  tables <- c(
    meshdb = "meshdb.parquet",
    parent = "parent.parquet",
    ancestor = "ancestor.parquet",
    human = "meshmap-human.parquet",
    mouse = "meshmap-mouse.parquet"
  )

  paths <- stats::setNames(file.path(path, tables), names(tables))
  missing <- names(paths)[!file.exists(paths)]

  if (length(missing) > 0) {
    rlang::abort(
      c(
        "Missing MeSH parquet file(s).",
        x = paste0(missing, collapse = ", "),
        i = paste0("Checked directory: ", path)
      )
    )
  }

  paths
}

#' Get a single MeSH parquet table path
#'
#' @param table Name of a required MeSH parquet table.
#' @param path Directory containing the MeSH parquet files.
#'
#' @return Path to the requested parquet file.
#' @noRd
table_path <- function(table, path = mesh_data_path()) {
  tables <- mesh_tables(path)
  tables[[rlang::arg_match0(table, names(tables))]]
}
