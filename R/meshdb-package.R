#' meshdb: Navigate a Local MeSH Ontology Database
#'
#' Tools for querying MeSH terms and traversing parent-child and ancestor
#' relationships stored in local parquet files.
#'
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  warn_invalid_meshdb_parquet_path()
}

warn_invalid_meshdb_parquet_path <- function() {
  path <- Sys.getenv("MESHDB_PARQUET_PATH", unset = NA_character_)

  if (is.na(path) || !nzchar(path)) {
    rlang::warn(
      c(
        "`MESHDB_PARQUET_PATH` is not set.",
        i = "Set it to a local MeSH parquet directory, or pass `path` to `meshdb()`."
      ),
      class = "meshdb_missing_parquet_path"
    )
    return(invisible(FALSE))
  }

  if (!dir.exists(path)) {
    rlang::warn(
      c(
        "`MESHDB_PARQUET_PATH` does not point to a valid local directory.",
        x = paste0("Current value: ", path),
        i = "Set it to a local MeSH parquet directory, or pass `path` to `meshdb()`."
      ),
      class = "meshdb_invalid_parquet_path"
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}
