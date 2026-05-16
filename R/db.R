#' Register a parquet file as a DuckDB temporary view
#'
#' @param con A DBI connection.
#' @param table Name for the temporary DuckDB view.
#' @param path Path to a parquet file.
#'
#' @return The result of [DBI::dbExecute()], invisibly from DuckDB.
#' @noRd
register_parquet_view <- function(con, table, path) {
  table_sql <- DBI::dbQuoteIdentifier(con, table)
  path_sql <- DBI::dbQuoteString(con, normalizePath(path, mustWork = TRUE))

  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMP VIEW ",
      table_sql,
      " AS SELECT * FROM read_parquet(",
      path_sql,
      ")"
    )
  )
}

#' Create a DuckDB-backed MeSH database object
#'
#' `meshdb()` opens an in-memory DuckDB connection and registers temporary views
#' over the MeSH parquet files. The parquet files are queried lazily by DuckDB;
#' they are not read into R memory.
#'
#' @param path Directory containing the MeSH parquet files. Defaults to
#'   `mesh_data_path()`, which uses the `MESHDB_PARQUET_PATH` environment
#'   variable when set.
#'
#' @return A `meshdb` object containing the parquet `path`, file `tables`, a
#'   DuckDB `con`, and dbplyr table references in `src`.
#' @export
meshdb <- function(path = mesh_data_path()) {
  tables <- mesh_tables(path)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  purrr::iwalk(tables, \(file, table) register_parquet_view(con, table, file))

  src <- purrr::map(names(tables), \(table) dplyr::tbl(con, table))
  names(src) <- names(tables)

  structure(
    list(
      path = normalizePath(path, mustWork = TRUE),
      tables = tables,
      con = con,
      src = src
    ),
    class = "meshdb"
  )
}

#' Disconnect a MeSH DuckDB connection
#'
#' @param db A `meshdb` object returned by [meshdb()].
#' @param shutdown If `TRUE`, shut down the DuckDB driver.
#'
#' @return `NULL`, invisibly.
#' @export
mesh_disconnect <- function(db, shutdown = TRUE) {
  check_meshdb(db)

  if (DBI::dbIsValid(db$con)) {
    DBI::dbDisconnect(db$con, shutdown = shutdown)
  }

  invisible(NULL)
}

#' Validate a meshdb object
#'
#' @param db Object to validate.
#'
#' @return `db`, invisibly.
#' @noRd
check_meshdb <- function(db) {
  if (!inherits(db, "meshdb")) {
    rlang::abort("`db` must be a meshdb object created by `meshdb()`.")
  }

  if (!DBI::dbIsValid(db$con)) {
    rlang::abort("`db` contains a closed DuckDB connection.")
  }

  invisible(db)
}

#' Access a registered meshdb table
#'
#' @param db A `meshdb` object returned by [meshdb()].
#' @param table Name of a registered table.
#'
#' @return A lazy dbplyr table.
#' @noRd
mesh_tbl <- function(db, table) {
  check_meshdb(db)
  tbl <- db$src[[rlang::arg_match0(table, names(db$src))]]
  dbplyr::remote_con(tbl)
  tbl
}

#' Copy resolved MeSH IDs to DuckDB
#'
#' @param db A `meshdb` object returned by [meshdb()].
#' @param resolved A data frame of resolved MeSH IDs.
#'
#' @return A lazy dbplyr table backed by a temporary DuckDB table.
#' @noRd
copy_resolved_ids <- function(db, resolved) {
  check_meshdb(db)

  name <- paste0(
    "meshdb_resolved_",
    format(
      as.integer(stats::runif(1, 1, .Machine$integer.max)),
      scientific = FALSE
    )
  )

  dplyr::copy_to(
    db$con,
    resolved,
    name = name,
    temporary = TRUE,
    overwrite = TRUE
  )
}
