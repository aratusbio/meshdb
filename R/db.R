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

#' Format a meshdb object
#'
#' @param x A `meshdb` object.
#' @param ... Unused.
#'
#' @return A character vector containing a compact summary of `x`.
#' @export
#' @noRd
format.meshdb <- function(x, ...) {
  connected <- DBI::dbIsValid(x$con)
  status <- if (connected) "connected" else "disconnected"

  output <- c(
    "<meshdb>",
    paste0("Status: ", status),
    paste0("Path: ", x$path),
    paste0("Views: ", paste(names(x$src), collapse = ", "))
  )

  if (connected) {
    category_counts <- x$src$meshdb |>
      dplyr::group_by(.data$category_code, .data$category) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(.data$category_code, .data$category) |>
      dplyr::collect()

    output <- c(
      output,
      "Terms by category:",
      format_category_counts(category_counts)
    )
  }

  output
}

#' Format mesh category counts as a compact ASCII table
#'
#' @param category_counts A data frame with `category_code`, `category`, and
#'   `n` columns.
#'
#' @return A character vector containing one table row per element.
#' @noRd
format_category_counts <- function(category_counts) {
  columns <- list(
    category_code = as.character(category_counts$category_code),
    category = as.character(category_counts$category),
    terms = format(category_counts$n, big.mark = ",", scientific = FALSE)
  )

  headers <- names(columns)
  widths <- purrr::map2_int(
    headers,
    columns,
    \(header, column) max(nchar(c(header, column), type = "width"))
  )

  header <- purrr::map2_chr(
    headers,
    widths,
    \(header, width) pillar::align(header, width, align = "left")
  )
  header <- paste(header, collapse = "  ")
  alignments <- c("left", "left", "right")
  rows <- purrr::pmap_chr(
    columns,
    \(category_code, category, terms) {
      values <- c(category_code, category, terms)
      aligned <- purrr::pmap_chr(
        list(values, widths, alignments),
        \(value, width, alignment) {
          pillar::align(value, width, align = alignment)
        }
      )
      paste(aligned, collapse = "  ")
    }
  )

  paste0("  ", c(header, rows))
}

#' Print a meshdb object
#'
#' @param x A `meshdb` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
#' @noRd
print.meshdb <- function(x, ...) {
  cat(format(x), sep = "\n")

  invisible(x)
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
