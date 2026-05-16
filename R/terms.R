term_columns <- function(data) {
  intersect(
    c(
      "mesh_id",
      "mesh_term",
      "category_code",
      "category",
      "synonym",
      "qualifier_id",
      "qualifier_term"
    ),
    colnames(data)
  )
}

term_summary <- function(db) {
  mesh_tbl(db, "meshdb") |>
    dplyr::select(dplyr::any_of(c("mesh_id", "mesh_term", "category_code", "category"))) |>
    dplyr::distinct()
}

standardize_values <- function(x, ignore_case = TRUE) {
  x <- as.character(x)

  if (ignore_case) {
    stringr::str_to_lower(x)
  } else {
    x
  }
}

filter_character <- function(data, column, values, exact = TRUE, ignore_case = TRUE) {
  if (is.null(values)) {
    return(data)
  }

  values <- as.character(values)
  column <- rlang::sym(column)

  if (ignore_case) {
    values <- stringr::str_to_lower(values)
  }

  if (exact && ignore_case) {
    dplyr::filter(data, stringr::str_to_lower(!!column) %in% values)
  } else if (exact) {
    dplyr::filter(data, !!column %in% values)
  } else if (ignore_case) {
    conditions <- purrr::map(
      values,
      \(value) rlang::expr(stringr::str_detect(stringr::str_to_lower(!!column), !!value))
    )
    dplyr::filter(data, !!purrr::reduce(conditions, \(x, y) rlang::expr(!!x | !!y)))
  } else {
    conditions <- purrr::map(
      values,
      \(value) rlang::expr(stringr::str_detect(!!column, !!value))
    )
    dplyr::filter(data, !!purrr::reduce(conditions, \(x, y) rlang::expr(!!x | !!y)))
  }
}

resolve_mesh_ids <- function(mesh, db = meshdb()) {
  mesh <- unique(as.character(mesh))

  if (length(mesh) == 0) {
    return(tibble::tibble(input = character(), mesh_id = character()))
  }

  terms <- term_summary(db)
  exact_ids <- terms |>
    dplyr::filter(.data$mesh_id %in% mesh) |>
    dplyr::transmute(input = .data$mesh_id, mesh_id = .data$mesh_id) |>
    dplyr::collect()

  unmatched <- setdiff(mesh, exact_ids$input)
  term_matches <- if (length(unmatched) > 0) {
    lower_unmatched <- stringr::str_to_lower(unmatched)

    terms |>
      dplyr::filter(stringr::str_to_lower(.data$mesh_term) %in% lower_unmatched) |>
      dplyr::select("mesh_term", "mesh_id") |>
      dplyr::collect() |>
      dplyr::mutate(input = mesh[match(stringr::str_to_lower(.data$mesh_term), stringr::str_to_lower(mesh))]) |>
      dplyr::select("input", "mesh_id")
  } else {
    tibble::tibble(input = character(), mesh_id = character())
  }

  resolved <- dplyr::bind_rows(exact_ids, term_matches) |>
    dplyr::distinct()

  missing <- setdiff(mesh, resolved$input)
  if (length(missing) > 0) {
    rlang::warn(
      paste0("No MeSH term found for: ", paste(missing, collapse = ", "))
    )
  }

  resolved
}

#' Look up MeSH terms
#'
#' Search the local MeSH term table by ID, term label, synonym, or qualifier.
#'
#' @param x Character vector of IDs or labels to search for.
#' @param by Search column. `"auto"` matches `mesh_id`, `mesh_term`, `synonym`,
#'   and `qualifier_term`.
#' @param exact If `TRUE`, require exact matches. If `FALSE`, use substring
#'   matching.
#' @param ignore_case If `TRUE`, ignore case when matching text columns.
#' @param include_qualifiers If `FALSE`, return one row per distinct MeSH term
#'   and category. If `TRUE`, include synonym and qualifier columns from the
#'   source table.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble of matching MeSH records.
#' @export
mesh_lookup <- function(
    x,
    by = c("auto", "mesh_id", "mesh_term", "synonym", "qualifier_term"),
    exact = TRUE,
    ignore_case = TRUE,
    include_qualifiers = FALSE,
    db = meshdb()) {
  by <- rlang::arg_match(by)
  data <- mesh_tbl(db, "meshdb")

  if (by == "auto") {
    columns <- intersect(c("mesh_id", "mesh_term", "synonym", "qualifier_term"), colnames(data))
    matches <- purrr::map(
      columns,
      \(column) {
        filter_character(data, column, x, exact = exact, ignore_case = ignore_case) |>
          dplyr::collect()
      }
    )

    out <- dplyr::bind_rows(matches)
  } else {
    out <- filter_character(data, by, x, exact = exact, ignore_case = ignore_case)
  }

  out <- out |>
    dplyr::select(dplyr::any_of(term_columns(out))) |>
    dplyr::distinct() |>
    dplyr::collect()

  if (!include_qualifiers) {
    out <- out |>
      dplyr::select(dplyr::any_of(c("mesh_id", "mesh_term", "category_code", "category"))) |>
      dplyr::distinct()
  }

  tibble::as_tibble(out)
}

#' Construct a URL for a mesh term
#'
#' @export
#' @param x the mesh term (or vector of them)
mesh_url <- function(x, term = NULL, ahref = FALSE, target = NULL, ...) {
  checkmate::assert_flag(ahref)
  if (checkmate::test_data_frame(x)) {
    checkmate::assert_subset(c("term_id", "term"), colnames(x))
    term <- x[["term"]]
    term_id <- x[["term_id"]]
  } else {
    term_id <- x
  }
  term_id <- checkmate::assert_character(
    term_id,
    pattern = "[DQ]\\d+$",
    min.len = 1,
    min.chars = 5
  )

  out <- sprintf("https://www.ncbi.nlm.nih.gov/mesh/?term=%s", term_id)

  if (ahref) {
    if (checkmate::test_character(term, len = length(out))) {
      out <- sprintf("<a href='%s':::target:::>%s</a> [%s]", out, term, term_id)
    } else {
      out <- sprintf("<a href='%s':::target:::>%s</a>", out, x)
    }
  }

  if (checkmate::test_string(target, min.chars = 1)) {
    target <- sprintf(" target='%s'", target)
  } else {
    target <- ""
  }
  out <- sub(":::target:::", target, out)
  out
}
