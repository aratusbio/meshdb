annotate_terms <- function(data, id_column, prefix, db) {
  terms <- term_summary(db) |>
    dplyr::rename_with(
      ~ paste0(prefix, c("_id", "_term", "_category_code", "_category")),
      dplyr::everything()
    )

  if (!inherits(data, "tbl_sql")) {
    terms <- dplyr::collect(terms)
  }

  dplyr::left_join(data, terms, by = stats::setNames(paste0(prefix, "_id"), id_column))
}

ancestor_distances <- function(db, inputs) {
  inputs <- copy_resolved_ids(db, inputs)
  input_sql <- DBI::dbQuoteIdentifier(db$con, dbplyr::remote_name(inputs))

  query <- paste0(
    "WITH RECURSIVE lineage(input, offspring_id, ancestor_id, distance) AS (",
    "  SELECT input.input, input.mesh_id, parent.parent_id, 1",
    "  FROM ", input_sql, " AS input",
    "  INNER JOIN parent ON input.mesh_id = parent.child_id",
    "  UNION ALL",
    "  SELECT lineage.input, lineage.offspring_id, parent.parent_id, lineage.distance + 1",
    "  FROM lineage",
    "  INNER JOIN parent ON lineage.ancestor_id = parent.child_id",
    "  WHERE lineage.distance < 100",
    ")",
    " SELECT lineage.input, lineage.offspring_id, lineage.ancestor_id,",
    "        ancestor.category_code, ancestor.category,",
    "        MIN(lineage.distance) AS distance",
    " FROM lineage",
    " INNER JOIN ancestor",
    "    ON lineage.offspring_id = ancestor.offspring_id",
    "   AND lineage.ancestor_id = ancestor.ancestor_id",
    " GROUP BY lineage.input, lineage.offspring_id, lineage.ancestor_id,",
    "          ancestor.category_code, ancestor.category"
  )

  DBI::dbGetQuery(db$con, query) |>
    tibble::as_tibble() |>
    dplyr::mutate(distance = as.integer(.data$distance))
}

child_distances <- function(db, inputs) {
  inputs <- copy_resolved_ids(db, inputs)
  input_sql <- DBI::dbQuoteIdentifier(db$con, dbplyr::remote_name(inputs))

  query <- paste0(
    "WITH RECURSIVE descendants(input, parent_id, child_id, distance) AS (",
    "  SELECT input.input, input.mesh_id, parent.child_id, 1",
    "  FROM ", input_sql, " AS input",
    "  INNER JOIN parent ON input.mesh_id = parent.parent_id",
    "  UNION ALL",
    "  SELECT descendants.input, descendants.parent_id, parent.child_id, descendants.distance + 1",
    "  FROM descendants",
    "  INNER JOIN parent ON descendants.child_id = parent.parent_id",
    "  WHERE descendants.distance < 100",
    ")",
    " SELECT descendants.input, descendants.parent_id, descendants.child_id,",
    "        ancestor.category_code, ancestor.category,",
    "        MIN(descendants.distance) AS distance",
    " FROM descendants",
    " INNER JOIN ancestor",
    "    ON descendants.parent_id = ancestor.ancestor_id",
    "   AND descendants.child_id = ancestor.offspring_id",
    " GROUP BY descendants.input, descendants.parent_id, descendants.child_id,",
    "          ancestor.category_code, ancestor.category"
  )

  DBI::dbGetQuery(db$con, query) |>
    tibble::as_tibble() |>
    dplyr::mutate(distance = as.integer(.data$distance))
}

#' Find direct parent terms
#'
#' @param mesh Character vector of MeSH IDs or exact MeSH term labels.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble with one row per input term and direct parent.
#' @export
mesh_parents <- function(mesh, db = meshdb()) {
  resolved <- resolve_mesh_ids(mesh, db)
  resolved <- copy_resolved_ids(db, resolved)
  parents <- mesh_tbl(db, "parent")

  resolved |>
    dplyr::inner_join(parents, by = c("mesh_id" = "child_id")) |>
    dplyr::rename(parent_id = "parent_id") |>
    annotate_terms("parent_id", "parent", db) |>
    dplyr::rename(child_id = "mesh_id") |>
    dplyr::select(dplyr::any_of(c(
      "input",
      "child_id",
      "parent_id",
      "parent_term",
      "parent_category_code",
      "parent_category",
      "category_code",
      "category"
    ))) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    # dplyr::rename(
    #   query_id = offspring_id,
    #   mesh_id = ancestor_id
    # ) |>
    identity()
}

#' Find child terms
#'
#' @param mesh Character vector of MeSH IDs or exact MeSH term labels.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble with one row per input term and child. The `distance`
#'   column gives the number of direct-child hops from the input term to the
#'   child.
#' @export
mesh_children <- function(mesh, db = meshdb()) {
  resolved <- resolve_mesh_ids(mesh, db)
  distances <- child_distances(db, resolved)

  distances |>
    annotate_terms("child_id", "child", db) |>
    dplyr::select(dplyr::any_of(c(
      "input",
      "parent_id",
      "child_id",
      "child_term",
      "child_category_code",
      "child_category",
      "category_code",
      "category",
      "distance"
    ))) |>
    dplyr::distinct() |>
    tibble::as_tibble() |>
    dplyr::arrange(.data$distance, .data$child_term) |>
    # dplyr::rename(
    #   query_id = parent_id,
    #   mesh_id = child_id,
    #   mesh_term = child_term
    # ) |>
    identity()
}

#' Find ancestor terms
#'
#' @param mesh Character vector of MeSH IDs or exact MeSH term labels.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble with one row per input term and ancestor. The `distance`
#'   column gives the number of direct-parent hops from the input term to the
#'   ancestor.
#' @export
mesh_ancestors <- function(mesh, db = meshdb()) {
  resolved <- resolve_mesh_ids(mesh, db)
  distances <- ancestor_distances(db, resolved)

  distances |>
    annotate_terms("ancestor_id", "ancestor", db) |>
    dplyr::select(dplyr::any_of(c(
      "input",
      "offspring_id",
      "ancestor_id",
      "ancestor_term",
      "ancestor_category_code",
      "ancestor_category",
      "category_code",
      "category",
      "distance"
    ))) |>
    dplyr::distinct() |>
    tibble::as_tibble() |>
    dplyr::arrange(.data$distance, .data$ancestor_term)
}

#' Test whether MeSH IDs descend from a parent term
#'
#' @param mesh Character vector of MeSH IDs.
#' @param parent A single MeSH ID or exact MeSH term label to test as the
#'   ancestor.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble with one row per `mesh` input and columns `is_child` and
#'   `distance`. `distance` is `NA_integer_` when `is_child` is `FALSE`.
#' @export
mesh_is_child <- function(mesh, parent, db = meshdb()) {
  if (length(parent) != 1) {
    rlang::abort("`parent` must be a single MeSH ID or exact MeSH term label.")
  }

  check_meshdb(db)
  mesh <- as.character(mesh)

  if (length(mesh) == 0) {
    return(tibble::tibble(is_child = logical(), distance = integer()))
  }

  parent_resolved <- resolve_mesh_ids(parent, db) |>
    dplyr::distinct(.data$mesh_id)

  if (nrow(parent_resolved) == 0) {
    rlang::abort("`parent` must resolve to a MeSH term.")
  }

  if (nrow(parent_resolved) > 1) {
    rlang::abort("`parent` must resolve to a single MeSH term.")
  }

  inputs <- tibble::tibble(input = mesh, mesh_id = mesh)
  distances <- ancestor_distances(db, inputs) |>
    dplyr::filter(.data$ancestor_id == parent_resolved$mesh_id[[1]]) |>
    dplyr::group_by(.data$offspring_id) |>
    dplyr::summarise(distance = min(.data$distance), .groups = "drop")

  tibble::tibble(mesh_id = mesh) |>
    dplyr::left_join(distances, by = c("mesh_id" = "offspring_id")) |>
    dplyr::mutate(is_child = !is.na(.data$distance)) |>
    dplyr::select("is_child", "distance")
}

#' Find shared parent or ancestor terms
#'
#' Given a vector of MeSH terms, identify parent terms shared by every input.
#' Use `relationship = "ancestor"` to include all ancestors or `"parent"` to
#' restrict the search to direct parents only.
#'
#' @param mesh Character vector of MeSH IDs or exact MeSH term labels.
#' @param relationship Either `"ancestor"` or `"parent"`.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble of shared terms, ordered by category and term.
#' @export
mesh_common_parents <- function(
    mesh,
    relationship = c("ancestor", "parent"),
    db = meshdb()) {
  relationship <- rlang::arg_match(relationship)
  resolved <- resolve_mesh_ids(mesh, db)
  n_inputs <- dplyr::n_distinct(resolved$mesh_id)

  if (n_inputs == 0) {
    return(tibble::tibble())
  }

  relationships <- switch(
    relationship,
    ancestor = mesh_ancestors(resolved$mesh_id, db) |>
      dplyr::transmute(
        input = .data$input,
        child_id = .data$offspring_id,
        common_parent_id = .data$ancestor_id,
        relationship = "ancestor"
      ),
    parent = mesh_parents(resolved$mesh_id, db) |>
      dplyr::transmute(
        input = .data$input,
        child_id = .data$child_id,
        common_parent_id = .data$parent_id,
        relationship = "parent"
      )
  )

  common <- relationships |>
    dplyr::distinct(.data$child_id, .data$common_parent_id, .data$relationship) |>
    dplyr::group_by(.data$common_parent_id, .data$relationship) |>
    dplyr::summarise(n_terms = dplyr::n_distinct(.data$child_id), .groups = "drop") |>
    dplyr::filter(.data$n_terms == n_inputs)

  terms <- term_summary(db) |>
    dplyr::rename(
      common_parent_id = "mesh_id",
      common_parent_term = "mesh_term",
      common_parent_category_code = "category_code",
      common_parent_category = "category"
    )

  terms |>
    dplyr::collect() |>
    dplyr::inner_join(common, by = "common_parent_id") |>
    dplyr::arrange(.data$common_parent_category_code, .data$common_parent_term) |>
    dplyr::distinct()
}
