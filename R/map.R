#' Query PubMed-to-MeSH mappings
#'
#' @param species Either `"human"` or `"mouse"`.
#' @param pmid Optional PubMed IDs.
#' @param mesh Optional MeSH IDs or exact MeSH term labels.
#' @param gene Optional gene symbols.
#' @param db A `meshdb` object returned by [meshdb()].
#'
#' @return A tibble of matching PubMed-to-MeSH records.
#' @export
mesh_map <- function(
    species = c("human", "mouse"),
    pmid = NULL,
    mesh = NULL,
    gene = NULL,
    db = meshdb()) {
  species <- rlang::arg_match(species)
  data <- mesh_tbl(db, species)

  if (!is.null(pmid)) {
    data <- data |>
      dplyr::filter(.data$pmid %in% pmid)
  }

  if (!is.null(mesh)) {
    resolved <- resolve_mesh_ids(mesh, db)
    data <- data |>
      dplyr::filter(.data$mesh_id %in% resolved$mesh_id)
  }

  if (!is.null(gene)) {
    data <- data |>
      dplyr::filter(.data$gene %in% gene)
  }

  dplyr::collect(data)
}
