local_mesh_path <- function() {
  path <- tempfile("meshdb-test-")
  dir.create(path)

  arrow::write_parquet(
    tibble::tribble(
      ~mesh_id, ~mesh_term, ~category_code, ~category, ~synonym, ~qualifier_id, ~qualifier_term,
      "D000001", "Root", "A", "Anatomy", NA_character_, NA_character_, NA_character_,
      "D000002", "Parent A", "A", "Anatomy", "Alpha", NA_character_, NA_character_,
      "D000003", "Parent B", "A", "Anatomy", NA_character_, NA_character_, NA_character_,
      "D000004", "Child A", "A", "Anatomy", NA_character_, NA_character_, NA_character_,
      "D000005", "Child B", "A", "Anatomy", NA_character_, NA_character_, NA_character_
    ),
    file.path(path, "meshdb.parquet")
  )

  arrow::write_parquet(
    tibble::tribble(
      ~parent_id, ~child_id, ~category_code, ~category,
      "D000001", "D000002", "A", "Anatomy",
      "D000001", "D000003", "A", "Anatomy",
      "D000002", "D000004", "A", "Anatomy",
      "D000002", "D000005", "A", "Anatomy",
      "D000003", "D000005", "A", "Anatomy"
    ),
    file.path(path, "parent.parquet")
  )

  arrow::write_parquet(
    tibble::tribble(
      ~ancestor_id, ~offspring_id, ~category_code, ~category,
      "D000001", "D000002", "A", "Anatomy",
      "D000001", "D000003", "A", "Anatomy",
      "D000001", "D000004", "A", "Anatomy",
      "D000002", "D000004", "A", "Anatomy",
      "D000001", "D000005", "A", "Anatomy",
      "D000002", "D000005", "A", "Anatomy",
      "D000003", "D000005", "A", "Anatomy"
    ),
    file.path(path, "ancestor.parquet")
  )

  map <- tibble::tribble(
    ~species, ~pmid, ~mesh_id, ~mesh_term, ~category_code, ~category, ~ensembl_id, ~entrez_id, ~gene, ~gene_biotype, ~sourcedb,
    "human", 1L, "D000004", "Child A", "A", "Anatomy", "ENSG1", 1L, "GENE1", "protein_coding", "test"
  )

  arrow::write_parquet(map, file.path(path, "meshmap-human.parquet"))
  arrow::write_parquet(dplyr::mutate(map, species = "mouse"), file.path(path, "meshmap-mouse.parquet"))

  path
}

local_meshdb <- function() {
  db <- meshdb(local_mesh_path())
  withr::defer(mesh_disconnect(db), testthat::teardown_env())
  db
}
