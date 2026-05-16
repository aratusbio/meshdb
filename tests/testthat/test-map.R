test_that("mesh_map filters pubmed mappings", {
  db <- local_meshdb()

  out <- mesh_map("human", mesh = "Child A", gene = "GENE1", db = db)

  expect_equal(nrow(out), 1)
  expect_equal(out$pmid, 1)
})
