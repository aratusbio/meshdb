test_that("mesh_lookup finds terms by id, label, and synonym", {
  db <- local_meshdb()

  by_id <- mesh_lookup("D000004", db = db)
  expect_equal(by_id$mesh_term, "Child A")

  by_term <- mesh_lookup("parent a", by = "mesh_term", db = db)
  expect_equal(by_term$mesh_id, "D000002")

  by_synonym <- mesh_lookup("Alpha", by = "synonym", db = db)
  expect_equal(by_synonym$mesh_term, "Parent A")
})
