test_that("meshdb creates DuckDB-backed table references", {
  db <- local_meshdb()

  expect_s3_class(db, "meshdb")
  expect_true(DBI::dbIsValid(db$con))
  expect_named(db$src, c("meshdb", "parent", "ancestor", "human", "mouse"))
  expect_s3_class(db$src$meshdb, "tbl_lazy")
})

test_that("meshdb formats a compact summary", {
  db <- local_meshdb()

  output <- format(db)

  expect_equal(output[[1]], "<meshdb>")
  expect_equal(output[[2]], "Status: connected")
  expect_match(output[[3]], "^Path: .+")
  expect_equal(output[[4]], "Views: meshdb, parent, ancestor, human, mouse")
  expect_equal(output[[5]], "Terms by category:")
  expect_equal(output[[6]], "  category_code  category  terms")
  expect_equal(output[[7]], "  A              Anatomy       5")

  path <- local_mesh_path()
  db <- meshdb(path)
  mesh_disconnect(db)
  output <- format(db)

  expect_equal(output[[2]], "Status: disconnected")
  expect_length(output, 4)
})

test_that("meshdb print writes the formatted summary", {
  db <- local_meshdb()

  expect_equal(capture.output(print(db)), format(db))
})
