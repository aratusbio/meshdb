test_that("meshdb creates DuckDB-backed table references", {
  db <- local_meshdb()

  expect_s3_class(db, "meshdb")
  expect_true(DBI::dbIsValid(db$con))
  expect_named(db$src, c("meshdb", "parent", "ancestor", "human", "mouse"))
  expect_s3_class(db$src$meshdb, "tbl_lazy")
})
