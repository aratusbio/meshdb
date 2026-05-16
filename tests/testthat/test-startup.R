test_that("package load warns when MESHDB_PARQUET_PATH is unset", {
  withr::local_envvar(MESHDB_PARQUET_PATH = NA_character_)

  expect_warning(
    meshdb:::warn_invalid_meshdb_parquet_path(),
    class = "meshdb_missing_parquet_path"
  )
})

test_that("package load warns when MESHDB_PARQUET_PATH is not a directory", {
  withr::local_envvar(MESHDB_PARQUET_PATH = tempfile("missing-meshdb-"))

  expect_warning(
    meshdb:::warn_invalid_meshdb_parquet_path(),
    class = "meshdb_invalid_parquet_path"
  )
})

test_that("package load accepts an existing MESHDB_PARQUET_PATH directory", {
  withr::local_envvar(MESHDB_PARQUET_PATH = tempdir())

  expect_no_warning(meshdb:::warn_invalid_meshdb_parquet_path())
})
