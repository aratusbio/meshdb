test_that("mesh_parents and mesh_ancestors traverse relationships", {
  db <- local_meshdb()

  parents <- mesh_parents("Child B", db = db)
  expect_setequal(parents$parent_id, c("D000002", "D000003"))

  children <- mesh_children("Root", db = db)
  expect_setequal(
    children$child_id,
    c("D000002", "D000003", "D000004", "D000005")
  )
  expect_equal(
    children$distance[match(
      c("D000002", "D000003", "D000004", "D000005"),
      children$child_id
    )],
    c(1L, 1L, 2L, 2L)
  )

  ancestors <- mesh_ancestors("D000005", db = db)
  expect_setequal(ancestors$ancestor_id, c("D000001", "D000002", "D000003"))
  expect_equal(
    ancestors$distance[match(
      c("D000001", "D000002", "D000003"),
      ancestors$ancestor_id
    )],
    c(2L, 1L, 1L)
  )
})

test_that("mesh_common_parents identifies shared direct and ancestor parents", {
  db <- local_meshdb()

  direct <- mesh_common_parents(
    c("Child A", "Child B"),
    relationship = "parent",
    db = db
  )
  expect_equal(direct$common_parent_id, "D000002")

  ancestors <- mesh_common_parents(
    c("Child A", "Child B"),
    relationship = "ancestor",
    db = db
  )
  expect_setequal(ancestors$common_parent_id, c("D000001", "D000002"))
})

test_that("mesh_is_child identifies descendants and distance to parent", {
  db <- local_meshdb()

  out <- mesh_is_child(
    c("D000004", "D000005", "D000001", "D999999"),
    "Parent A",
    db = db
  )

  expect_named(out, c("is_child", "distance"))
  expect_equal(out$is_child, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(out$distance, c(1L, 1L, NA_integer_, NA_integer_))

  root <- mesh_is_child(c("D000004", "D000005"), "D000001", db = db)
  expect_equal(root$is_child, c(TRUE, TRUE))
  expect_equal(root$distance, c(2L, 2L))
})
