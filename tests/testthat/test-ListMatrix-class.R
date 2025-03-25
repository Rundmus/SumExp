m1 <- matrix(sample(20), nrow = 4)
m2 <- matrix(sample(LETTERS, 20), nrow = 4)
rownames(m2) <- rownames(m1) <- LETTERS[1:4]
colnames(m2) <- colnames(m1) <- letters[1:5]

test_that("methods inherited from list", {
  expect_s4_class(
    l <- ListMatrix(a = m1, b = m2),
    "ListMatrix"
  )
  expect_equal(names(l), c("a", "b"))
  expect_equal(l[["a"]], m1)
  expect_equal(l$b, m2)
  l$a <- m2
  expect_equal(l$a, m2)
  m3 <- matrix(sample(LETTERS, 20), nrow = 4)
  dimnames(m3) <- list(LETTERS[1:4], letters[1:5])
  l[["a"]] <- m3
  expect_equal(l$a, m3)
  expect_equal(names(l), c("a", "b"))
})

l <- ListMatrix(a = m1, b = m2)

test_that("methods for ListMatrix", {
  expect_s4_class(
    replace_if_true(l, l[["a"]] > 10, NA),
    "ListMatrix"
  )
})
