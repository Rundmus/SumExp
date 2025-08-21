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

test_that("methods for ListMatrix", {
  l <- ListMatrix(a = m1, b = m2)
  expect_s4_class(
    replace_if_true(l, l[["a"]] > 10, NA),
    "ListMatrix"
  )
  # t() : transpose
  expect_equal(rownames(t(l)), colnames(l))
  expect_equal(colnames(t(l)), rownames(l))
  expect_equal(l[["b"]], t(t(l[["b"]])))
})

test_that("`colnames<-` & `rownames<-` works", {
  l <- ListMatrix(a = m1, b = m2)
  colnames(l) <- c("aa", "bb", "cc", "dd", "ee")
  expect_equal(colnames(l), c("aa", "bb", "cc", "dd", "ee"))
  expect_equal(names(l), c("a", "b"))
  rownames(l) <- c("alpha", "beta", "gamma", "delta")
  expect_equal(rownames(l), c("alpha", "beta", "gamma", "delta"))
  expect_equal(names(l), c("a", "b"))
  expect_s4_class(l, "ListMatrix")
})
