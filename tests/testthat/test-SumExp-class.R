m1 <- matrix(sample(20), nrow = 4)
m2 <- matrix(sample(LETTERS, 20), nrow = 4)
rownames(m2) <- rownames(m1) <- LETTERS[1:4]
colnames(m2) <- colnames(m1) <- letters[1:5]
df_c <- data.frame(
  x = c("alpha", "beta", "gamma", "delta", "epsilon"),
  row.names = letters[1:5]
)
df_r <- data.frame(
  y = rnorm(4),
  row.names = LETTERS[1:4]
)

test_that("ListMatrix functions inherited from list", {
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


expect_s4_class(
  se <- SumExp(a = m1, b = m2, row_df = df_r, col_df = df_c),
  "SumExp"
)
test_that("ListMatrix validation works with SumExp obj", {
  # No rows left
  expect_s4_class(se[rep(FALSE, nrow(se)), ], "SumExp")

  expect_error(
    SumExp(a = m1, b = t(m2), row_df = df_r, col_df = df_c),
    "Dimensions of all matrices must be equal"
  )
  m3 <- matrix(sample(LETTERS, 20), nrow = 4)
  rownames(m3) <- LETTERS[1:4]
  expect_error(
    SumExp(a = m1, b = m3, row_df = df_r, col_df = df_c),
    "Column names in all matrices must be defined"
  )
  m3 <- matrix(sample(LETTERS, 20), nrow = 4)
  colnames(m3) <- letters[1:5]
  expect_error(
    SumExp(a = m1, b = m3, row_df = df_r, col_df = df_c),
    "Row names in all matrices must be defined"
  )
  m3 <- m2
  rownames(m3)[3] <- "Wrong"
  expect_error(
    SumExp(a = m1, b = m3, row_df = df_r, col_df = df_c),
    "Row names in all matrices must be equal"
  )
  m3 <- m2
  colnames(m3)[3] <- "Wrong"
  expect_error(
    SumExp(a = m1, b = m3, row_df = df_r, col_df = df_c),
    "Column names in all matrices must be equal"
  )
})

test_that("SumExp validation works", {
  expect_error(
    SumExp(m1, m2, row_df = df_r, col_df = df_c),
    "Names of matrices must be defined"
  )
})

test_that("ListMatrix methods with SumExp obj", {
  expect_equal(names(se), c("a", "b"))
  expect_equal(se[["a"]], m1)
  expect_equal(se[["b"]], m2)
  m3 <- matrix(sample(300, 20), nrow = 4)
  dimnames(m3) <- dimnames(m2)
  expect_s4_class({se[["b"]] <- m3; se}, "SumExp")
})

test_that("as_tibble works", {
  se_tbl <- as_tibble(se)
  expect_s3_class(se_tbl, "tbl_df")
  expect_equal(nrow(se_tbl), 20)
  expect_equal(ncol(se_tbl), 6)
})

test_that("labelled works", {
  m2 <- labelled::set_label_attribute(m2, "matrix_b")
  se <- SumExp(a = m1, b = m2, row_df = df_r, col_df = df_c)
  expect_s4_class(se, "SumExp")
  expect_equal(labelled::get_label_attribute(se[["b"]]), "matrix_b")
  se_tbl <- as_tibble(se)
  expect_equal(labelled::get_label_attribute(se_tbl[["b"]]), "matrix_b")
})

test_that("SumExp subset works", {
  expect_s4_class(se[1:2, ], "SumExp")
  expect_s4_class(se[, 3:5], "SumExp")
  se_sub <- se[1:2, 3:5]
  expect_s4_class(se_sub, "SumExp")
  expect_equal(se_sub[["a"]], m1[1:2, 3:5])
  expect_equal(se_sub[["b"]], m2[1:2, 3:5])
  expect_equal(se_sub@row_df, df_r[1:2, , drop = FALSE])
  expect_equal(se_sub@col_df, df_c[3:5, , drop = FALSE])
})


