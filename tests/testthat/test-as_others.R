m1 <- matrix(sample(20), nrow = 4)
m2 <- matrix(sample(LETTERS, 20), nrow = 4)
rownames(m2) <- rownames(m1) <- LETTERS[1:4]
colnames(m2) <- colnames(m1) <- letters[1:5]
df_c <- data.frame(
  x = c("alpha", "beta", "gamma", "delta", "epsilon"),
  type = c("", "", "fruit", "fruit", "fruit"),
  row.names = letters[1:5]
)
df_r <- data.frame(
  y = rnorm(4),
  grp = rep(c("White", "Black"), each = 2),
  row.names = LETTERS[1:4]
)
se <- SumExp(a = m1, b = m2, row_df = df_r, col_df = df_c)

test_that("as_tibble works", {
  se_tbl <- as_tibble(se)
  expect_s3_class(se_tbl, "tbl_df")
  expect_equal(nrow(se_tbl), 20)
  expect_equal(ncol(se_tbl), 8)
})

test_that("as_SummarizedExperiment works", {
  se_se <- as_SummarizedExperiment(se)
  expect_s4_class(se_se, "SummarizedExperiment")
  expect_equal(nrow(se_se), nrow(se))
  expect_equal(ncol(se_se), ncol(se))
  expect_equal(as.data.frame(SummarizedExperiment::rowData(se_se)), row_df(se))
  expect_equal(as.data.frame(SummarizedExperiment::colData(se_se)), col_df(se))
  expect_equal(SummarizedExperiment::assays(se_se)$b, se[["b"]])
})
