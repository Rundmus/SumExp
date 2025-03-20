## code to prepare `test_se` dataset goes here

# Prep matrices
m1 <- matrix(sample(30), nrow = 5)
L <- sample(LETTERS, 30, replace = TRUE)
l <- sample(letters, 30, replace = TRUE)
m2 <- matrix(paste0(L, l), nrow = 5)
rownames(m2) <- rownames(m1) <- LETTERS[1:5]
colnames(m2) <- colnames(m1) <- 1:6
labelled::label_attribute(m1) <- "Matrix A"

# Prep data frames
df_c <- data.frame(
  x = c("alpha", "beta", "gamma", "delta", "epsilon", "zeta"),
  type = gl(3, 2, labels = c("", "fruit", "veg")) |>
    labelled::set_label_attribute("Type of product"),
  row.names = colnames(m1)
)
df_r <- data.frame(
  y = round(rnorm(5, mean = 10, sd = 5)) |>
    as.integer() |>
    labelled::set_label_attribute("Days"),
  color = rep(c("White", "Black"), each = 3)[-1],
  row.names = rownames(m1)
)
exmpl_se <- SumExp(a = m1, mat2 = m2, row_df = df_r, col_df = df_c)


usethis::use_data(exmpl_se, overwrite = TRUE)
