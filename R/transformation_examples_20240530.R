# Simulating Skewed Data
# Gabriel Odom
# 2024-05-30

baseX_nrml <- rnorm(200)
baseX_count <- rpois(200, lambda = 50)

# Log transform
par(mfrow = c(1, 2))
hist(
  exp(baseX_nrml),
  main = "Histogram of Skewed Data",
  xlab = "Original X"
)
hist(
  log(exp(baseX_nrml)),
  main = "Histogram after Natural Log Trans",
  xlab = "ln(X)"
)
par(mfrow = c(1, 1))


# Square root transform
par(mfrow = c(1, 2))
hist(
  (baseX_nrml + 3)^2,
  main = "Histogram of Skewed Data",
  xlab = "Original X"
)
hist(
  sqrt((baseX_nrml + 3)^2),
  main = "Histogram after Sq Root Trans",
  xlab = "sqrt(X)"
)
par(mfrow = c(1, 1))


# Geometric reciprocal transform
par(mfrow = c(1, 2))
hist(
  1 / baseX_count,
  main = "Histogram of Skewed Data",
  xlab = "Original X"
)
hist(
  baseX_count,
  main = "Histogram after Geometric Reciprocal",
  xlab = "1/X"
)
par(mfrow = c(1, 1))
