
probs <- runif(10)
truth_lgl <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
plot(x = truth_lgl, y = probs)

Accuracy <- function(target_lgl, truth_lgl) {
  TP <- sum(target_lgl & truth_lgl)
  TN <- sum(!target_lgl & !truth_lgl)
  FP <- sum(target_lgl & !truth_lgl)
  FN <- sum(!target_lgl & truth_lgl)
  
  (TP + TN) / (TP + TN + FP + FN)
}

# Test
Accuracy(
  target_lgl = probs > 0.603,
  truth_lgl = truth_lgl
)

# For all thresholds
ROC_fun <- function(threshold_vec) {
  nThreshs <- length(threshold_vec)
  accur_num <- numeric(length = nThreshs)
  for (i in 1:nThreshs) {
    accur_num[i] <- Accuracy(
      target_lgl = probs > threshold_vec[i],
      truth_lgl = truth_lgl
    )
  }
  accur_num
}

# Test
ROC_fun(threshold_vec = seq(0.01, 0.99, by = 0.01))
plot(
  ROC_fun(threshold_vec = seq(0.01, 0.99, by = 0.01))
)
