#
#
# test_that("calc_ttest matches t.test for paired t-test", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#
#   # Base R paired t-test
#   ref <- t.test(mpg ~ treated, data = fuel, paired = TRUE) |> broom::tidy()
#
#   # lamisc version
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, paired = TRUE)
#
#   # Compare key components
#   expect_equal(res$method, "Paired t-test")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[res$hypothesis_tests$alternative == "two.sided"], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[res$hypothesis_tests$alternative == "two.sided"], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[res$hypothesis_tests$alternative == "two.sided"], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for paired t-test", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#
#   ref <- t.test(mpg ~ treated, data = fuel, paired = TRUE) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, paired = TRUE)
#
#   expect_equal(res$method, "Paired t-test")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[res$hypothesis_tests$alternative == "two.sided"], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[res$hypothesis_tests$alternative == "two.sided"], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[res$hypothesis_tests$alternative == "two.sided"], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for unpaired t-test with unequal variances", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#
#   ref <- t.test(mpg ~ treated, data = fuel, paired = FALSE, var.equal = FALSE) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, paired = FALSE, var_equal = FALSE)
#
#   expect_equal(res$method, "Two-sample t-test with unequal variances")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[res$hypothesis_tests$alternative == "two.sided"], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[res$hypothesis_tests$alternative == "two.sided"], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[res$hypothesis_tests$alternative == "two.sided"], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for one-sample t-test", {
#   fuel <- tibble::tibble(mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19))
#   ref <- t.test(fuel$mpg, mu = 20) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, mu = 20)
#
#   expect_equal(res$method, "One-sample t-test")
#   expect_equal(res$summary_stats$mean[1], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[res$hypothesis_tests$alternative == "two.sided"], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[res$hypothesis_tests$alternative == "two.sided"], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[res$hypothesis_tests$alternative == "two.sided"], ref$parameter, tolerance = 1e-6)
# })
#
#
#
#
# test_that("calc_ttest matches base R for one-sample t-test", {
#   fuel <- tibble::tibble(mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19))
#   ref <- t.test(fuel$mpg, mu = 20) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, mu = 20)
#
#   expect_equal(res$method, "One-sample t-test")
#   expect_equal(res$summary_stats$mean[1], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[1], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[1], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[1], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for two-sample unpaired t-test (unequal variance)", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#   ref <- t.test(mpg ~ treated, data = fuel, var.equal = FALSE) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, var_equal = FALSE)
#
#   expect_equal(res$method, "Two-sample t-test with unequal variances")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[1], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[1], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[1], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for two-sample unpaired t-test (equal variance)", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#   ref <- t.test(mpg ~ treated, data = fuel, var.equal = TRUE) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, var_equal = TRUE)
#
#   expect_equal(res$method, "Two-sample t-test with equal variances")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[1], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[1], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[1], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest matches base R for paired t-test", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#   ref <- t.test(mpg ~ treated, data = fuel, paired = TRUE) |> broom::tidy()
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, paired = TRUE)
#
#   expect_equal(res$method, "Paired t-test")
#   expect_equal(res$summary_stats$mean[res$summary_stats$group == "diff"], ref$estimate, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$statistic[1], ref$statistic, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$p_value[1], ref$p.value, tolerance = 1e-6)
#   expect_equal(res$hypothesis_tests$df[1], ref$parameter, tolerance = 1e-6)
# })
#
#
# test_that("calc_ttest returns permutation test p-values", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, include_perm = TRUE, n_perms = 500)
#   expect_true("p_value_perm_test" %in% names(res$hypothesis_tests))
#   expect_equal(nrow(res$permutation_distribution), 500)
# })
#
#
# test_that("calc_ttest returns non-parametric test p-values", {
#   fuel <- tibble::tibble(
#     mpg = c(20, 23, 21, 25, 18, 17, 18, 24, 20, 24, 23, 19,
#             24, 25, 21, 22, 23, 18, 17, 28, 24, 27, 21, 23),
#     treated = rep(c(0, 1), each = 12)
#   )
#   res <- calc_ttest(data = fuel, var = mpg, by = treated, include_np = TRUE)
#   expect_true("p_value_non_param" %in% names(res$hypothesis_tests))
# })
