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




#### Check permutation test --------------------------------

# df <- tibble::tribble(
#         ~record_id, ~uba1_mutation, ~mcv_at_time_of_biopsy,
#                 1L,           "No",                   98.2,
#                 3L,           "No",                   87.4,
#                 4L,           "No",                     82,
#                 5L,           "No",                     95,
#                 6L,           "No",                   97.7,
#                 7L,           "No",                     85,
#                 8L,           "No",                   95.6,
#                 9L,           "No",                     95,
#                10L,           "No",                   93.4,
#                11L,           "No",                   90.4,
#                12L,           "No",                   86.7,
#                14L,           "No",                   78.7,
#                16L,           "No",                   88.2,
#                17L,           "No",                   89.4,
#                18L,           "No",                   76.9,
#                20L,           "No",                   92.7,
#                21L,           "No",                     89,
#                22L,           "No",                   79.3,
#                23L,           "No",                   93.2,
#                24L,           "No",                     96,
#                25L,           "No",                   90.1,
#                27L,           "No",                   79.1,
#                28L,          "Yes",                   95.9,
#                30L,           "No",                   85.1,
#                31L,           "No",                   89.4,
#                32L,           "No",                   94.6,
#                34L,           "No",                   86.6,
#                35L,           "No",                   95.6,
#                36L,          "Yes",                     96,
#                37L,           "No",                   88.9,
#                39L,           "No",                   88.4,
#                40L,           "No",                   89.4,
#                41L,           "No",                   89.4,
#                42L,           "No",                   93.3,
#                43L,           "No",                   94.1,
#                44L,           "No",                   90.9,
#                45L,           "No",                   73.5,
#                46L,           "No",                   77.9,
#                47L,           "No",                     87,
#                48L,           "No",                   89.6,
#                49L,          "Yes",                  108.5,
#                50L,          "Yes",                  117.3,
#                51L,           "No",                   88.5,
#                53L,           "No",                  100.4,
#                54L,           "No",                   86.4,
#                55L,           "No",                   89.8,
#                56L,           "No",                   85.7,
#                59L,           "No",                   95.1,
#                61L,           "No",                   84.4,
#                62L,           "No",                   88.7,
#                63L,           "No",                   91.9,
#                64L,           "No",                   92.7,
#                65L,           "No",                   87.8,
#                66L,           "No",                   93.2,
#                67L,           "No",                   98.8,
#                70L,           "No",                   91.7,
#                73L,           "No",                   76.5,
#                75L,           "No",                   92.3,
#                76L,           "No",                   76.8,
#                77L,           "No",                   90.6,
#                78L,           "No",                  108.2,
#                79L,           "No",                   90.6,
#                81L,           "No",                   83.9,
#                82L,           "No",                     NA,
#                83L,           "No",                   91.1,
#                85L,           "No",                   88.6,
#                86L,           "No",                   84.9,
#                87L,           "No",                   81.4,
#                88L,           "No",                   93.8,
#                89L,           "No",                   89.4,
#                91L,           "No",                   89.8,
#                92L,           "No",                   92.7,
#                93L,           "No",                     67,
#                94L,           "No",                     NA,
#                96L,           "No",                  101.1,
#                98L,           "No",                   87.1,
#               100L,           "No",                   89.9,
#               102L,           "No",                   96.2,
#               103L,           "No",                   78.7,
#               104L,           "No",                  119.3,
#               105L,           "No",                   95.4,
#               106L,           "No",                     85,
#               107L,           "No",                   88.9,
#               108L,           "No",                   66.5,
#               110L,           "No",                   64.7,
#               111L,           "No",                   62.3,
#               112L,           "No",                   99.3,
#               113L,           "No",                   92.7,
#               114L,           "No",                   79.4,
#               115L,           "No",                   90.2,
#               116L,           "No",                   91.4,
#               117L,           "No",                  105.6,
#               119L,           "No",                   86.5,
#               120L,           "No",                   86.9,
#               121L,           "No",                   89.8,
#               122L,           "No",                     84,
#               123L,           "No",                   88.8,
#               124L,           "No",                     85,
#               125L,           "No",                     82
#         )
#
#
#
# df |>
#   print(n = Inf)
#
# lamisc::calc_ttest(data = df,
#                    var = mcv_at_time_of_biopsy,
#                    by = uba1_mutation,
#                    include_perm = TRUE)
