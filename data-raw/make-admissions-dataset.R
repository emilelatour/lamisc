# Data courtesy of:
# "https://stats.idre.ucla.edu/stat/data/binary.csv"

library(tibble)

admissions <- tibble::tribble(
             ~admit, ~gre, ~gpa, ~rank,
                 0L, 380L, 3.61,    3L,
                 1L, 660L, 3.67,    3L,
                 1L, 800L,    4,    1L,
                 1L, 640L, 3.19,    4L,
                 0L, 520L, 2.93,    4L,
                 1L, 760L,    3,    2L,
                 1L, 560L, 2.98,    1L,
                 0L, 400L, 3.08,    2L,
                 1L, 540L, 3.39,    3L,
                 0L, 700L, 3.92,    2L,
                 0L, 800L,    4,    4L,
                 0L, 440L, 3.22,    1L,
                 1L, 760L,    4,    1L,
                 0L, 700L, 3.08,    2L,
                 1L, 700L,    4,    1L,
                 0L, 480L, 3.44,    3L,
                 0L, 780L, 3.87,    4L,
                 0L, 360L, 2.56,    3L,
                 0L, 800L, 3.75,    2L,
                 1L, 540L, 3.81,    1L,
                 0L, 500L, 3.17,    3L,
                 1L, 660L, 3.63,    2L,
                 0L, 600L, 2.82,    4L,
                 0L, 680L, 3.19,    4L,
                 1L, 760L, 3.35,    2L,
                 1L, 800L, 3.66,    1L,
                 1L, 620L, 3.61,    1L,
                 1L, 520L, 3.74,    4L,
                 1L, 780L, 3.22,    2L,
                 0L, 520L, 3.29,    1L,
                 0L, 540L, 3.78,    4L,
                 0L, 760L, 3.35,    3L,
                 0L, 600L,  3.4,    3L,
                 1L, 800L,    4,    3L,
                 0L, 360L, 3.14,    1L,
                 0L, 400L, 3.05,    2L,
                 0L, 580L, 3.25,    1L,
                 0L, 520L,  2.9,    3L,
                 1L, 500L, 3.13,    2L,
                 1L, 520L, 2.68,    3L,
                 0L, 560L, 2.42,    2L,
                 1L, 580L, 3.32,    2L,
                 1L, 600L, 3.15,    2L,
                 0L, 500L, 3.31,    3L,
                 0L, 700L, 2.94,    2L,
                 1L, 460L, 3.45,    3L,
                 1L, 580L, 3.46,    2L,
                 0L, 500L, 2.97,    4L,
                 0L, 440L, 2.48,    4L,
                 0L, 400L, 3.35,    3L,
                 0L, 640L, 3.86,    3L,
                 0L, 440L, 3.13,    4L,
                 0L, 740L, 3.37,    4L,
                 1L, 680L, 3.27,    2L,
                 0L, 660L, 3.34,    3L,
                 1L, 740L,    4,    3L,
                 0L, 560L, 3.19,    3L,
                 0L, 380L, 2.94,    3L,
                 0L, 400L, 3.65,    2L,
                 0L, 600L, 2.82,    4L,
                 1L, 620L, 3.18,    2L,
                 0L, 560L, 3.32,    4L,
                 0L, 640L, 3.67,    3L,
                 1L, 680L, 3.85,    3L,
                 0L, 580L,    4,    3L,
                 0L, 600L, 3.59,    2L,
                 0L, 740L, 3.62,    4L,
                 0L, 620L,  3.3,    1L,
                 0L, 580L, 3.69,    1L,
                 0L, 800L, 3.73,    1L,
                 0L, 640L,    4,    3L,
                 0L, 300L, 2.92,    4L,
                 0L, 480L, 3.39,    4L,
                 0L, 580L,    4,    2L,
                 0L, 720L, 3.45,    4L,
                 0L, 720L,    4,    3L,
                 0L, 560L, 3.36,    3L,
                 1L, 800L,    4,    3L,
                 0L, 540L, 3.12,    1L,
                 1L, 620L,    4,    1L,
                 0L, 700L,  2.9,    4L,
                 0L, 620L, 3.07,    2L,
                 0L, 500L, 2.71,    2L,
                 0L, 380L, 2.91,    4L,
                 1L, 500L,  3.6,    3L,
                 0L, 520L, 2.98,    2L,
                 0L, 600L, 3.32,    2L,
                 0L, 600L, 3.48,    2L,
                 0L, 700L, 3.28,    1L,
                 1L, 660L,    4,    2L,
                 0L, 700L, 3.83,    2L,
                 1L, 720L, 3.64,    1L,
                 0L, 800L,  3.9,    2L,
                 0L, 580L, 2.93,    2L,
                 1L, 660L, 3.44,    2L,
                 0L, 660L, 3.33,    2L,
                 0L, 640L, 3.52,    4L,
                 0L, 480L, 3.57,    2L,
                 0L, 700L, 2.88,    2L,
                 0L, 400L, 3.31,    3L,
                 0L, 340L, 3.15,    3L,
                 0L, 580L, 3.57,    3L,
                 0L, 380L, 3.33,    4L,
                 0L, 540L, 3.94,    3L,
                 1L, 660L, 3.95,    2L,
                 1L, 740L, 2.97,    2L,
                 1L, 700L, 3.56,    1L,
                 0L, 480L, 3.13,    2L,
                 0L, 400L, 2.93,    3L,
                 0L, 480L, 3.45,    2L,
                 0L, 680L, 3.08,    4L,
                 0L, 420L, 3.41,    4L,
                 0L, 360L,    3,    3L,
                 0L, 600L, 3.22,    1L,
                 0L, 720L, 3.84,    3L,
                 0L, 620L, 3.99,    3L,
                 1L, 440L, 3.45,    2L,
                 0L, 700L, 3.72,    2L,
                 1L, 800L,  3.7,    1L,
                 0L, 340L, 2.92,    3L,
                 1L, 520L, 3.74,    2L,
                 1L, 480L, 2.67,    2L,
                 0L, 520L, 2.85,    3L,
                 0L, 500L, 2.98,    3L,
                 0L, 720L, 3.88,    3L,
                 0L, 540L, 3.38,    4L,
                 1L, 600L, 3.54,    1L,
                 0L, 740L, 3.74,    4L,
                 0L, 540L, 3.19,    2L,
                 0L, 460L, 3.15,    4L,
                 1L, 620L, 3.17,    2L,
                 0L, 640L, 2.79,    2L,
                 0L, 580L,  3.4,    2L,
                 0L, 500L, 3.08,    3L,
                 0L, 560L, 2.95,    2L,
                 0L, 500L, 3.57,    3L,
                 0L, 560L, 3.33,    4L,
                 0L, 700L,    4,    3L,
                 0L, 620L,  3.4,    2L,
                 1L, 600L, 3.58,    1L,
                 0L, 640L, 3.93,    2L,
                 1L, 700L, 3.52,    4L,
                 0L, 620L, 3.94,    4L,
                 0L, 580L,  3.4,    3L,
                 0L, 580L,  3.4,    4L,
                 0L, 380L, 3.43,    3L,
                 0L, 480L,  3.4,    2L,
                 0L, 560L, 2.71,    3L,
                 1L, 480L, 2.91,    1L,
                 0L, 740L, 3.31,    1L,
                 1L, 800L, 3.74,    1L,
                 0L, 400L, 3.38,    2L,
                 1L, 640L, 3.94,    2L,
                 0L, 580L, 3.46,    3L,
                 0L, 620L, 3.69,    3L,
                 1L, 580L, 2.86,    4L,
                 0L, 560L, 2.52,    2L,
                 1L, 480L, 3.58,    1L,
                 0L, 660L, 3.49,    2L,
                 0L, 700L, 3.82,    3L,
                 0L, 600L, 3.13,    2L,
                 0L, 640L,  3.5,    2L,
                 1L, 700L, 3.56,    2L,
                 0L, 520L, 2.73,    2L,
                 0L, 580L,  3.3,    2L,
                 0L, 700L,    4,    1L,
                 0L, 440L, 3.24,    4L,
                 0L, 720L, 3.77,    3L,
                 0L, 500L,    4,    3L,
                 0L, 600L, 3.62,    3L,
                 0L, 400L, 3.51,    3L,
                 0L, 540L, 2.81,    3L,
                 0L, 680L, 3.48,    3L,
                 1L, 800L, 3.43,    2L,
                 0L, 500L, 3.53,    4L,
                 1L, 620L, 3.37,    2L,
                 0L, 520L, 2.62,    2L,
                 1L, 620L, 3.23,    3L,
                 0L, 620L, 3.33,    3L,
                 0L, 300L, 3.01,    3L,
                 0L, 620L, 3.78,    3L,
                 0L, 500L, 3.88,    4L,
                 0L, 700L,    4,    2L,
                 1L, 540L, 3.84,    2L,
                 0L, 500L, 2.79,    4L,
                 0L, 800L,  3.6,    2L,
                 0L, 560L, 3.61,    3L,
                 0L, 580L, 2.88,    2L,
                 0L, 560L, 3.07,    2L,
                 0L, 500L, 3.35,    2L,
                 1L, 640L, 2.94,    2L,
                 0L, 800L, 3.54,    3L,
                 0L, 640L, 3.76,    3L,
                 0L, 380L, 3.59,    4L,
                 1L, 600L, 3.47,    2L,
                 0L, 560L, 3.59,    2L,
                 0L, 660L, 3.07,    3L,
                 1L, 400L, 3.23,    4L,
                 0L, 600L, 3.63,    3L,
                 0L, 580L, 3.77,    4L,
                 0L, 800L, 3.31,    3L,
                 1L, 580L,  3.2,    2L,
                 1L, 700L,    4,    1L,
                 0L, 420L, 3.92,    4L,
                 1L, 600L, 3.89,    1L,
                 1L, 780L,  3.8,    3L,
                 0L, 740L, 3.54,    1L,
                 1L, 640L, 3.63,    1L,
                 0L, 540L, 3.16,    3L,
                 0L, 580L,  3.5,    2L,
                 0L, 740L, 3.34,    4L,
                 0L, 580L, 3.02,    2L,
                 0L, 460L, 2.87,    2L,
                 0L, 640L, 3.38,    3L,
                 1L, 600L, 3.56,    2L,
                 1L, 660L, 2.91,    3L,
                 0L, 340L,  2.9,    1L,
                 1L, 460L, 3.64,    1L,
                 0L, 460L, 2.98,    1L,
                 1L, 560L, 3.59,    2L,
                 0L, 540L, 3.28,    3L,
                 0L, 680L, 3.99,    3L,
                 1L, 480L, 3.02,    1L,
                 0L, 800L, 3.47,    3L,
                 0L, 800L,  2.9,    2L,
                 1L, 720L,  3.5,    3L,
                 0L, 620L, 3.58,    2L,
                 0L, 540L, 3.02,    4L,
                 0L, 480L, 3.43,    2L,
                 1L, 720L, 3.42,    2L,
                 0L, 580L, 3.29,    4L,
                 0L, 600L, 3.28,    3L,
                 0L, 380L, 3.38,    2L,
                 0L, 420L, 2.67,    3L,
                 1L, 800L, 3.53,    1L,
                 0L, 620L, 3.05,    2L,
                 1L, 660L, 3.49,    2L,
                 0L, 480L,    4,    2L,
                 0L, 500L, 2.86,    4L,
                 0L, 700L, 3.45,    3L,
                 0L, 440L, 2.76,    2L,
                 1L, 520L, 3.81,    1L,
                 1L, 680L, 2.96,    3L,
                 0L, 620L, 3.22,    2L,
                 0L, 540L, 3.04,    1L,
                 0L, 800L, 3.91,    3L,
                 0L, 680L, 3.34,    2L,
                 0L, 440L, 3.17,    2L,
                 0L, 680L, 3.64,    3L,
                 0L, 640L, 3.73,    3L,
                 0L, 660L, 3.31,    4L,
                 0L, 620L, 3.21,    4L,
                 1L, 520L,    4,    2L,
                 1L, 540L, 3.55,    4L,
                 1L, 740L, 3.52,    4L,
                 0L, 640L, 3.35,    3L,
                 1L, 520L,  3.3,    2L,
                 1L, 620L, 3.95,    3L,
                 0L, 520L, 3.51,    2L,
                 0L, 640L, 3.81,    2L,
                 0L, 680L, 3.11,    2L,
                 0L, 440L, 3.15,    2L,
                 1L, 520L, 3.19,    3L,
                 1L, 620L, 3.95,    3L,
                 1L, 520L,  3.9,    3L,
                 0L, 380L, 3.34,    3L,
                 0L, 560L, 3.24,    4L,
                 1L, 600L, 3.64,    3L,
                 1L, 680L, 3.46,    2L,
                 0L, 500L, 2.81,    3L,
                 1L, 640L, 3.95,    2L,
                 0L, 540L, 3.33,    3L,
                 1L, 680L, 3.67,    2L,
                 0L, 660L, 3.32,    1L,
                 0L, 520L, 3.12,    2L,
                 1L, 600L, 2.98,    2L,
                 0L, 460L, 3.77,    3L,
                 1L, 580L, 3.58,    1L,
                 1L, 680L,    3,    4L,
                 1L, 660L, 3.14,    2L,
                 0L, 660L, 3.94,    2L,
                 0L, 360L, 3.27,    3L,
                 0L, 660L, 3.45,    4L,
                 0L, 520L,  3.1,    4L,
                 1L, 440L, 3.39,    2L,
                 0L, 600L, 3.31,    4L,
                 1L, 800L, 3.22,    1L,
                 1L, 660L,  3.7,    4L,
                 0L, 800L, 3.15,    4L,
                 0L, 420L, 2.26,    4L,
                 1L, 620L, 3.45,    2L,
                 0L, 800L, 2.78,    2L,
                 0L, 680L,  3.7,    2L,
                 0L, 800L, 3.97,    1L,
                 0L, 480L, 2.55,    1L,
                 0L, 520L, 3.25,    3L,
                 0L, 560L, 3.16,    1L,
                 0L, 460L, 3.07,    2L,
                 0L, 540L,  3.5,    2L,
                 0L, 720L,  3.4,    3L,
                 0L, 640L,  3.3,    2L,
                 1L, 660L,  3.6,    3L,
                 1L, 400L, 3.15,    2L,
                 1L, 680L, 3.98,    2L,
                 0L, 220L, 2.83,    3L,
                 0L, 580L, 3.46,    4L,
                 1L, 540L, 3.17,    1L,
                 0L, 580L, 3.51,    2L,
                 0L, 540L, 3.13,    2L,
                 0L, 440L, 2.98,    3L,
                 0L, 560L,    4,    3L,
                 0L, 660L, 3.67,    2L,
                 0L, 660L, 3.77,    3L,
                 1L, 520L, 3.65,    4L,
                 0L, 540L, 3.46,    4L,
                 1L, 300L, 2.84,    2L,
                 1L, 340L,    3,    2L,
                 1L, 780L, 3.63,    4L,
                 1L, 480L, 3.71,    4L,
                 0L, 540L, 3.28,    1L,
                 0L, 460L, 3.14,    3L,
                 0L, 460L, 3.58,    2L,
                 0L, 500L, 3.01,    4L,
                 0L, 420L, 2.69,    2L,
                 0L, 520L,  2.7,    3L,
                 0L, 680L,  3.9,    1L,
                 0L, 680L, 3.31,    2L,
                 1L, 560L, 3.48,    2L,
                 0L, 580L, 3.34,    2L,
                 0L, 500L, 2.93,    4L,
                 0L, 740L,    4,    3L,
                 0L, 660L, 3.59,    3L,
                 0L, 420L, 2.96,    1L,
                 0L, 560L, 3.43,    3L,
                 1L, 460L, 3.64,    3L,
                 1L, 620L, 3.71,    1L,
                 0L, 520L, 3.15,    3L,
                 0L, 620L, 3.09,    4L,
                 0L, 540L,  3.2,    1L,
                 1L, 660L, 3.47,    3L,
                 0L, 500L, 3.23,    4L,
                 1L, 560L, 2.65,    3L,
                 0L, 500L, 3.95,    4L,
                 0L, 580L, 3.06,    2L,
                 0L, 520L, 3.35,    3L,
                 0L, 500L, 3.03,    3L,
                 0L, 600L, 3.35,    2L,
                 0L, 580L,  3.8,    2L,
                 0L, 400L, 3.36,    2L,
                 0L, 620L, 2.85,    2L,
                 1L, 780L,    4,    2L,
                 0L, 620L, 3.43,    3L,
                 1L, 580L, 3.12,    3L,
                 0L, 700L, 3.52,    2L,
                 1L, 540L, 3.78,    2L,
                 1L, 760L, 2.81,    1L,
                 0L, 700L, 3.27,    2L,
                 0L, 720L, 3.31,    1L,
                 1L, 560L, 3.69,    3L,
                 0L, 720L, 3.94,    3L,
                 1L, 520L,    4,    1L,
                 1L, 540L, 3.49,    1L,
                 0L, 680L, 3.14,    2L,
                 0L, 460L, 3.44,    2L,
                 1L, 560L, 3.36,    1L,
                 0L, 480L, 2.78,    3L,
                 0L, 460L, 2.93,    3L,
                 0L, 620L, 3.63,    3L,
                 0L, 580L,    4,    1L,
                 0L, 800L, 3.89,    2L,
                 1L, 540L, 3.77,    2L,
                 1L, 680L, 3.76,    3L,
                 1L, 680L, 2.42,    1L,
                 1L, 620L, 3.37,    1L,
                 0L, 560L, 3.78,    2L,
                 0L, 560L, 3.49,    4L,
                 0L, 620L, 3.63,    2L,
                 1L, 800L,    4,    2L,
                 0L, 640L, 3.12,    3L,
                 0L, 540L,  2.7,    2L,
                 0L, 700L, 3.65,    2L,
                 1L, 540L, 3.49,    2L,
                 0L, 540L, 3.51,    2L,
                 0L, 660L,    4,    1L,
                 1L, 480L, 2.62,    2L,
                 0L, 420L, 3.02,    1L,
                 1L, 740L, 3.86,    2L,
                 0L, 580L, 3.36,    2L,
                 0L, 640L, 3.17,    2L,
                 0L, 640L, 3.51,    2L,
                 1L, 800L, 3.05,    2L,
                 1L, 660L, 3.88,    2L,
                 1L, 600L, 3.38,    3L,
                 1L, 620L, 3.75,    2L,
                 1L, 460L, 3.99,    3L,
                 0L, 620L,    4,    2L,
                 0L, 560L, 3.04,    3L,
                 0L, 460L, 2.63,    2L,
                 0L, 700L, 3.65,    2L,
                 0L, 600L, 3.89,    3L
             )


usethis::use_data(admissions, overwrite = TRUE)
