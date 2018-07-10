
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# if (getRversion() >= "2.15.1")  utils::globalVariables(c(".",
#                                                          "combo",
#                                                          "conf_low",
#                                                          "conf_high",
#                                                          "estimate",
#                                                          "kap",
#                                                          "po",
#                                                          "se_po",
#                                                          "x",
#                                                          "y"))
