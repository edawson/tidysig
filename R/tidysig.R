#' \code{tidysig} package
#'
#' tidysig: A tidyverse-style package for plotting mutational signatures.
#'
#' See the README on
#' \href{https://github.com/edawson/tidysig/README.md}{GitHub}
#'
#' @docType package
#' @name tidysig
NULL

## silence R CMD check notes about missing global variables.
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Signature",
                                                        "Amount",
                                                        "Context",
                                                        "Change",
                                                        "Motif",
                                                        "nTotal",
                                                        "Sample",
                                                        "Samples",
                                                        "variable",
                                                        "amount",
                                                        "MotifLength",
                                                        "MotifLengthChar",
                                                        "CompoundMotif",
                                                        "Trinucleotide",
                                                        "Mutation Type",
                                                        "MutationType",
                                                        "MutationTypes",
                                                        "MutationsType",
                                                        "Mutation type",
                                                        "Sample Names",
                                                        "Length",
                                                        "tot",
                                                        "totalForOrder",
                                                        "Type"
                                                        ))
