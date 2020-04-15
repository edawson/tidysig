#' Transform per-signature feature counts to proportions
#' @param x a tibble or dataframe (TidySig format) of counts
#' @return a tibble with the Amount column containing proportions per-signature.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
normalize_counts <- function(x){
  if (sum(x$Amount) > 1){
    x <- x %>%
      dplyr::group_by(Signature) %>%
      dplyr::mutate(tot = sum(Amount)) %>%
      dplyr::mutate(Amount = Amount / tot) %>%
      dplyr::select(-tot)
  }
  return (x)
}

#' Convert per-sample counts to proportions
#' @param x a TidySig tibble
#' @return A TidySig tibble with the Amount column now containing a proportion.
#' @export
normalize_sample_counts <- function(x){
  if (sum(x$Amount) > 1){
    x <- x %>%
      dplyr::group_by(Sample) %>%
      dplyr::mutate(tot = sum(Amount)) %>%
      dplyr::mutate(Amount = Amount / tot) %>%
      dplyr::select(-tot)
  }
  return (x)
}
