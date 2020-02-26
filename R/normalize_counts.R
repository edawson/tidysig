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
