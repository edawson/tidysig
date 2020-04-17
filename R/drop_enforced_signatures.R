#' Remove enforced signatures from a dataframe
#' @param x A Tidy-formatted dataframe / tibble of signatures
#' @return A dataframe with all rows from enforced signatures removed
#' @import dplyr
#' @export
drop_enforced_signatures <- function(x){
  return(
    x %>%
      dplyr::filter(!grepl("NESS_ENFORCED_SIGNATURE", Signature))
  )
}
