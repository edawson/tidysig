#' @export
drop_enforced_signatures <- function(x){
  return(
    x %>%
      dplyr::filter(!grepl("NESS_ENFORCED_SIGNATURE", Signature))
  )
}
