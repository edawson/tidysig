paste_helper <- function(x){
  return (paste("SigProfiler", x, "Count", sep = "_"))
}

#' @export
per_sample_activity_counts <- function(x){
  x <- x %>%
    spread(Signature, Amount)
  mods <- colnames(x)[grepl("96[A-Z]*|ID[A-Z]*", colnames(x))]
  
  x <- x %>% 
    rename_at(mods, paste_helper)
  return(x)
}
