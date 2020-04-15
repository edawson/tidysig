paste_count_helper <- function(x, y){
  return (paste(y, x, "Count", sep = "_"))
}

paste_prop_helper <- function(x, y){
  return (paste(y, x, "Proportion", sep = "_"))
}

#' Generate a sample-level summary of Signature proportions
#' @param x A TidySig tibble
#' @param label An optional label to append to the output column name(s)
#' @return A tibble, with new columns (one per signature) listing per-sample proportions of that signature.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' 
#' @export
per_sample_proportions_summary <- function(x, label="Sigprofiler"){
  tots <- x %>% dplyr::group_by(Sample) %>%
    dplyr::summarize(nTotal = sum(Amount)) %>% 
    dplyr::distinct()
  x <- dplyr::left_join(x, tots, by = c("Sample"))
  x <- x %>% dplyr::mutate(Amount = Amount / nTotal)
  x <- x %>% dplyr::select(-nTotal)
  x <- x %>%
    dplyr::spread(Signature, Amount)
  mods <- x %>% dplyr::select(-Sample) %>% names()
  x <- x %>%
    dplyr::rename_at(mods, paste_prop_helper, label)
  return(x)
}

#' Generate a sample-level summary of Signature counts
#' @param x A TidySig tibble
#' @param label An optional label to append to the output column name(s)
#' @return A tibble, with new columns (one per signature) listing per-sample counts of that signature.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
per_sample_counts_summary <- function(x, label="Sigprofiler"){
  
  totalVarName = paste(label, "total", sep="_")
  tots <- x %>% dplyr::group_by(Sample) %>%
    dplyr::summarize({{totalVarName}} := sum(Amount)) %>% 
    dplyr::distinct()
  x <- x %>%
    dplyr::spread(Signature, Amount)
  mods <- x %>% dplyr::select(-Sample) %>% names()


  x <- x %>%
    dplyr::rename_at(mods, paste_count_helper, label)
  x <- dplyr::left_join(x, tots)
  
  return(x)
}

#' Generate a sample-level summary of Signature counts and proportions
#' @param x A TidySig tibble
#' @param label An optional label to append to the output column name(s)
#' @return A tibble, with new columns (two per signature) listing per-sample counts and
#' proportions of that signature.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
per_sample_summary <- function(x, label = "tidysig"){
  y <- per_sample_counts_summary(x, label)
  z <- per_sample_proportions_summary(x, label)
  ret <- dplyr::left_join(y, z, by=c("Sample"))
  return(ret)
}
