paste_count_helper <- function(x, y){
  return (paste(y, x, "Count", sep = "_"))
}

paste_prop_helper <- function(x, y){
  return (paste(y, x, "Proportion", sep = "_"))
}

#' @export
per_sample_proportions_summary <- function(x, label="Sigprofiler"){
  tots <- x %>% group_by(Sample) %>%
    summarize(nTotal = sum(Amount)) %>% 
    distinct()
  x <- left_join(x, tots, by = c("Sample"))
  x <- x %>% mutate(Amount = Amount / nTotal)
  x <- x %>% select(-nTotal)
  x <- x %>%
    spread(Signature, Amount)
  mods <- x %>% select(-Sample) %>% names()
  x <- x %>%
    rename_at(mods, paste_prop_helper, label)
  return(x)
}

#' @export
per_sample_counts_summary <- function(x, label="Sigprofiler"){
  
  totalVarName = paste(label, "total", sep="_")
  tots <- x %>% group_by(Sample) %>%
    summarize({{totalVarName}} := sum(Amount)) %>% 
    distinct()
  x <- x %>%
    spread(Signature, Amount)
  mods <- x %>% select(-Sample) %>% names()


  x <- x %>%
    rename_at(mods, paste_count_helper, label)
  x <- left_join(x, tots)
  
  return(x)
}

#' @export
per_sample_summary <- function(x, label = "tidysig"){
  y <- per_sample_count_summary(x, label)
  z <- per_sample_proportion_summary(x, label)
  ret <- left_join(y, z, by=c("Sample"))
  return(ret)
}
