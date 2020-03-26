#' @export
transpose_sigprof_df <- function(x){
  rownames(x) <- x$`MutationType`
  x$`MutationType` <- NULL
  
  x_t <- as.data.frame(t(as.matrix(x)))
  x_t$Signature <- rownames(x_t)
  rownames(x_t) <- c()
  x_t <- arrange_vars(x_t, c(Signature=1))
  return (x_t)
}

#' @export
tidy_sigprof_id83_df <- function(x){
  x <- transpose_sigprof_df(x)
  x <- x %>% melt(id.vars="Signature") %>%
    separate(variable, c("Length", "Type", "Motif", "MotifLength")) %>%
    mutate(Motif = case_when(
      Motif == "R" ~ "Repeat",
      Motif == "M" ~ "Microhomology",
      TRUE ~ Motif
    )) %>%
    rename(Amount = value)
  return (x)
}

#' @export
tidy_sigprof_SBS96_df <- function(x){
  x <- transpose_sigprof_df(x)
  x <- x %>% melt(id.vars="Signature")
  x <- x %>%
    mutate(Context = paste(str_sub(variable, 1, 1), str_sub(variable, 3, 3), str_sub(variable, 7, 7), sep =""),
           Change = str_sub(variable, 3, 5)) %>%
    rename(Amount = value) %>%
    dplyr::select(-variable) %>%
    select(Signature, Change, Context, Amount) %>%
    arrange(Signature, Change, Context)
  return(x)
}

#' @export
tidy_sigprof_sbs96_probabilities <- function(x){
  x <- x %>%
    rename(Sample = `Sample Names`) %>%
    melt(id.vars = c("Sample", "MutationTypes")) %>%
    mutate(Context = paste(str_sub(MutationTypes, 1, 1), str_sub(MutationTypes, 3, 3), str_sub(MutationTypes, 7, 7), sep =""),
          Change = str_sub(MutationTypes, 3, 5)) %>%
    rename(Signature = variable, probability = value) %>%
    dplyr::select(-MutationTypes) %>%
    arrange(Sample, Signature, Change, Context)
  
  x <- arrange_vars(x, c(Sample=1,Signature=2,Change=3,Context=4,probability=5))
  return (x)
}


#' @export
tidy_sigprof_id83_probabilities <- function(x){
  x <- x %>%
    rename(Sample = `Sample Names`) %>%
    melt(id.vars = c("Sample", "MutationTypes")) %>%
    separate(MutationTypes, c("Length", "Type", "Motif", "MotifLength")) %>%
    mutate(Motif = case_when(
      Motif == "R" ~ "Repeat",
      Motif == "M" ~ "Microhomology",
      TRUE ~ Motif
    )) %>%
    rename(Signature = variable, probability = value) %>%
    arrange(Sample,Signature,Length,Type,Motif,MotifLength)
  
  x <- arrange_vars(x, c(Sample=1,Signature=2,Length=3,Type=4,Motif=5,MotifLength=6))
  return (x)
}

#' @export
tidy_sigprof_activities <- function(x){
  x <- x %>%
    rename(Sample = Samples) %>%
    melt(id.vars = c("Sample")) %>%
    rename(Signature = variable, Amount = value)
  return (x)
}

combine_context_change_cols <- function(x){
  if ("Mutation type" %in% names(x)){
    x <- x %>% rename(Change = `Mutation type`)
  }
  if ("Trinucleotide" %in% names(x)){
    x <- x %>% rename(Context = Trinucleotide)
  }
  x <- x %>%
    mutate(MutationType = paste(str_sub(Context,1,1),
                                "[",
                                Change,
                                "]",
                                str_sub(Context,3,3),
                                sep=""))
  x <- arrange_vars(x, c(MutationType=1)) 
  x <- x %>% select(-Change, -Context)
  return (x)
}

#' @export
export_to_sigprofiler_SBS96 <- function(x){
  x <- x %>%
    pivot_wider(id_cols=c(Signature, Context, Change),
                values_from=Amount,
                names_from = Signature)
  x <- combine_context_change_cols(x)
  return (x)
}

#' @export
convert_pcawg_to_sigprofiler_SBS96 <- function(x){
  return(
    combine_context_change_cols(x)
  )
}

#' @export
export_to_signatureanalyzer_word_SBS96 <- function(x){
  x <- export_to_sigprofiler(x)
  x <- x %>%
    mutate(context96.word = paste(str_sub(MutationType,1,1),
                                  str_sub(MutationType,3,3),
                                  str_sub(MutationType,5,5),
                                  str_sub(MutationType,7,7),sep=""))
  x <- arrange_vars(x,c(context96.word=1))
  x <- x %>% select(-MutationType)
  return(x)
}




