
transpose_sigprofiler_df <- function(x){
  ttt <- combine_context_change_cols(x)
  if ("MutationsType" %in% names(ttt)){
    ttt <- ttt %>% rename(MutationType=MutationsType)
  }
  ttt <- ttt %>% column_to_rownames("MutationType")
  ttt_t <- as.data.frame(t(as.matrix(ttt)))
  ttt_t <- ttt_t %>% rownames_to_column(var="Signature")
  ttt_t <- arrange_vars(ttt_t, c(Signature=1))
  return (ttt_t)
}

## TODO: make this also handle probs / counts / proportions

#' @export
transform_sigprofiler_df <- function(x){
  
  labelProbs = FALSE
  dataType=NULL
  ## Handles irregular variable names in PCAWG, COSMIC, and different versions
  ## of SigProfilerMatrixGenerator and SigProfilerExtractor
  if ("MutationsType" %in% names(x) ){
    x <- x %>% rename(MutationType=MutationsType)
  } else if ("MutationTypes" %in% names(x)){
    x <- x %>% rename(MutationType=MutationTypes)
  } else if("Mutation type" %in% names(x) & ! "Trinucleotide" %in% names(x)){
    x <- x %>% rename(MutationType=`Mutation type`)
  }
  
  
  ## Probabilities files
  if ("Sample Names" %in% names(x) & "MutationType" %in% names(x)){
    message("Transforming sigprofiler activity file.")

    labelProbs = TRUE
    x <- x %>% rename(Sample=`Sample Names`)
    x <- x %>% pivot_longer(-c(Sample, MutationType), names_to = "variable", values_to = "value") %>%
      mutate(Context = paste(str_sub(MutationType, 1, 1), str_sub(MutationType, 3, 3), str_sub(MutationType, 7, 7), sep =""),
             Change = str_sub(MutationType, 3, 5)) %>%
      rename(Signature = variable, probability = value) %>%
      dplyr::select(-MutationType) %>%
      arrange(Sample, Signature, Change, Context)
    x <- arrange_vars(x, c(Sample=1,Signature=2,Change=3,Context=4,probability=5))
    return(x)
  }
  
  
  x <- combine_context_change_cols(x)
  
  feature_count <- (x %>% distinct(MutationType) %>% count())$n
  cat(feature_count)
  
  x <- transpose_sigprofiler_df(x)
  
  if (feature_count == 83){
    x <- x %>% pivot_longer(-Signature,names_to = "variable",values_to = "value") %>%
      separate(variable, c("Length", "Type", "Motif", "MotifLength")) %>%
      mutate(Motif = case_when(
        Motif == "R" ~ "Repeat",
        Motif == "M" ~ "Microhomology",
        TRUE ~ Motif
      )) %>%
      rename(Amount = value)
  }
  else if (feature_count == 96){
    x <- x %>% 
      tidyr::pivot_longer(-Signature,names_to = "variable",values_to="value") %>%
      dplyr::mutate(Context = paste(stringr::str_sub(variable, 1, 1), stringr::str_sub(variable, 3, 3), stringr::str_sub(variable, 7, 7), sep =""),
                    Change = stringr::str_sub(variable, 3, 5)) %>%
      dplyr::rename(Amount = value) %>%
      dplyr::select(-variable) %>%
      dplyr::select(Signature, Change, Context, Amount) %>%
      dplyr::arrange(Signature, Change, Context)
  }
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
  if ("MutationType" %in% names(x) | "MutationsType" %in% names(x)){
    return(x)
  }
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
read_pcawg_SBS96_csv <- function(filename){
  x <- read_csv(filename)
  tidy_x <- tidy_sigprof_SBS96_df(convert_pcawg_to_sigprofiler_SBS96(x))
  return(tidy_x)
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




