#' Transpose a SigProfiler dataframe
#' @param x A dataframe produced by reading in a SigProfiler matrix (usually with readr::read_tsv)
#' @return A tibble with Context/Changes as rows and the Sample/Signature as rows.
#' @import dplyr tibble
#' @importFrom magrittr "%>%"
transpose_sigprofiler_df <- function(x){
  ttt <- combine_context_change_cols(x)
  if ("MutationsType" %in% names(ttt)){
    ttt <- ttt %>% dplyr::rename(MutationType=MutationsType)
  }
  ttt <- ttt %>% tibble::column_to_rownames("MutationType")
  ttt_t <- as.data.frame(t(as.matrix(ttt)))
  ttt_t <- ttt_t %>% tibble::rownames_to_column(var="Signature")
  ttt_t <- arrange_vars(ttt_t, c(Signature=1))
  return (ttt_t)
}

## TODO: make this also handle probs / counts / proportions

#' Transform a Sigprofiler dataframe (of counts, percentages,
#' signatures, or activities) into a Tidy format.
#' @param x A dataframe from the SigProfiler output (usually read in with readr::read_tsv)
#' @return A Tidy-formatted tibble of signatures, activities, or counts.
#' @export
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr separate

transform_sigprofiler_df <- function(x){
  
  labelProbs = FALSE
  dataType=NULL
  ## Handles irregular variable names in PCAWG, COSMIC, and different versions
  ## of SigProfilerMatrixGenerator and SigProfilerExtractor
  if ("MutationsType" %in% names(x) ){
    x <- x %>% dplyr::rename(MutationType=MutationsType)
  } else if ("MutationTypes" %in% names(x)){
    x <- x %>% dplyr::rename(MutationType=MutationTypes)
  } else if("Mutation type" %in% names(x) & ! "Trinucleotide" %in% names(x)){
    x <- x %>% dplyr::rename(MutationType=`Mutation type`)
  }
  
  
  ## Probabilities files
  if ("Sample Names" %in% names(x) &
      "MutationType" %in% names(x) &
      (x %>% dplyr::distinct(MutationType) %>% count())$n == 96){
    message("Transforming sigprofiler SBS96 probability file.")

    labelProbs = TRUE
    x <- x %>% rename(Sample=`Sample Names`)
    x <- x %>% tidyr::pivot_longer(-c(Sample, MutationType), names_to = "variable", values_to = "value") %>%
      mutate(Context = paste(str_sub(MutationType, 1, 1), str_sub(MutationType, 3, 3), str_sub(MutationType, 7, 7), sep =""),
             Change = str_sub(MutationType, 3, 5)) %>%
      rename(Signature = variable, probability = value) %>%
      dplyr::select(-MutationType) %>%
      arrange(Sample, Signature, Change, Context)
    x <- arrange_vars(x, c(Sample=1,Signature=2,Change=3,Context=4,probability=5))
    return(x)
  } else if ("Sample Names" %in% names(x) &
             "MutationType" %in% names(x) &
             (x %>% distinct(MutationType) %>% count())$n == 83){
    message("Transforming sigprofiler ID83 probability file.")
    x <- x %>% rename(Sample = `Sample Names`) %>%
      tidyr::pivot_longer(-c("Sample", "MutationType"), names_to = "variable", values_to = "value") %>%
      tidyr::separate(MutationType, c("Length", "Type", "Motif", "MotifLength")) %>%
      mutate(Motif = case_when(
        Motif == "R" ~ "Repeat",
        Motif == "M" ~ "Microhomology",
        TRUE ~ Motif
      )) %>%
      rename(Signature = variable, probability = value) %>%
      arrange(Sample,Signature,Length,Type,Motif,MotifLength)
    
    x <- arrange_vars(x, c(Sample=1,Signature=2,Length=3,Type=4,Motif=5,MotifLength=6))
    return(x)
  } else if ("Samples" %in% names(x)){
    message("Transforming SigProfiler activities file")
    return (tidy_sigprof_activities(x))
  }
  
  
  x <- combine_context_change_cols(x)
  
  feature_count <- (x %>% distinct(MutationType) %>% count())$n
  
  x <- transpose_sigprofiler_df(x)
  
  if (feature_count == 83){
    message("Transforming SigProfiler ID83 signature / counts file.")
    x <- x %>% tidyr::pivot_longer(-Signature,names_to = "variable",values_to = "value") %>%
      tidyr::separate(variable, c("Length", "Type", "Motif", "MotifLength")) %>%
      mutate(Motif = case_when(
        Motif == "R" ~ "Repeat",
        Motif == "M" ~ "Microhomology",
        TRUE ~ Motif
      )) %>%
      rename(Amount = value)
  }
  else if (feature_count == 96){
    message("Transforming SigProfiler SBS96 signature / counts file.")
    x <- x %>% 
      tidyr::pivot_longer(-Signature,names_to = "variable",values_to="value") %>%
      dplyr::mutate(Context = paste(stringr::str_sub(variable, 1, 1), stringr::str_sub(variable, 3, 3), stringr::str_sub(variable, 7, 7), sep =""),
                    Change = stringr::str_sub(variable, 3, 5)) %>%
      dplyr::rename(Amount = value) %>%
      dplyr::select(-variable) %>%
      dplyr::select(Signature, Change, Context, Amount) %>%
      dplyr::arrange(Signature, Change, Context)
  }
  else{
    
  }
  return(x)
}

#' A wrapper around transform_sigprofiler_df for legacy compatibility
#' @param x a SigProfiler signature dataframe
#' @return A Tidy-formatted tibble of signature amounts.
#' @export
tidy_sigprof_SBS96_df <- function(x){
  warning("tidy_sigprof_SBS96_df and tidy_sigprof_ID83_df are deprecated. Please use the transform_sigprofiler_df function." )
  return(
    transform_sigprofiler_df(x)
  )
}

#' A wrapper around transform_sigprofiler_df for legacy compatibility (ID83 contexts)
#' @param x A dataframe of sigprofiler counts / signatures
#' @return A Tidy-formatted tibble of signature amounts.
#' @export
tidy_sigprof_ID83_df <- function(x){
  warning("tidy_sigprof_SBS96_df and tidy_sigprof_ID83_df are deprecated. Please use the transform_sigprofiler_df function." )
  return(
    transform_sigprofiler_df(x)
  )
}


#' A fuction for formatting the signature probability files of SigProfiler
#' @param x a dataframe of SigProfiler SBS96 probabilities per signatures
#' @return A Tidy-formatted tibble of probabilities.
#' @importFrom magrittr "%>%"
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

#' A fuction for formatting the signature probability files of SigProfiler (ID83 context)
#' @param x a dataframe of SigProfiler ID83 probabilities per signatures
#' @return A Tidy-formatted tibble of probabilities.
#' @importFrom tidyr separate
#' @import dplyr
#' @importFrom reshape2 melt
#' @export
tidy_sigprof_id83_probabilities <- function(x){
  x <- x %>%
    dplyr::rename(Sample = `Sample Names`) %>%
    reshape2::melt(id.vars = c("Sample", "MutationTypes")) %>%
    tidyr::separate(MutationTypes, c("Length", "Type", "Motif", "MotifLength")) %>%
    dplyr::mutate(Motif = case_when(
      Motif == "R" ~ "Repeat",
      Motif == "M" ~ "Microhomology",
      TRUE ~ Motif
    )) %>%
    dplyr::rename(Signature = variable, probability = value) %>%
    dplyr::arrange(Sample,Signature,Length,Type,Motif,MotifLength)
  
  x <- arrange_vars(x, c(Sample=1,Signature=2,Length=3,Type=4,Motif=5,MotifLength=6))
  return (x)
}

#' Transform sigprofiler activities into TidySig format
#' @param x a Sigprofiler dataframe
#' @return a TidySig tibble
#' @import dplyr
#' @importFrom reshape2 melt
#' @export
tidy_sigprof_activities <- function(x){
  x <- x %>%
    rename(Sample = Samples) %>%
    pivot_longer(-Sample, names_to = "variable", values_to = "value") %>%
    rename(Signature = variable, Amount = value)
  return (x)
}

#' A helper function to 
#' 1. Properly rename columns in SigProfiler inputs
#' 2. Transform the Context/Change columns to the proper format.
#' @param x A dataframe of Sigprofiler signatures / counts
#' @return a tibble, with the new columns "Change" and "Context"
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @importFrom stringr str_sub
combine_context_change_cols <- function(x){
  if ("MutationType" %in% names(x) | "MutationsType" %in% names(x)){
    return(x)
  }
  if ("Mutation type" %in% names(x)){
    x <- x %>% dplyr::rename(Change = `Mutation type`)
  }
  if ("Trinucleotide" %in% names(x)){
    x <- x %>% dplyr::rename(Context = Trinucleotide)
  }
  x <- x %>%
    dplyr::mutate(MutationType = paste(str_sub(Context,1,1),
                                "[",
                                Change,
                                "]",
                                str_sub(Context,3,3),
                                sep=""))
  x <- arrange_vars(x, c(MutationType=1)) 
  x <- x %>% dplyr::select(-Change, -Context)
  return (x)
}

#' Convert a Tidy-formatted tibble to the SigProfiler matrix format
#' @param x a Tidy-formatted tibble of Sigprofiler signatures / counts
#' @return A tibble in the expected SigProfiler format
#' @export
export_to_sigprofiler_SBS96 <- function(x){
  x <- x %>%
    tidyr::pivot_wider(id_cols=c(Signature, Context, Change),
                values_from=Amount,
                names_from = Signature)
  x <- combine_context_change_cols(x)
  return (x)
}

#' Read in a PCAWG-format CSV file
#' @param filename The name of a CSV input file
#' @return a TidySig tibble
#' @importFrom readr read_csv
#' @export
read_pcawg_SBS96_csv <- function(filename){
  x <- readr::read_csv(filename)
  tidy_x <- tidy_sigprof_SBS96_df(convert_pcawg_to_sigprofiler_SBS96(x))
  return(tidy_x)
}

#' Convert a pcawg-formatted dataframe to modern sigprofiler SBS96.
#' PCAWG's sigprofiler formatting is different from that of
#' the current version, so we munge the column names.
#' @param x a dataframe from PCAWG's sigprofiler examples (usually read in with readr::read_csv)
#' @return A tibble with the column names conforming to SigProfiler's expected column names.
#' @export
convert_pcawg_to_sigprofiler_SBS96 <- function(x){
  return(
    combine_context_change_cols(x)
  )
}

#' Transform a tibble in TidySig format
#' to SigantureAnalyzer's word format.
#' @param x a Tidy-formatted tibble of signatures
#' @return A tibble in SignatureAnalyzer's word format
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_sub
#' @export
export_to_signatureanalyzer_word_SBS96 <- function(x){
  x <- export_to_sigprofiler(x)
  x <- x %>%
    dplyr::mutate(context96.word = paste(stringr::str_sub(MutationType,1,1),
                                         stringr::str_sub(MutationType,3,3),
                                         stringr::str_sub(MutationType,5,5),
                                         stringr::str_sub(MutationType,7,7),sep=""))
  x <- arrange_vars(x,c(context96.word=1))
  x <- x %>% dplyr::select(-MutationType)
  return(x)
}




