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

#' Returns a set of ylimits trimmed to fit the input dataframe.
#' @param x A TidySig tibble
#' @param fitMinimum fit a minimum value instead of defaulting to zero.
#' @return A vector of two elements containing calculated ylimits.
#' @export
calculate_smart_ylimits <- function(x, fitMinimum=FALSE, asInteger=FALSE){
  min_y <- 0
  if (fitMinimum){
    min_y <- add_val_buffer(get_minimum_amount(x), buffer_pct = 0.1, ceilingBuf = FALSE, asInteger = asInteger)
  }
  max_y <-add_val_buffer(get_maximum_amount(x), buffer_pct = 0.1, ceilingBuf = TRUE, asInteger = asInteger)

  return (c(min_y, max_y))
}

#' Add some amount of buffer to a value.
#' Used to calculate the min/max amounts
#' for ylimits
#' @param val A numeric type.
#' @param buffer_pct The proportion of val to add/subtract to/from val.
#' @param ceilingBuf If true, add buffer_pct to val. Otherwise, subtract buffer_pct from val.
#' @param asInteger If true, return the nearest ceiling/floor integer to the buffered value.
#' @return A numeric value (v +- buffer_pct * val)
add_val_buffer <- function(val, buffer_pct=0.01, ceilingBuf=TRUE, asInteger=FALSE){
  stopifnot(buffer_pct < 1)
  ret <- NULL
  if (ceilingBuf){
    ret <- val + (val * buffer_pct)
    if (asInteger){
      ret <- ceiling(ret)
    }
  }
  else{
    ret <- max(0, val - (val * buffer_pct))
    if (asInteger){
      ret <- floor(ret)
    }
  }
  return(ret)
}

#' Retrieve the maximum value
#' of a signature or activity dataframe.
#' This function can be used to set intelligent y-limits
#' in plotting functions.
#' @param x a TidySig tibble
#' @return A numeric with the highest Amount in the dataframe
#' @export
get_maximum_amount <- function(x){
  return (max(x$Amount))
}

#' Retrieve the minimum value
#' of a signature or activity dataframe.
#' This function can be used to set intelligent y-limits
#' in plotting functions.
#' @param x a TidySig tibble
#' @return A numeric with the lowest Amount in the dataframe
#' @export
get_minimum_amount <- function(x){
  return (min(x$Amount))
}
