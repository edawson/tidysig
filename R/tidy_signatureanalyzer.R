
## https://github.com/pandas-dev/pandas/issues/9636

#' Loads a Pandas data frame, restricting to a top level path restrictToPath if desired
#' @param finame The name of the input HDF5 file
#' @param restrictToPath a top-level path in the HDF5 file.
#' @importFrom magrittr "%>%"
#' @import tibble
#' @import dplyr
#' @import hdf5r
load_pandas_hdf5_data <- function(finame, restrictToPath="") {
    data <- NULL
    ret <- NULL
    fi <- H5File$new(finame, mode="r+")
    listing <- fi[[restrictToPath]]$ls(recursive=TRUE)
    #message(listing)
    # Find all data nodes, values are stored in *_values and corresponding column
    # titles in *_items

    rnames <- fi[[paste(restrictToPath, "axis1",sep = "/")]]$read()
    #colnames(rnames) <- c("Context")
    #message(rnames)

    data_nodes <- grep("_values", listing$name)
    name_nodes <- grep("_items", listing$name)

    data_paths = paste(restrictToPath, listing$group[data_nodes], listing$name[data_nodes], sep = "/")
    name_paths = paste(restrictToPath, listing$group[name_nodes], listing$name[name_nodes], sep = "/")
    columns = list()
    for (idx in seq(data_paths)) {
        #message("loading", data_paths[idx])
        data <- data.frame(t(fi[[data_paths[idx]]]$read()))
        dnames  <- t(fi[[name_paths[idx]]]$read())
        entry <- data.frame(data)
        colnames(entry) <- dnames
        columns <- append(columns, entry)
    }

    data <- data.frame(columns)
    #data$contexts <- rnames
    data <- data %>% dplyr::select(!starts_with("NA"))
    data <- tibble::tibble(data)
    data$rname <- rnames

    return(data)
}


load_signatureanalyzer_sigs <- function(finame){
    h5_dat <- load_pandas_hdf5_data(finame, "signatures")
    return (NULL)
}


