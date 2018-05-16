#' Get data dictionary
#'
#' This function reads the data dictionary from a .csv file and returns it as a list
#' @param path The path to the data dictionary. Defaults to "extdata/".
#' @param file_name The name of the data dictionary file. Defaults to "data_dictionary.csv".
#' @export
get.data.dictionary <- function(
                                path = "extdata/",
                                file_name = "data_dictionary.csv"
                                )
{
    ## Paste path and filename to form a full path
    fp <- paste0(path, file_name)
    ## Get data dictionary
    data_dictionary <- fread(fp, data.table = FALSE)
    ## Modify data dictionary column names
    data_dictionary_names <- names(data_dictionary) # Store current dictionary names
    new_names <- unlist(lapply(data_dictionary_names, function(x) unlist(strsplit(x, "\\(|\\)"))[2])) # Get short names
    names(data_dictionary) <- new_names # Apply names
    ## Make a list of the data dictionary
    data_dictionary_list <- lapply(setNames(nm = data_dictionary$vn), function(x) as.list(data_dictionary[data_dictionary$vn == x, ]))
    return(data_dictionary_list)
}

