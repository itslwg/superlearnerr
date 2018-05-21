#' Transform factor variables to dummy variables
#'
#' This function transforms factor variables to dummy variables.
#' @param prepped_data. The study_data as prepared by prep.data.for.superlearner as list. No default.
#' @export
to.dummy.variables <- function(
                               prepped_data
                               )
{
    ## Error handling
    if (!is.list(prepped_data)) stop("Prepared data is not a list")
    ## Extract training and review sets and transform factor variables to dummy variables
    transformed_sets <- lapply(prepped_data$sets, function(set) {
        new_set <- as.data.frame(model.matrix( ~., data = set)[, -1])
        colnames(new_set) <- gsub(" ", "_", colnames(new_set))
        return(new_set)
        
    })
    ## Name the sets
    names(transformed_sets) <- c("x_train", "x_review")
    prepped_data[["sets"]] <- NULL
    ## Bind modified sets with function input prepped_data
    data <- c(transformed_sets,
              prepped_data)

    return(data)
}
