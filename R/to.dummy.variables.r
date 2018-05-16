#' Transform factor variables to dummy variables
#'
#' This function transforms factor variables to dummy variables.
#' @param prepped_data. The prepared data for SuperLearner predictions as list. No default.
#' @export
to.dummy.variables <- function(
                               prepped_data
                               )
{
    if (!is.list(prepped_data)) stop("Prepped data is not a list")
    transformed_sets <- lapply(prepped_data$sets,
                               function(the_set)
                                   as.data.frame(model.matrix( ~.,
                                                              data = the_set)[, -1]))
    names(transformed_sets) <- c("x_train", "x_review")
    prepped_data[["sets"]] <- NULL
    data <- c(transformed_sets,
              prepped_data)

    return(data)
}
