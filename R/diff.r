#' 'Statistic' function to use with the "boot"-package
#'
#' This function is used as 'Statistic' in the generate.pvalue function.
#' @param d1 The study data as data frame. No default.
#' @param i The indice variable which defines the bootstrap sample.
#' @export
diff <- function(
                 d1,
                 i
                 )
{
    d = d1;
    d$x <- d$x[i];  # randomly re-assign groups
    ## Subset d1 for models
    model_1_df <- d[d$x %in% 'model_1', ]
    model_2_df <- d[d$x %in% 'model_2', ]
    ## List for looping
    dfs <- list(model_1 = model_1_df,
                model_2 = model_2_df)
    ## Calculate AUROOC for each df
    AUROCCS <- lapply(dfs, function(df) ROCR::performance(ROCR::prediction(df$Variable,
                                                                           labels = df$s30d),
                                                          'auc')@y.values)
    ## Calculate difference of estimates
    Diff <- AUROCCS$model_1[[1]] - AUROCCS$model_2[[1]]

    Diff
}
