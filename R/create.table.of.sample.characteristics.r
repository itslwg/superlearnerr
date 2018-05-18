#' Create sample characteristics table
#'
#' Creates a table of sample characteristics 
#' @param study_data The study data frame, no default.
#' @param data_dictionary the data dictionary, no default.
#' @param strata The strata variable, defaults to NULL. 
#' @param vars The variables to include in the table, defaults to NULL, in which case it is defined as names(data_dictionary)[sapply(data_dictionary, function(x) x$incl == "Yes")].
#' @param exclude_vars Character vector of variable names to exclude from table, defaults to NULL.
#' @param include_overall Logical and used only if strata is not NULL. defaults to TRUE in which case an overall column is also included.
#' @param save Logical. If TRUE the table object is also saved to disk as a .tex file. Defaults to FALSE.
#' @export
create.table.of.sample.characteristics <- function(
                                                   study_data,
                                                   data_dictionary,
                                                   strata = NULL,
                                                   vars = NULL,
                                                   exclude_vars = NULL,
                                                   include_overall = TRUE,
                                                   digits = 2,
                                                   save = FALSE
                                                   )
{
    ## Identify type of data and reformat if necessary
    prepped <- FALSE
    if ("prepped" %in% class(study_data)) {
        table_data <- with(study_data, rbind(data.frame(sets$x_train, y = y_train, set = "Training"), data.frame(sets$x_review, y = y_review, set = "Test")))
        table_data$y <- factor(table_data$y, labels = study_data$y_levels)
        names(table_data)[grep("^y$", names(table_data))] <- study_data$y_name
        prepped <- TRUE
    } else table_data <- study_data
    ## Define vars
    if (is.null(vars)) vars <- names(data_dictionary)[sapply(data_dictionary, function(x) x$incl == "Yes")]
    if (is.null(strata) & prepped) strata <- "set"
    ## Exclude exclude_vars from table vars
    if (!is.null(exclude_vars)) vars <- vars[!(vars %in% exclude_vars)]
    ## Define table data
    table_data <- table_data[, c(vars, strata)]
    ## Make a list that will hold the individual tables
    table_list <- list()
    ## Create the stratified table if there should be one
    if (!is.null(strata)) {
        vars <- vars[!(vars %in% strata)] # Remove the strata variable from the list of variables to be put in the table
        table_list$t0 <- CreateTableOne(vars = vars, strata = strata, data = table_data, test = FALSE) # Create the stratified table
    }
    ## Create the overall table if there should be one
    if (is.null(strata) | include_overall) table_list$t1 <- CreateTableOne(vars = vars, data = table_data)
    ## Define variables to be treated as non-normally distributed, i.e. so that they are reported using medians and IQR
    nonormal <- sapply(table_data, is.numeric)
    ## Format the tables in table_list
    formatted_tables <- lapply(table_list, print, nonnormal = vars[nonormal], noSpaces = TRUE, catDigits = digits, showAllLevels = TRUE, printToggle = FALSE)
    ## Combine the formatted tables into one
    table <- do.call(cbind, formatted_tables)
    ## Remove duplicate level columns
    level_indices <- grep("level", colnames(table)) # Find the indices of columns named level
    if (length(level_indices) > 1) table <- table[, -level_indices[2]] # Remove the second level column
    ## Rename level column
    colnames(table)[1] <- "Level"
    ## Modify the first table row with n to also include percentages
    if (!is.null(strata)) {
        ni <- grep("^n$", rownames(table)) # Get index of row with n
        nnum <- as.numeric(table[ni, ]) # Make numeric
        ps <- round(nnum/nrow(table_data) * 100, digits = digits) # Estimate percentages
        fmt <- paste0("%.", digits, "f") # Generate format based on number of digits
        nn <- paste0(nnum, " (", sprintf(fmt, ps), ")") # Format numbers with percentages
        table[ni, ] <- nn # Put back in table
        rownames(table)[ni] <- "n (%)" # Modify name of n row
        table["n (%)", "Level"] <- ""
    }
    ## Replace variable names with labels
    nrns <- rownames(table) # Get current rownames
    abbr <- list() # Genderate vector to hold abbreviations
    for (x in vars) {
        vdd <- data_dictionary[[x]] # Get variable specific data dictionary
        l <- vdd$al # Get abbreviated as label
        if (l == "") l <- vdd$l else abbr[[x]] <- paste0(vdd$al, ", ", vdd$l) # If there is no abbreviated label get full label, else store full label to use in explanatory note
        i <- grep(paste0("^", x), rownames(table)) # Get position of old rowname
        nrns[i] <- sub(paste0("^", x), l, rownames(table)[i]) # Put new rowname there
    }
    table <- cbind(nrns, table) # Add rownames as column
    colnames(table)[1] <- "Characteristic" # Name that column
    rownames(table) <- NULL # R rownames
    abbrv <- paste0("Abbreviations and explanations: ", paste0(sort(unlist(abbr)), collapse = "; ")) # Make abbreviation string
    ## Format the table using xtable
    formatted_table <- print.xtable(xtable(table,
                                           caption = "Sample characteristics"),
                                    type = "latex",
                                    include.rownames = FALSE,
                                    include.colnames = TRUE,
                                    caption.placement = "top",
                                    print.results = FALSE)
    formatted_table <- add.star.caption(formatted_table, abbrv) # add caption*
    ## Dirty fix to make sure superscripts format correctly
    formatted_table <- gsub("SUPS ", "\\textsuperscript{", formatted_table, fixed = TRUE)
    formatted_table <- gsub(" SUPE", "}", formatted_table, fixed = TRUE)
    ## Add adjustbox to make table fit on page
    formatted_table <- sub("\\begin{tabular}",
                           paste0("\\begin{adjustbox}{max width=\\textwidth} \n",
                                  "\\begin{tabular} \n"),
                           formatted_table,
                           fixed = TRUE)
    formatted_table <- sub("\\end{tabular}",
                           paste0("\\end{tabular} \n",
                                  "\\end{adjustbox}"),
                           formatted_table,
                           fixed = TRUE)
    ## Save formatted table to disk if save is TRUE
    if (save) write(formatted_table, "table_of_sample_characteristics.tex")
    return(formatted_table)
}
