#' Create sample characteristics table
#'
#' Creates a table of sample characteristics 
#' @param data_for_table The data to use to create the table, no default.
#' @param data_dictionary the data dictionary, no default.
#' @param strata The strata variable, defaults to NULL. 
#' @param vars The variables to include in the table, defaults to NULL, in which case it is defined as names(data_dictionary)[sapply(data_dictionary, function(x) x$incl == "Yes")].
#' @param exclude_vars Character vector of variable names to exclude from table, defaults to NULL.
#' @param include_overall Logical and used only if strata is not NULL. defaults to TRUE in which case an overall column is also included.
#' @param include_missing_column Logical. If TRUE a column is missing indicating the number (%) of missing values in each variable. Details to TRUE.
#' @param digits Integer. Number of digits to use when rounding table entries. Defaults to 1. 
#' @param save Logical. If TRUE the table object is also saved to disk as a .tex file. Defaults to FALSE.
#' @export
create.table.of.sample.characteristics <- function(
                                                   data_for_table,
                                                   data_dictionary,
                                                   strata = NULL,
                                                   vars = NULL,
                                                   exclude_vars = NULL,
                                                   include_overall = TRUE,
                                                   include_missing_column = TRUE,
                                                   digits = 1,
                                                   save = FALSE
                                                   )
{
    ## Identify type of data and reformat if necessary
    prepped <- FALSE
    if ("prepped" %in% class(data_for_table)) {
        table_data <- with(data_for_table, rbind(data.frame(sets$x_train, y = y_train, set = "Training"), data.frame(sets$x_review, y = y_review, set = "Test")))
        table_data$y <- factor(table_data$y, labels = data_for_table$y_levels)
        names(table_data)[grep("^y$", names(table_data))] <- data_for_table$y_name
        prepped <- TRUE
    } else table_data <- data_for_table
    ## Define vars
    if (is.null(vars)) vars <- names(data_dictionary)[sapply(data_dictionary, function(x) x$incl == "Yes")]
    if (is.null(strata) & prepped) strata <- "set"
    ## Exclude exclude_vars from table vars
    if (!is.null(exclude_vars)) vars <- vars[!(vars %in% exclude_vars)]
    ## Define table data
    full_table_data <- table_data
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
    ## Format table cells
    table_copy <- table
    sqb_index <- grep("\\[", table_copy)
    sqb_data <- table_copy[sqb_index]
    sqb_fmt <- unlist(lapply(sqb_data, function(x) {
        numbers <- as.numeric(trimws(unlist(strsplit(x, "\\[|,|\\]"))))
        new_numbers <- sprintf(paste0("%.", digits, "f"), numbers)
        cell <- paste0(new_numbers[1], " [", new_numbers[2], ", ", new_numbers[3], "]")
        return(cell)
    }))
    table[sqb_index] <- sqb_fmt
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
    nrns <- orns <- rownames(table) # Get current rownames
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
    ## Add missing values column
    if (include_missing_column) {
        missing_column <- rep("", nrow(table))
        for (variable in vars) {
            missing_variable <- grep(paste0(variable, "_missing"), colnames(full_table_data), value = TRUE)
            missing_entry <- "0 (0)"
            if (length(missing_variable) != 0) {
                missing_data <- full_table_data[, missing_variable]
                n_missing <- sum(missing_data == 0)
                p_missing <- round(100 - mean(missing_data) * 100, digits = digits)
                missing_entry <- paste0(n_missing, " (", p_missing, ")")
            }
            index <- grep(paste0(variable, " "), rownames(table))
            missing_column[index] <- missing_entry
        }
        missing_cols <- full_table_data[, grep("_missing", colnames(full_table_data))]
        total_n_missing <- nrow(full_table_data) - nrow(missing_cols[-unique(which(missing_cols == 0, arr.ind = TRUE)[, 1]), ])
        total_p_missing <- round(total_n_missing/nrow(full_table_data) * 100, digits = digits)
        missing_column[1] <- paste0(total_n_missing, " (", total_p_missing, ")*")
        missing_column <- matrix(missing_column, ncol = 1)
        colnames(missing_column) <- "Missing values, n (%)"
        rownames(missing_column) <- NULL
        rownames(table) <- NULL
        table <- cbind(table, missing_column)
    }
    rownames(table) <- NULL # Remove rownames
    ## Save raw table object
    tables <- list(raw = table)
    rownames(tables$raw) <- orns # Add old rownames back, for easy access
    ## Make abbreviations and explanations text
    abbrv <- paste0("Abbreviations and explanations: ", paste0(sort(unlist(abbr)), collapse = "; ")) # Make abbreviation string
    ## Format the table using xtable
    formatted_table <- print.xtable(xtable(table,
                                           caption = "\\bf Characteristics of the samples analysed in this study",
                                           label = "tab:sample-characteristics"),
                                    type = "latex",
                                    table.placement = "!ht",
                                    include.rownames = FALSE,
                                    include.colnames = TRUE,
                                    caption.placement = "top",
                                    print.results = FALSE)
    star_caption <- abbr
    if (include_missing_column) star_caption <- paste0("*The total number (\\%) of observations with missing data. ", abbrv)
    formatted_table <- add.star.caption(formatted_table, star_caption) # add caption*
    ## Dirty fix to make sure subscripts format correctly
    formatted_table <- gsub("SUPS ", "\\textsubscript{", formatted_table, fixed = TRUE)
    formatted_table <- gsub(" SUPE", "}", formatted_table, fixed = TRUE)
    ## Adjust width to fit plos latex template
    formatted_table <- sub("\\begin{table}[!ht]",
                           paste("\\begin{table}[!ht] \n",
                                 "\\begin{adjustwidth}{-2.25in}{0in}"),
                           formatted_table,
                           fixed = TRUE)
    formatted_table <- sub("\\end{table}",
                           paste("\\end{adjustwidth} \n",
                                 "\\end{table}"),
                           formatted_table,
                           fixed = TRUE)
    ## Add adjustbox to make table fit on page
    formatted_table <- sub("\\begin{tabular}",
                           paste0("\\begin{adjustbox}{max width=\\linewidth} \n",
                                  "\\begin{tabular} \n"),
                           formatted_table,
                           fixed = TRUE)
    formatted_table <- sub("\\end{tabular}",
                           paste0("\\end{tabular} \n",
                                  "\\end{adjustbox}"),
                           formatted_table,
                           fixed = TRUE)
    ## Put formatted table in tables
    tables$formatted <- formatted_table
    ## Save formatted table to disk if save is TRUE
    if (save) write(formatted_table, "table_of_sample_characteristics.tex")
    return(tables)
}
