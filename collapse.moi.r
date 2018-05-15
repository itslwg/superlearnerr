#' Collapse mechanism of injury function
#'
#' Takes the main data as input and generates a new mechanism of injury variable with overall levels instead of codes.
#' @param study_data The study data as a data frame. No default
#' @export
collapse.moi <- function(
                         study_data
                         )
{
    ## Test that study data if a data frame, stop if not
    if (!is.data.frame(study_data)) stop("study_data is not a data frame")
    ## Transport accidents are V01-V99, falls are W00-W19, burns are X00-X19, other
    ## external causes of accidental injury are W20-W99 and X20-X59, intentional
    ## self-harm is X60-X84, assault is X85-Y09, events of undetermined intent is
    ## Y10-Y34, and legal intervention is Y35-Y36
    c00_09 <- paste0(0, 0:9) # make vector of codes 00-09
    ## List of lists, in which each sub-list holds two vectors. The first vector is
    ## called codes and has the codes used to define a certain mechanism and the
    ## second is called level and has the name of that mechanism
    moi_list <- list(ta = list(codes = paste0("V", c(c00_09, 10:99)),
                               level = "Transport accident"),
                     falls = list(codes = paste0("W", c(c00_09, 10:19)),
                                  level = "Fall"),
                     burns = list(codes = paste0("X", c(c00_09, 10:19)),
                                  level = "Burn"),
                     oeca = list(codes = c(paste0("W", 20:99), paste0("X", 20:59)),
                                 level = "Other external cause of accidental injury"),
                     ish = list(codes = paste0("X", 60:84),
                                level = "Intentional self harm"),
                     assault = list(codes = c(paste0("X", 85:99), paste0("Y", c00_09)),
                                    level = "Assault"),
                     ui = list(codes = paste0("Y", 10:34),
                               level = "Event of undetermined intent"),
                     li = list(codes = paste0("Y", 35:36),
                               level  = "Legal intervention"))
    new_moi <- as.character(study_data$moi) # make a new character vector of moi
    ## Replace codes with levels in the new copy
    for (m in moi_list)
        new_moi[grep(paste0(m$codes, collapse = "|"), study_data$moi)] <- m$level
    ## Replace old moi with new, factor moi in dataset
    study_data$moi <- as.factor(new_moi)
    return (study_data)
}
