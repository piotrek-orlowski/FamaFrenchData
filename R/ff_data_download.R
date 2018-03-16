#' Fama-French data download
#'
#' @param file_name name of FF zip file
#' @param all_subsets logical, if TRUE, returns all data subdivisions from file, if FALSE, only the first table (i.e. until first line-break after data)
#'
#' @description Downloads data from \url{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}, cleans, returns nice form.
#'
#' @return *tidy* \code{data.frame} with return data. Columns date_start, date_end are of type \code{Date}, columns series_name, series_type, data_type are \code{character}; column value is \code{numeric}.
#' @export
#'
ff_tidy_data <- function(file_name
                         , all_subsets = FALSE
) {


}
