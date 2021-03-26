#' Fama-French data download
#'
#' @param file_names names of FF zip files, character vector
#'
#' @description Downloads data from \url{http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}, cleans, returns nice form.
#'
#' @return *tidy* \code{data.frame} with return data. Column date is of type \code{Date}, columns series_name, series_type, data_type, frequency are \code{character}; column value is \code{numeric}.
#' @export
#'
ff_tidy_data <- function(file_names) {

  # temporary directory  ---  2018-03-16 14:55:57  -----
  r_temp_dir <- tempdir()

  # force lowercase
  file_names <- tolower(file_names)

  save_paths <- file.path(r_temp_dir, file_names)

  # create download urls  ---  2018-03-16 14:49:08  -----
  download_urls <- file.path(ff_ftp_url, file_names)

  # attempt to get files  ---  2018-03-16 14:49:16  -----
  sapply(download_urls, function(url){
    tryCatch(expr = download.file(url, destfile = save_paths[which(sapply(download_urls,function(x) identical(x,url)))]))
  })

  # unzip  ---  2018-03-16 15:14:23  -----
  file_paths <- lapply(save_paths, unzip, exdir = r_temp_dir, list = TRUE) %>% bind_rows()
  file_paths <- file_paths$Name

  file_paths <- sprintf("%s/%s", r_temp_dir, file_paths)

  # # extract file paths from zip paths  ---  2018-03-16 15:16:03  -----
  # file_paths <- gsub(".zip","",save_paths)
  # file_paths <- gsub("_txt",".txt",file_paths)
  # file_paths <- gsub("_csv",".csv",file_paths)

  # apply to file paths  ---  2018-03-16 17:46:19  -----
  output_list <- lapply(file_paths, FamaFrenchData:::ff_data_extract)

  output <- bind_rows(output_list)
  return(output)
}
