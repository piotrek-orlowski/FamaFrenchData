#' List Fama-French files
#'
#' @param file_extension character, FF supply CSV or TXT data.
#'
#' @return character vector of filenames available from the website
#' @export
#'

ff_list_available_files <- function(file_extension = c("TXT","CSV")){

  # ask for FF website  ---  2018-03-16 14:09:41  -----
  webpage_content <- RCurl::getURLContent(ff_url)

  # split into lines  ---  2018-03-16 14:09:55  -----
  webpage_content <- strsplit(x = webpage_content, split = "\n")[[1]]

  # pick lines which contain .zip extensions  ---  2018-03-16 14:11:26  -----
  zip_lines <- webpage_content[grep("\\.zip", webpage_content)]

  # extract filenames  ---  2018-03-16 14:12:40  -----
  zip_positions <- cbind(unlist(gregexpr(pattern = "ftp/", text = zip_lines, fixed = TRUE)) + 4
                         , unlist(gregexpr(pattern = ".zip", text = zip_lines, fixed = TRUE))+3
                         )

  zip_lines <- substr(zip_lines, zip_positions[,1], zip_positions[,2])

  # filter for extensions  ---  2018-03-16 14:27:41  -----
  zip_lines <- zip_lines[grep(paste0(file_extension,collapse="|"),zip_lines,ignore.case = TRUE)]

  # return  ---  2018-03-16 14:27:06  -----
  return(zip_lines)
}
