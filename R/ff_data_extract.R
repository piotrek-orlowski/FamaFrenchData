#' Extract data from downloaded file
#' @description Internal function
#' @param file character string, file connection
#'
#' @return

ff_data_extract <- function(file){

  raw_file <- readLines(file)

  # remove non-ASCII strings
  raw_file <- iconv(raw_file, sub = "")

  breaks_in_data <- which(sapply(raw_file, nchar)==0)
  if(length(breaks_in_data)==1){
    breaks_in_data <- c(breaks_in_data, length(raw_file)-1)
  }

  # identify data blocks  ---  2018-03-16 15:56:34  -----
  # assume anything longer than 24 lines is data  ---  2018-03-16 15:58:58  -----
  data_blocks_index <- cbind(breaks_in_data[which(diff(breaks_in_data)>=24)]+1, breaks_in_data[which(diff(breaks_in_data)>=24)+1])

  # list of separate data blocks  ---  2018-03-16 16:01:03  -----
  data_blocks_list <- lapply(1:nrow(data_blocks_index), function(ind){
    raw_file[data_blocks_index[ind,1]:data_blocks_index[ind,2]]
  })

  # what is our file extension?  ---  2018-03-16 16:05:52  -----
  file_extension <- stringr::str_sub(file,-3)

  # if CSV, try loading each block. if fails, remove first line, retry, as many times as necessary  ---  2018-03-16 16:07:26  -----
  if(file_extension %in% c("csv", "CSV")){
    data_blocks_list <- lapply(data_blocks_list, recursive_csv_read)
  } else if(file_extension %in% c("txt", "TXT")){

  }

  # bind data  ---  2018-03-16 16:54:46  -----
  data_tall <- bind_rows(data_blocks_list)

  # make sure dates are text  ---  2018-03-16 16:58:10  -----
  data_tall <- data_tall %>% mutate(date = as.character(date))

  # change dates to dates  ---  2018-03-16 16:55:16  -----
  data_tall <- data_tall %>%
    mutate(year = stringr::str_sub(date,1,4)
           , month = stringr::str_sub(date,5,6)
           , day = stringr::str_sub(date,7,8)
           ) %>%
    mutate(date = ifelse(month=="", sprintf("%s1231",year), date), month = ifelse(month=="","12",month)) %>%
    mutate(date = ifelse(day=="", sprintf("%s%s%s",year,month,as.character(lubridate::days_in_month(as.Date(sprintf("%s-%s-%s",year,month,"01"))))),date)) %>%
    mutate(date = as.Date(date,"%Y%m%d")) %>%
    select(-year,-month,-day)

  # mutate(date = ifelse(nchar(date)==4
  #                        , lubridate::as_date(paste0(date,"1231"), format= "%Y%m%d")
  #                        , ifelse(nchar(date==6)
  #                                 , lubridate::as_date(paste0(date,"01"), format="%Y%m%d") + lubridate::as.difftime(lubridate::days_in_month(lubridate::as_date(paste0(date,"01"), format="%Y%m%d"))-1,units = "days")
  #                                 , lubridate::as_date(date, format="%Y%m%d")
  #                                 )
  #                        )
  #          ) %>%
  #   mutate(date = as_date(date))

  # guess frequency  ---  2018-03-16 17:10:18  -----
  data_tall <- data_tall %>%
    group_by(ff_comment_lines) %>%
    mutate(frequency = ifelse(median(diff(date)) <= 4
                              , "daily"
                              , ifelse(median(diff(date)) <= 8
                                       , "weekly"
                                       , ifelse(median(diff(date)) <= 32
                                                , "monthly"
                                                , ifelse (median(diff(date)) <= 95
                                                          , "quarterly"
                                                          , "annual"
                                                          )
                                                )
                                       )
                              )
           ) %>%
    ungroup()

  # change data types  ---  2018-03-16 17:15:54  -----
  data_tall <- data_tall %>%
    rename(data_type = ff_comment_lines) %>%
    mutate(data_type = ifelse(data_type=="","value_weighted_excess_return",data_type))

  # really tall  ---  2018-03-16 17:21:40  -----
  data_tall <- data_tall %>%
    tidyr::gather_(key_col = "series_name"
                   , value_col = "value"
                   , gather_cols = colnames(.)[!grepl("date|frequency|type", colnames(.))]
                   )

  # divide values by 100  ---  2018-03-16 17:24:44  -----
  data_tall <- data_tall %>%
    mutate(value = value/100)

  # from filename determine if portfolio or factor return  ---  2018-03-16 17:27:05  -----
  data_tall <- data_tall %>%
    mutate(series_type = ifelse(grepl("portf",file,ignore.case = TRUE)
                                , "portfolio"
                                , ifelse(grepl("breakp",file,ignore.case = TRUE)
                                         , "breakpoint"
                                         , "factor"
                                         )
                                )
           )

  # if no "return" in data_type, extract data_type description from f****** formula  ---  2018-03-16 17:31:32  -----
  data_tall <- data_tall %>%
    mutate(data_type = gsub("for_portfolios_formed_in_june_of_year_t_","",data_type)) %>%
    mutate(data_type = gsub("_calculated.*","",data_type))

  # tolower series names  ---  2018-03-16 17:41:45  -----
  data_tall <- data_tall %>%
    mutate(series_name = tolower(series_name))

  # if data_type na and we're dealing with factors  ---  2018-03-16 17:44:52  -----
  data_tall <- data_tall %>%
    mutate(data_type = ifelse(data_type == "na" & series_type=="factor","value_weighted_excess_return",data_type))

  # add source filename  ---  2018-03-22 18:41:16  -----
  file_temp <- gsub("\\\\[^\\\\]*$","",file)
  # on linux we have to do it differently
  file_temp <- gsub("/[^/]*$","",file)
  file <- gsub(file_temp,"",file,fixed = "TRUE")
  file <- gsub("\\","",file,fixed = "TRUE")

  data_tall <- data_tall %>%
    mutate(source_file = file)

  return(data_tall)
}


recursive_csv_read <- function(conn){

  max_nrow <- length(conn)

  is_error <- TRUE
  n_skip <- 0
  while(is_error){

    temp_data <- tryCatch(read.csv(file = textConnection(conn), skip = n_skip, sep = ","), error = function(e) NA_real_)

    if(!is.na(temp_data)){
      if((nrow(temp_data) <= max_nrow) & (ncol(temp_data) > 1)){
        is_error <- FALSE
      } else {
        n_skip <- n_skip + 1
      }
    } else {
      n_skip <- n_skip + 1
    }

  }

  colnames(temp_data) <- gsub("\\.","_", colnames(temp_data))
  temp_data <- temp_data %>% rename(date = X)

  # scrub FF comment preceding data  ---  2018-03-16 16:42:43  -----
  ff_comment <- NA_character_
  if(n_skip > 0){
    ff_comment <- conn[1:n_skip]
  }

  ff_comment <- paste0(ff_comment, collapse = "_")
  ff_comment <- gsub("  ","",ff_comment)
  ff_comment <- gsub(" ","_",ff_comment)
  ff_comment <- tolower(ff_comment)

  temp_data <- temp_data %>% mutate(ff_comment_lines = ff_comment)

  return(temp_data)
}
