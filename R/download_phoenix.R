# get all the URLs on a page
get_links <- function (phoenix_version, start_date, end_date) {
  phoenix_version <- gsub('.', '', phoenix_version, fixed = T) # remove dots

  # check version user input, either 'current' or up to 3 digits
  # with optional 'v' in the beginning
  if (!grepl('(current|v?\\d{,3})', phoenix_version)) stop('Incorrect version name.')

  if (!grepl('^(v|current)', phoenix_version)) # if the user submitted a version without 'v'
    phoenix_version <- paste0('v', phoenix_version)

  # Access the Phoenix API. 
  url <- "http://phoenixdata.org/data"
  page <- xml2::read_html(url)
  
  table_links <- page %>%
    rvest::html_table() %>% # get the html table
    dplyr::first() %>%      # get the first element from the list
    dplyr::mutate(Date = as.Date(Date), # convert to date
                  Link = xml2::url_absolute(Link, url)) %>% # make url absolute
    dplyr::filter(stringr::str_detect(Link, "\\.zip$"), # find those that end in zip
                  stringr::str_detect(Link, paste0('data/', phoenix_version))) # check the version
  
  if (!is.null(start_date)) {
    table_links <- table_links %>%
      dplyr::filter(Date >= start_date)
  }
    
  if (!is.null(end_date)) {
    table_links <- table_links %>%
      dplyr::filter(Date <= end_date)
  }
  
  return(table_links)
}


# given a link, download the file and write it to the specified directory
dw_file <- function(link, destpath) {
  # extract filename from link
  m <- regexpr('[^/]*(?=\\.zip$)', link, perl = T)
  filename <- regmatches(link, m)

  # remove trailing filepath separator to destpath if it's there
  destpath <- gsub(paste0(.Platform$file.sep, '$'), '', destpath)

  # download method
  if (.Platform$OS.type == 'windows')
    download_method <- 'wininet'
  else
    download_method <- 'curl'

  # download and unzip to destpath
  temp <- tempfile()
  download.file(link, temp, method = download_method, quiet = T)
  unzip(temp, exdir = destpath)
  unlink(temp)
}

#' Download the Phoenix Dataset
#'
#' Download and unzip all of the data files for the Phoenix dataset from the
#' Phoenix data website into a given directory.
#'
#' @param destpath The path to the directory where Phoenix should go.
#' @param phoenix_version Download a specific version of Phoenix ("v0.1.0" or the current version by default).
#' @param start_date Filter the dataset to only include events from start_date.
#' @param end_date Filter the dataset to only include events before end_date.
#' 
#'
#' @return NULL
#' @author Andy Halterman, Altaf Ali
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' download_phoenix("~/OEDA/phoxy_test/", phoenix_version = "current", start_date = "2017-01-01", end_date = "2017-01-31")
#'
#' @rdname download_phoenix

#' @export
#' @importFrom plyr l_ply progress_text
download_phoenix <- function(destpath, phoenix_version = 'current', start_date = NULL, end_date = NULL){
  links <- get_links(phoenix_version = phoenix_version, start_date = start_date, end_date = end_date)
  message("Downloading and unzipping ", nrow(links), " files")
  plyr::l_ply(links$Link, dw_file, destpath = destpath, .progress = plyr::progress_text(char = '='))
}
