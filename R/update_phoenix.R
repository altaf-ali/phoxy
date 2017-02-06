#' Update a local directory of Phoenix dataset files with new files from the server
#'
#' Checks the contents of a directory containing Phoenix event data files, checks whether the 
#' server has new events, and downloads them to that directory. (It'll have some version handling ability,
#' too, either from the file names or by reading in the events.)
#'
#' @param destpath The path to download Phoenix into.
#' @param phoenix_version. Download a specific version of Phoenix ("v0.1.0" or "current").
#' @param start_date Filter the dataset to only include events from start_date.
#' @param end_date Filter the dataset to only include events before end_date.
#'
#' @return NULL
#' @author Andy Halterman, Altaf Ali
#' @note This function, like Phoenix, is still in development and may contain errors and change quickly.
#' @examples
#'
#' update_phoenix("~/OEDA/phoxy_test/", phoenix_version = "current")

#' 
#' @export
update_phoenix <- function(destpath, phoenix_version = "current", start_date = NULL, end_date = NULL){
  # pulls all the links from the OEDA Phoenix page
  links <- phoxy:::get_links(phoenix_version = phoenix_version, start_date = start_date, end_date = end_date)  

  downloaded_files <- data.frame(
    Filename = list.files(destpath)
  )
  
  new_files <- links %>%
    mutate(Filename = tools::file_path_sans_ext(basename(Link))) %>%
    anti_join(downloaded_files, by = "Filename")
    
  message("Downloading and unzipping ", nrow(new_files), " files")
  plyr::l_ply(new_files$Link, phoxy:::dw_file, destpath = destpath, .progress = plyr::progress_text(char = '='))
}





