#' Uploads or creates a file.
#' 
#' Uploads or creates a file.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param channels Comma-separated list of channel names or IDs where the file will be shared.
#' @param content File contents via a POST variable. If omitting this parameter, you must provide a file.
#' @param file File contents via multipart/form-data. If omitting this parameter, you must submit content.
#' @param filename Filename of file.
#' @param filetype A \href{https://api.slack.com/types/file#file_types}{file type} identifier.
#' @param initial_comment The message text introducing the file in specified channels.
#' @param Provide another message's ts value to upload this file as a reply. Never use a reply's ts value; use its parent instead.
#' @param title Title of file.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return The details of the uploaded file.
#' @seealso \url{https://api.slack.com/methods/files.upload}
#' @family Files
#' @export
files_upload <- function(token, channels = NULL, content = NULL, file = NULL, filename = NULL, filetype = NULL, initial_comment = NULL, thread_ts = NULL, title = NULL, return_response = F){
  
  if(!is.null(file) && class(file) != 'form_file') file <- httr::upload_file(file)
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap(), return_response = purrr::zap()) %>% purrr::compact()
 
  response <- httr::POST('https://slack.com/api/files.upload', body = body, httr::add_headers(Authorization = glue::glue('Bearer {token}')))
 
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}
