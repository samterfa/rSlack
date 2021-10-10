
#' Lists all users in a Slack team.
#' 
#' Lists all users in a Slack team.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param limit The maximum number of items to return. Fewer than the requested number of items may be returned, even if the end of the list hasn't been reached. Must be an integer no larger than 1000.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return A list of channels
#' @seealso \url{https://api.slack.com/methods/users.list}
#' @family users
#' @export
users_list <- function(token = Sys.getenv("SLACK_TOKEN"), limit = 100, return_response = F){
  
  query <- list(limit = limit)
  
  response <- httr::GET('https://slack.com/api/users.list', query = query,  httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}
