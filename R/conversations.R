
#' Lists all channels in a Slack team.
#' 
#' Lists all channels in a Slack team.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param limit The maximum number of items to return. Fewer than the requested number of items may be returned, even if the end of the list hasn't been reached. Must be an integer no larger than 1000.
#' @param types Mix and match channel types by providing a comma-separated list of any combination of public_channel, private_channel, mpim, im.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return A list of channels
#' @seealso \url{https://api.slack.com/methods/conversations.list}
#' @family Conversations
#' @export
conversations_list <- function(token = Sys.getenv("SLACK_TOKEN"), limit = 100, types = 'public_channel,private_channel,mpim,im', return_response = F){
  
  query <- list(types = types, limit = limit)
  
  response <- httr::GET('https://slack.com/api/conversations.list', query = query,  httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}

