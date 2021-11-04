
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

#' Sends a message to a channel.
#'
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param channel Channel, private group, or IM channel to send message to. Can be an encoded ID, or a name. See below for more details.
#' @param as_user Pass true to post the message as the authed user, instead of as a bot. Defaults to false. See authorship below.
#' @param message A message object.
#' @param attachments A JSON-based array of structured attachments, presented as a URL-encoded string.
#' @param blocks A JSON-based array of structured blocks, presented as a URL-encoded string.
#' @param container_id 
#' @param file_annotation 
#' @param icon_emoji Emoji to use as the icon for this message. Overrides icon_url. Must be used in conjunction with as_user set to false, otherwise ignored. See authorship below.
#' @param icon_url URL to an image to use as the icon for this message. Must be used in conjunction with as_user set to false, otherwise ignored. See authorship below.
#' @param link_names Find and link channel names and usernames.
#' @param mrkdwn Disable Slack markup parsing by setting to false. Enabled by default. Default: true
#' @param parse Change how messages are treated. Defaults to none. See below.
#' @param reply_broadcast Used in conjunction with thread_ts and indicates whether reply should be made visible to everyone in the channel or conversation. Defaults to false.
#' @param text How this field works and whether it is required depends on other fields you use in your API call. See below for more detail.
#' @param thread_ts Provide another message's ts value to make this message a reply. Avoid using a reply's ts value; use its parent instead.
#' @param unfurl_links Pass true to enable unfurling of primarily text-based content.
#' @param unfurl_media Pass false to disable unfurling of media content.
#' @param username Set your bot's user name. Must be used in conjunction with as_user set to false, otherwise ignored. See authorship below.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @section Details:
#' This method supports application/json via HTTP POST. Present your token in your request's Authorization header. Learn more.
#'
#' @export
post_message <- function(token = Sys.getenv("SLACK_TOKEN"), channel, message = NULL, as_user = NULL, attachments = NULL, blocks = NULL, icon_emoji = NULL, icon_url = NULL, link_names = NULL, mrkdwn = NULL, parse = NULL, reply_broadcast = NULL, text = NULL, thread_ts = NULL, unfurl_links = NULL, unfurl_media = NULL, username = NULL, return_response = F){
  
  if(!is.null(message)){
    body <- message
    body$channel <- channel
  }else{
    body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap(), return_response = purrr::zap()) %>% purrr::compact()
  }
  
  response <- httr::POST('https://slack.com/api/chat.postMessage', body = body, encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}

