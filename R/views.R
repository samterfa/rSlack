
#' View Object
#' 
#' Views are app-customized visual areas within modals and Home tabs.
#' 
#' @param type The type of view. Set to modal for modals and home for Home tabs. Used for Modals and Home Tabs.
#' @param title The title that appears in the top-left of the modal. Must be a plain_text text element with a max length of 24 characters. Used for Modals.
#' @param blocks A list of blocks that defines the content of the view. Max of 100 blocks. Used for Modals and Home tabs.
#' @param close An optional \code{\link{text_object}} that defines the text displayed in the close button at the bottom-right of the view. Max length of 24 characters. Used for Modals.
#' @param submit An optional \code{\link{text_object}} that defines the text displayed in the submit button at the bottom-right of the view. submit is required when an input block is within the blocks array. Max length of 24 characters. Used for Modals.
#' @param private_metadata An optional string that will be sent to your app in view_submission and block_actions events. Max length of 3000 characters. Used for Modals Home tabs.
#' @param callback_id An identifier to recognize interactions and submissions of this particular view. Don't use this to store sensitive information (use private_metadata instead). Max length of 255 characters. Used for Modals Home tabs.
#' @param clear_on_close When set to true, clicking on the close button will clear all views in a modal and close it. Defaults to false. Used for Modals.
#' @param notify_on_close Indicates whether Slack will send your request URL a view_closed event when a user clicks the close button. Defaults to false. Used for Modals.
#' @param external_id A custom identifier that must be unique for all views on a per-team basis. Used for Modals Home tabs.
#' @return A View Object
#' @seealso \url{https://api.slack.com/reference/surfaces/views}
#' @export
view_object <- function(type = c('modal', 'home')[[1]], title, blocks, close = NULL, submit = NULL, private_metadata = NULL, callback_id = NULL, clear_on_close = NULL, notify_on_close = NULL, external_id = NULL){
  
  assertthat::assert_that(type %in% c('modal', 'home'), 
                          'slack.text.object' %in% class(title))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), 'slack.view.object')
  
  obj
}


#' Open a View
#' 
#' Open a view for a user.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param trigger_id Exchange a trigger to post to the user.
#' @param view A \code{\link{view_object}}. See \url{https://api.slack.com/reference/surfaces/views} for more details.
#' @return A \code{\link{view_object}} with status or an error message.
#' @seealso \url{https://api.slack.com/methods/views.open}
#' @export
views_open <- function(token, trigger_id, view, return_response = F){

  assertthat::assert_that('slack.view.object' %in% class(view))
  assertthat::assert_that(all(unlist(lapply(view$blocks, function(x) 'slack.block.object' %in% class(x)))), msg = 'blocks must be of class slack.block.object')
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap()) %>% purrr::compact()
  print(body)
 
  response <- httr::POST('https://slack.com/api/views.open', body = body %>% jsonlite::toJSON(auto_unbox = T), encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}
