
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
#' @family Views
#' @export
view_object <- function(type, title, blocks, close = NULL, submit = NULL, private_metadata = NULL, callback_id = NULL, clear_on_close = NULL, notify_on_close = NULL, external_id = NULL){
  
  if(is.character(title)) title <- text_object(type = 'plain_text', text = title)
  if(is.character(close)) close <- text_object(type = 'plain_text', text = close)
  if(is.character(title)) submit <- text_object(type = 'plain_text', text = submit)
  
  assertthat::assert_that(type %in% c('modal', 'home'), msg = 'type must be one of modal or home')
  assertthat::assert_that(inherits(title, 'slack.text.object'), msg = 'title must be of type slack.text.object')
  assertthat::assert_that(all(unlist(lapply(view$blocks, function(x) inherits(x, 'slack.block.object')))), msg = 'blocks must be of class slack.block.object')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), 'slack.view.object')
  
  obj
}


#' Open a View for a User
#' 
#' Open a modal with a user by exchanging a trigger_id received from another interaction. See the \href{https://api.slack.com/surfaces/modals}{modals documentation} to learn how to obtain triggers from interactive components.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param trigger_id Exchange a trigger to post to the user.
#' @param view A \code{\link{view_object}}.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return A \code{\link{view_object}} with status or an error message.
#' @seealso \url{https://api.slack.com/methods/views.open}
#' @family Views
#' @export
views_open <- function(token, trigger_id, view, return_response = F){

  assertthat::assert_that(inherits(view, 'slack.view.object'), msg = "view must be created using view_object()")
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap()) %>% purrr::compact()
  
  response <- httr::POST('https://slack.com/api/views.open', body = body %>% jsonlite::toJSON(auto_unbox = T), encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}


#' Update an Existing View
#' 
#' Update a view by passing a new view definition object along with the view_id returned in views.open or the external_id. See the \href{https://api.slack.com/surfaces/modals}{modals documentation} to learn more about updating views and avoiding race conditions with the hash argument.
#' 
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param view A \code{\link{view_object}}.
#' @param external_id A unique identifier of the view set by the developer. Must be unique for all views on a team. Max length of 255 characters. Either view_id or external_id is required.
#' @param hash A string that represents view state to protect against possible race conditions.
#' @param view_id A unique identifier of the view to be updated. Either view_id or external_id is required.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return A Success Response with the Updated payload.
#' @seealso https://api.slack.com/methods/views.update
#' @family Views
#' @export
views_update <- function(token, view, external_id = NULL, hash = NULL, view_id = NULL, return_response = F){
  
  assertthat::assert_that(inherits(view, 'slack.view.object'), msg = "view must be created using view_object()")
  assertthat::assert_that(!is.null(external_id) | !is.null(view_id))
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap()) %>% purrr::compact()
  
  response <- httr::POST('https://slack.com/api/views.update', body = body %>% jsonlite::toJSON(auto_unbox = T), encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}


#' Push a View
#' 
#' Push a new view onto the existing view stack by passing a view object and a valid trigger_id generated from an interaction within the existing modal. The pushed view is added to the top of the stack, so the user will go back to the previous view after they complete or cancel the pushed view. After a modal is opened, the app is limited to pushing 2 additional views. Read the \href{https://api.slack.com/surfaces/modals}{modals documentation} to learn more about the lifecycle and intricacies of views.
#'
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param trigger_id Exchange a trigger to post to the user.
#' @param view A \code{\link{view_object}}.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return If you pass a valid view object along with a valid trigger_id, you'll receive a success response with the view object that was pushed to the stack.
#' @seealso \url{https://api.slack.com/methods/views.push}
#' @family Views
#' @export
views_push <- function(token, trigger_id, view, return_response = F){
  
  assertthat::assert_that(inherits(view, 'slack.view.object'), msg = "view must be created using view_object()")
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap()) %>% purrr::compact()
  
  response <- httr::POST('https://slack.com/api/views.push', body = body %>% jsonlite::toJSON(auto_unbox = T), encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}

#' Publish a View
#' 
#' Create or update the view that comprises an app's Home tab for a specific user.
#'
#' @param token Authentication token bearing required scopes. Tokens should be passed as an HTTP Authorization header or alternatively, as a POST parameter.
#' @param user_id id of the user you want publish a view to.
#' @param view A \code{\link{view_object}}.
#' @param hash A string that represents view state to protect against possible race conditions.
#' @param return_response Whether or not to return the API call response as opposed to the response body. Defaults to FALSE (return response body)
#' @return Assuming your view object was properly formatted, valid, and the user_id was viable, you will receive a success response.
#' @seealso \url{https://api.slack.com/methods/views.publish}
#' @family Views
#' @export
views_publish <- function(token, user_id, view, hash = NULL, return_response = F){
  
  assertthat::assert_that(inherits(view, 'slack.view.object'), msg = "view must be created using view_object()")
  
  body <- as.list(environment()) %>% purrr::list_modify(token = purrr::zap()) %>% purrr::compact()
  
  response <- httr::POST('https://slack.com/api/views.push', body = body %>% jsonlite::toJSON(auto_unbox = T), encode = 'json', httr::content_type_json(), httr::add_headers(Authorization = glue::glue('Bearer {token}')))
  
  if(return_response) return(response)
  
  body <- httr::content(response)
  
  if(!body$ok){
    stop(body$error)
  }
  
  body
}


