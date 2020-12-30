
#' Text object
#' 
#' An object containing some text, formatted either as plain_text or using mrkdwn, our proprietary contribution to the much beloved Markdown standard.
#' 
#' @param type The formatting to use for this text object. Can be one of plain_text or mrkdwn.
#' @param text The text for the block. This field accepts any of the standard text formatting markup when type is mrkdwn.
#' @param emoji Indicates whether emojis in a text field should be escaped into the colon emoji format. This field is only usable when type is plain_text.
#' @param verbatim When set to false (as is default) URLs will be auto-converted into links, conversation names will be link-ified, and certain mentions will be automatically parsed. Using a value of true will skip any preprocessing of this nature, although you can still include manual parsing strings. This field is only usable when type is mrkdwn.
#' @return A Slack Text Object
#' @seealso \url{https://api.slack.com/reference/block-kit/composition-objects#text}
#' @family Composition
#' @export
text_object <- function(type, text, emoji = NULL, verbatim = NULL){
  
  assertthat::assert_that(type %in% c('plain_text', 'mrkdwn'), msg = 'type must be one of "plain_text" or "mrkdwn"')
  assertthat::assert_that(is.null(emoji) || is.logical(emoji), msg = "emoji must be NULL or logical")
  assertthat::assert_that(is.null(verbatim) || is.logical(verbatim), msg = "verbatim must be NULL or logical")
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.text.object'))
  
  obj
}

#' Confirmation dialog object
#' 
#' An object that defines a dialog that provides a confirmation step to any interactive element. This dialog will ask the user to confirm their action by offering a confirm and deny buttons.
#' 
#' @param title A plain_text-only \code{\link{text_object}} that defines the dialog's title. Maximum length for this field is 100 characters.
#' @param text 	A \code{\link{text_object}} that defines the explanatory text that appears in the confirm dialog. Maximum length for the text in this field is 300 characters.
#' @param confirm A plain_text-only \code{\link{text_object}} to define the text of the button that confirms the action. Maximum length for the text in this field is 30 characters.
#' @param deny A plain_text-only \code{\link{text_object}} to define the text of the button that cancels the action. Maximum length for the text in this field is 30 characters.
#' @param style Defines the color scheme applied to the confirm button. A value of danger will display the button with a red background on desktop, or red text on mobile. A value of primary will display the button with a green background on desktop, or blue text on mobile. If this field is not provided, the default value will be primary.
#' @return A Slack Confirmation Object
#' @seealso \url{https://api.slack.com/reference/block-kit/composition-objects#confirm}
#' @family Composition
#' @export
confirm_object <- function(title, text, confirm, deny, style = NULL){
  
  if(is.character(title)) title <- text_object(type = 'plain_text', text = title)
  if(is.character(text)) text <- text_object(type = 'plain_text', text = text)
  if(is.character(confirm)) confirm <- text_object(type = 'plain_text', text = confirm)
  if(is.character(deny)) deny <- text_object(type = 'plain_text', text = deny)
  
  assertthat::assert_that(inherits(title, 'slack.text.object'), msg = "title must be of class slack.text.object")
  assertthat::assert_that(inherits(text, 'slack.text.object'), msg = "title must be of class slack.text.object")
  assertthat::assert_that(inherits(confirm, 'slack.text.object'), msg = "title must be of class slack.text.object")
  assertthat::assert_that(inherits(deny, 'slack.text.object'), msg = "title must be of class slack.text.object")
  assertthat::assert_that(is.null(style) || style %in% c('danger', 'primary'), msg = "style must be NULL or one of danger or primary")
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.confirm.object'))
  
  obj
}


#' Option Object
#' 
#' An object that represents a single selectable item in a select menu, multi-select menu, checkbox group, radio button group, or overflow menu.
#' 
#' @param text A \code{\link{text_object}} that defines the text shown in the option on the menu. Overflow, select, and multi-select menus can only use plain_text objects, while radio buttons and checkboxes can use mrkdwn text objects. Maximum length for the text in this field is 75 characters.
#' @param value The string value that will be passed to your app when this option is chosen. Maximum length for this field is 75 characters.
#' @param description A plain_text only text object that defines a line of descriptive text shown below the text field beside the radio button. Maximum length for the text object within this field is 75 characters.
#' @param url A URL to load in the user's browser when the option is clicked. The url attribute is only available in overflow menus. Maximum length for this field is 3000 characters. If you're using url, you'll still receive an interaction payload and will need to send an acknowledgement response.
#' @seealso https://api.slack.com/reference/block-kit/composition-objects#option
#' @family Composition
#' @export
option_object <- function(text, value, description = NULL, url = NULL){
  
  if(is.character(text)) text <- text_object(type = 'plain_text', text = text)
  
  assertthat::assert_that(inherits(text, 'slack.text.object'), msg = "text must be of class slack.text.object")
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.option.object'))
  
  obj
}

#' Create List of Option Objects
#'
#' Helper functions for making lists of options quickly from a dataframe.
#' 
#' @param df Table-like object containing columns text and value, and optionally text_type, value, description, and url. See \code{\link{option_object}} documentation for details.
#' @family Helpers
#' @return A list of \code{\link{option_object}}s
#' @export
option_object_list <- function(df){
  
  assertthat::assert_that(hasName(df, 'text') & hasName(df, 'value'), msg = 'df must have columns text and value')
  
  if(hasName(df, 'text_type')){
    list(options = purrr::pmap(df, list) %>% purrr::map(~ option_object(text = text_object(type = .x$text_type, text = .x$text), value = .x$value, description = .x$description, url = .x$url)))
  }else{
    list(options = purrr::pmap(df, list) %>% purrr::map(~ option_object(text = .x$text, value = .x$value, description = .x$description, url = .x$url)))
  }
}

#' Options Group Object
#' 
#' Provides a way to group options in a select menu or multi-select menu.
#' 
#' @param label A plain_text only \code{\link{text_object}} that defines the label shown above this group of options. Maximum length for the text in this field is 75 characters.
#' @param options A list of \code{\link{option_objects}} that belong to this specific group. Maximum of 100 items.
#' @seealso https://api.slack.com/reference/block-kit/composition-objects#option_group
#' @family Composition
#' @export
option_group <- function(label, options){
  
  if(is.character(label)) label <- text_object(type = 'plain_text', text = label)
  
  assertthat::assert_that(inherits(label, 'slack.text.object'), msg = "label must be of class slack.text.object")
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.option_group.object'))
  
  obj <- list(option_groups = obj)
  
  obj
}


#' Dispatch Action Configuration
#' 
#' Determines when a \code{\link{plain_text_input_element}} will return a block_actions \href{https://api.slack.com/reference/interaction-payloads/block-actions}{interaction payload}.
#' 
#' @param trigger_actions_on A list of interaction types that you would like to receive a \href{https://api.slack.com/reference/interaction-payloads/block-actions}{block_actions payload} for. Should be one or both of:
#' \itemize{
#' \item{on_enter_pressed - payload is dispatched when user presses the enter key while the input is in focus. Hint text will appear underneath the input explaining to the user to press enter to submit.}
#' \item{on_character_entered - payload is dispatched when a character is entered (or removed) in the input.}
#' }
#' @seealso \url{https://api.slack.com/reference/block-kit/composition-objects#dispatch_action_config}
#' @family Composition
#' @export
dispaction_action_configuration <- function(trigger_actions_on){
  
  assertthat::assert_that(all(trigger_actions_on %in% c('on_enter_pressed', 'on_character_entered')), msg = 'trigger_actions_on must be one or both of on_enter_pressed and on_character_entered')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.option.object'))
  
  obj <- list(dispatch_action_config = obj)
  
  obj
}


#' Filter Object for Conversation Lists
#'
#' Provides a way to filter the list of options in a \code{\link{conversations_select_menu}} or \code{\link{conversations_multi_select_menu}}. Please note that while none of the fields above are individually required, you must supply at least one of these fields.
#' 
#' @param include Indicates which type of conversations should be included in the list. When this field is provided, any conversations that do not match will be excluded. You should provide an array of strings from the following options: im, mpim, private, and public. The array cannot be empty.
#' @param exclude_external_shared_channels Indicates whether to exclude external \href{https://api.slack.com/enterprise/shared-channels}{shared channels} from conversation lists. Defaults to false.
#' @param exclude_bot_users Indicates whether to exclude bot users from conversation lists. Defaults to false.
#' @seealso \url{https://api.slack.com/reference/block-kit/composition-objects#filter_conversations}
#' @family Composition
#' @export
conversation_list_filter <- function(include = NULL, exclude_external_shared_channels = NULL, exclude_bot_users = NULL){
  
  assertthat::assert_that(!is.null(include) | !is.null(exclude_external_shared_channels) | !is.null(exclude_bot_users), msg = 'You must supply at least one of include, exclude_external_shared_channels, or exclude_bot_users.')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.composition.object', 'slack.conversation_filter.object'))
  
  obj <- list(filter = obj)
  
  obj
}



