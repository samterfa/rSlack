
#' Button Element
#' 
#' An interactive component that inserts a button. The button can be a trigger for anything from opening a simple link to starting a complex workflow.
#' 
#' @param text A \code{\link{text_object}} that defines the button's text. Can only be of type: plain_text. Maximum length for the text in this field is 75 characters.
#' @param action_id An identifier for this action. You can use this when you receive an interaction payload to identify the source of the action. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param url A URL to load in the user's browser when the button is clicked. Maximum length for this field is 3000 characters. If you're using url, you'll still receive an \href{https://api.slack.com/interactivity/handling#payloads}{interaction payload} and will need to send an \href{https://api.slack.com/interactivity/handling#acknowledgment_response}{acknowledgment response}.
#' @param value The value to send along with the interaction payload. Maximum length for this field is 2000 characters.
#' @param style Decorates buttons with alternative visual color schemes. Use this option with restraint. primary gives buttons a green outline and text, ideal for affirmation or confirmation actions. primary should only be used for one button within a set. danger gives buttons a red outline and text, and should be used when the action is destructive. Use danger even more sparingly than primary. If you don't include this field, the default button style will be used.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog after the button is clicked.
#' @return A button element
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#button}
#' @export
button_element <- function(text, action_id = NULL, url = NULL, value = NULL, style = NULL, confirm = NULL){
  
  type <- 'button'
  
  if(is.character(text)) text <- text_object(type = 'plain_text', text = text)
  
  assertthat::assert_that(inherits(text, 'slack.text.object'), msg = 'text must be of class slack.text.object')
  assertthat::assert_that(is.null(style) || style %in% c('danger', 'primary'), msg = "style must be NULL or one of danger or primary")
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.button.element'))
  
  obj
}


#' Checkbox Group
#' 
#' A checkbox group that allows a user to choose multiple items from a list of possible options. Checkboxes are only supported in the following app surfaces: Home tabs, Modals, and Messages.
#' 
#' @param action_id An identifier for the action triggered when the checkbox group is changed. You can use this when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s. A maximum of 10 options are allowed.
#' @param initial_options A list of \code{\link{option_object}}s that exactly matches one or more of the options within options. These options will be selected when the checkbox group initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after clicking one of the checkboxes in this element.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#checkboxes}
#' @export
checkbox_element <- function(action_id, options, initial_options = NULL, confirm = NULL){
  
  type <- 'checkboxes'
  
  assertthat::assert_that(all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object()')
  assertthat::assert_that(is.null(initial_options) || all(unlist(lapply(initial_options, function(x) inherits(x, 'slack.option.object')))), msg = 'initial_options must be created using option_object()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.checkbox.element'))
  
  obj
}


#' Date Picker Element
#' 
#' An element which lets users easily select a date from a calendar style UI. To use interactive components like this, you will need to make some changes to prepare your app. Read our guide to enabling interactivity.
#' 
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to identify the source of the action. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param placeholder A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the datepicker. Maximum length for the text in this field is 150 characters.
#' @param initial_date The initial date that is selected when the element is loaded. This should be in the format YYYY-MM-DD.
#' @param confirm	A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after a date is selected.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#datepicker}
#' @export
datepicker_element <- function(action_id, placeholder = NULL, initial_date = NULL, confirm = NULL){
  
  type <- 'datepicker'
  
  if(is.character(placeholder)) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object()')
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(initial_date) || grepl('^....-..-..$', initial_date), msg = 'initial_date must be of the form YYYY-MM-DD')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.datepicker.element'))
  
  obj
}

