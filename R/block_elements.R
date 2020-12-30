
#' Button Element
#' 
#' An interactive component that inserts a button. The button can be a trigger for anything from opening a simple link to starting a complex workflow. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions
#' 
#' @param text A \code{\link{text_object}} that defines the button's text. Can only be of type: plain_text. Maximum length for the text in this field is 75 characters.
#' @param action_id An identifier for the action triggered when the checkbox group is changed. You can use this when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param url A URL to load in the user's browser when the button is clicked. Maximum length for this field is 3000 characters. If you're using url, you'll still receive an \href{https://api.slack.com/interactivity/handling#payloads}{interaction payload} and will need to send an \href{https://api.slack.com/interactivity/handling#acknowledgment_response}{acknowledgment response}.
#' @param value The value to send along with the interaction payload. Maximum length for this field is 2000 characters.
#' @param style Decorates buttons with alternative visual color schemes. Use this option with restraint. primary gives buttons a green outline and text, ideal for affirmation or confirmation actions. primary should only be used for one button within a set. danger gives buttons a red outline and text, and should be used when the action is destructive. Use danger even more sparingly than primary. If you don't include this field, the default button style will be used.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog after the button is clicked.
#' @return A button element
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#button}
#' @family Elements
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
#' A checkbox group that allows a user to choose multiple items from a list of possible options. Checkboxes are only supported in the following app surfaces: Home tabs, Modals, and Messages. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param action_id An identifier for the action triggered when the checkbox group is changed. You can use this when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s. A maximum of 10 options are allowed.
#' @param initial_options A list of \code{\link{option_object}}s that exactly matches one or more of the options within options. These options will be selected when the checkbox group initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after clicking one of the checkboxes in this element.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#checkboxes}
#' @family Elements
#' @export
checkbox_element <- function(action_id, options, initial_options = NULL, confirm = NULL){
  
  type <- 'checkboxes'
  
  assertthat::assert_that(all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(initial_options) || all(unlist(lapply(initial_options, function(x) inherits(x, 'slack.option.object')))), msg = 'initial_options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.checkbox.element'))
  
  obj
}


#' Date Picker Element
#' 
#' An element which lets users easily select a date from a calendar style UI. To use interactive components like this, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param action_id An identifier for the action triggered when the checkbox group is changed. You can use this when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param placeholder A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the datepicker. Maximum length for the text in this field is 150 characters.
#' @param initial_date The initial date that is selected when the element is loaded. This should be in the format YYYY-MM-DD.
#' @param confirm	A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after a date is selected.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#datepicker}
#' @family Elements
#' @export
datepicker_element <- function(action_id, placeholder = NULL, initial_date = NULL, confirm = NULL){
  
  type <- 'datepicker'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(initial_date) || grepl('^....-..-..$', initial_date), msg = 'initial_date must be of the form YYYY-MM-DD')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.datepicker.element'))
  
  obj
}


#' Image Element
#' 
#' An element to insert an image as part of a larger block of content. If you want a block with only an image in it, you're looking for the image block. Works with block types: Section Context
#' 
#' @param image_url The URL of the image to be displayed.
#' @param alt_text A plain-text summary of the image. This should not contain any markup.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#image}
#' @family Elements
#' @export
image_element <- function(image_url, alt_text){
  
  type <- 'image'
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.image.element'))
  
  obj
}


#' Static Options Multi-Select Menu Element
#' 
#' This is the simplest form of select menu, with a static list of options passed in when defining the element. A multi-select menu allows a user to select multiple items from a list of options. Just like regular select menus, multi-select menus also include type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Input
#' 
#' @param placeholder A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s. Maximum number of options is 100. If option_groups is specified, this field should not be.
#' @param option_groups A list of \code{\link{option_group}} objects. Maximum number of option groups is 100. If options is specified, this field should not be.
#' @param initial_options A list of \code{link{option_object}}s that exactly match one or more of the options within options or option_groups. These options will be selected when the menu initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the multi-select choices are submitted.
#' @param max_selected_items Specifies the maximum number of items that can be selected in the menu. Minimum number is 1.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#static_multi_select_menu}
#' @family Elements
#' @export
static_multi_select_menu <- function(placeholder, action_id, options = NULL, option_groups = NULL, initial_options = NULL, confirm = NULL, max_selected_items = NULL){
  
  type <- 'multi_static_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(options) || all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(option_groups) || all(unlist(lapply(option_groups, function(x) inherits(x, 'slack.option_group.object')))), msg = 'option_groups must be created using option_group()')
  assertthat::assert_that(is.null(initial_options) || all(unlist(lapply(initial_options, function(x) inherits(x, 'slack.option.object')))), msg = 'initial_options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(max_selected_items) || max_selected_items > 0, msg = 'max_selected_items must be numeric and greater than 0')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.multi_static_select.element'))
  
  obj
}

#' External Data Source Multi-Select Menu Element
#' 
#' This menu will load its options from an external data source, allowing for a dynamic list of options. Each time a select menu of this type is opened or the user starts typing in the typeahead field, we'll send a request to your specified URL. Your app should return an HTTP 200 OK response, along with an application/json post body with an object containing either an \code{\link{option_object}} list, or an \code{\link{option_groups}} list. A multi-select menu allows a user to select multiple items from a list of options. Just like regular select menus, multi-select menus also include type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param min_query_length When the typeahead field is used, a request will be sent on every character change. If you prefer fewer requests or more fully ideated queries, use the min_query_length attribute to tell Slack the fewest number of typed characters required before dispatch. The default value is 3.
#' @param initial_options A list of \code{link{option_object}}s that exactly match one or more of the options within options or \code{\link{option_group}}s. These options will be selected when the menu initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the multi-select choices are submitted.
#' @param max_selected_items Specifies the maximum number of items that can be selected in the menu. Minimum number is 1.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#external_multi_select_menu}
#' @family Elements
#' @export
external_multi_select_menu <- function(placeholder, action_id, min_query_length = NULL, initial_options = NULL, confirm = NULL, max_selected_items = NULL){
  
  type <- 'multi_external_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(min_query_length) || min_query_length > 0)
  assertthat::assert_that(is.null(initial_options) || all(unlist(lapply(initial_options, function(x) inherits(x, 'slack.option.object')))), msg = 'initial_options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(max_selected_items) || max_selected_items > 0, msg = 'max_selected_items must be numeric and greater than 0')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.multi_external_select.element'))
  
  obj
}

#' User List Multi-Select Menu Element
#' 
#' This multi-select menu will populate its options with a list of Slack users visible to the current user in the active workspace. A multi-select menu allows a user to select multiple items from a list of options. Just like regular select menus, multi-select menus also include type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_users A list of user IDs of any valid users to be pre-selected when the menu loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the multi-select choices are submitted.
#' @param max_selected_items Specifies the maximum number of items that can be selected in the menu. Minimum number is 1.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#users_multi_select_menu}
#' @family Elements
#' @export
users_multi_select_menu <- function(placeholder, action_id, initial_users = NULL, confirm = NULL, max_selected_items = NULL){
  
  type <- 'multi_users_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(max_selected_items) || max_selected_items > 0, msg = 'max_selected_items must be numeric and greater than 0')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.multi_users_select.element'))
  
  obj
}

#' Conversations List Multi-Select Menu Element
#' 
#' This multi-select menu will populate its options with a list of public and private channels, DMs, and MPIMs visible to the current user in the active workspace. A multi-select menu allows a user to select multiple items from a list of options. Just like regular select menus, multi-select menus also include type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_conversations A list of one or more IDs of any valid conversations to be pre-selected when the menu loads. If default_to_current_conversation is also supplied, initial_conversations will be ignored.
#' @param default_to_current_conversation Pre-populates the select menu with the conversation that the user was viewing when they opened the modal, if available. Default is false.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the multi-select choices are submitted.
#' @param max_selected_items Specifies the maximum number of items that can be selected in the menu. Minimum number is 1.
#' @param filter A \code{\link{filter_object}} that reduces the list of available conversations using the specified criteria.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#conversation_multi_select_menu}
#' @family Elements
#' @export
conversations_multi_select_menu <- function(placeholder, action_id, initial_conversations = NULL, default_to_current_conversation = NULL, confirm = NULL, max_selected_items = NULL, filter = NULL){
  
  type <- 'multi_conversations_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(default_to_current_conversation) || is.logical(default_to_current_conversation))
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(max_selected_items) || max_selected_items > 0, msg = 'max_selected_items must be numeric and greater than 0')
  assertthat::assert_that(is.null(filter) || inherits(filter, 'slack.filter.object'))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.multi_conversations_select.element'))
  
  obj
}


#' Public Channels List Multi-Select Menu Element
#' 
#' This multi-select menu will populate its options with a list of public channels visible to the current user in the active workspace. A multi-select menu allows a user to select multiple items from a list of options. Just like regular select menus, multi-select menus also include type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_channels A list of one or more IDs of any valid public channel to be pre-selected when the menu loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the multi-select choices are submitted.
#' @param max_selected_items Specifies the maximum number of items that can be selected in the menu. Minimum number is 1.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#channel_multi_select_menu}
#' @family Elements
#' @export
channels_multi_select_menu <- function(placeholder, action_id, initial_channels = NULL, confirm = NULL, max_selected_items = NULL){
  
  type <- 'multi_channels_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(max_selected_items) || max_selected_items > 0, msg = 'max_selected_items must be numeric and greater than 0')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.multi_channels_select.element'))
  
  obj
}


#' Overflow Menu Element
#' 
#' This is like a cross between a button and a select menu - when a user clicks on this overflow button, they will be presented with a list of options to choose from. Unlike the select menu, there is no typeahead field, and the button always appears with an ellipsis ("…") rather than customisable text. As such, it is usually used if you want a more compact layout than a select menu, or to supply a list of less visually important actions after a row of buttons. You can also specify simple URL links as overflow menu options, instead of actions. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions
#' 
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s to display in the menu. Maximum number of options is 5, minimum is 2.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after a menu item is selected.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#overflow}
#' @family Elements
#' @export
overflow_menu_element <- function(action_id, options, confirm = NULL){
  
  type <- 'overflow'
  
  assertthat::assert_that(all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.overflow_menu.element'))
  
  obj
}

#' Plain-Text Input Element
#' 
#' A plain-text input, similar to the HTML `<input>` tag, creates a field where a user can enter freeform data. It can appear as a single-line field or a larger textarea using the multiline flag. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Input. Plain-text input elements are supported in the following \href{https://api.slack.com/surfaces}{app surfaces}: Home tabs Modals
#' 
#' @param action_id An identifier for the input value when the parent modal is submitted. You can use this when you receive a view_submission payload to \href{https://api.slack.com/surfaces/modals/using#handling-submissions}{identify the value of the input element}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param initial_value The initial value in the plain-text input when it is loaded.
#' @param multiline	Indicates whether the input will be a single line (false) or a larger textarea (true). Defaults to false.
#' @param min_Length The minimum length of input that the user must provide. If the user provides less, they will receive an error. Maximum value is 3000.
#' @param max_length The maximum length of input that the user can provide. If the user provides more, they will receive an error.
#' @param dispatch_action_config A \code{\link{dispatch_action_configuration_object}} that determines when during text input the element returns a \href{https://api.slack.com/reference/interaction-payloads/block-actions}{block_actions payload}.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#input}
#' @family Elements
#' @export
plain_text_input_element <- function(action_id, placeholder = NULL, multiline = NULL, min_length = NULL, max_length = NULL, dispatch_action_config = NULL){
  
  type <- 'plain_text_input'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(is.null(placeholder) || inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(multiline) || is.logical(multiline))
  assertthat::assert_that(is.null(min_length) || (min_length > 0 & min_length <= 3000))
  assertthat::assert_that(is.null(dispatch_action_config) || inherits(dispatch_action_config, 'slack.dispatch_action_configuration.object'))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.plain_text_input.element'))
  
  obj 
}


#' Radio Button Group Element
#' 
#' A radio button group that allows a user to choose one item from a list of possible options. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Radio buttons are supported in the following app surfaces: Home tabs Modals Messages. Works with block types: Section Actions Input
#' 
#' @param action_id An identifier for the action triggered when the checkbox group is changed. You can use this when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s. A maximum of 10 options are allowed.
#' @param initital_option An \code{\link{option_object}} that exactly matches one of the options within options. This option will be selected when the radio button group initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after clicking one of the radio buttons in this element.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#radio}
#' @family Elements
#' @export
radio_button_group_element <- function(action_id, options, initial_option = NULL, confirm = NULL){
  
  type <- 'radio_buttons'
  
  assertthat::assert_that(all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(initial_option) || inherits(initial_option, 'slack.option.object'), msg = 'initial_option must be created using option_object()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.radio_buttons.element'))
  
  obj 
}



#' Static Options Select Menu Element
#' 
#' This is the simplest form of select menu, with a static list of options passed in when defining the element. A select menu, just as with a standard HTML `<select>` tag, creates a drop down menu with a list of options for a user to choose. The select menu also includes type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param placeholder A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param options A list of \code{\link{option_object}}s. Maximum number of options is 100. If option_groups is specified, this field should not be.
#' @param option_groups A list of \code{\link{option_group}} objects. Maximum number of option groups is 100. If options is specified, this field should not be.
#' @param initial_option A single \code{link{option_object}}s that exactly match one or more of the options within option_objects or option_groups. These options will be selected when the menu initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears after a menu item is selected.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#static_select}
#' @family Elements
#' @export
static_select_menu <- function(placeholder, action_id, options = NULL, option_groups = NULL, initial_option = NULL, confirm = NULL){
  
  type <- 'static_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(options) || all(unlist(lapply(options, function(x) inherits(x, 'slack.option.object')))), msg = 'options must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(option_groups) || all(unlist(lapply(option_groups, function(x) inherits(x, 'slack.option_group.object')))), msg = 'option_groups must be created using option_group()')
  assertthat::assert_that(is.null(initial_option) || inherits(initial_option, 'slack.option.object'), msg = 'initial_option must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(options) | is.null(option_groups), msg = 'Only one of options or option_groups should be specified.')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.static_select.element'))
  
  obj
}

#' External Data Source Select Menu Element
#' 
#' This menu will load its options from an external data source, allowing for a dynamic list of options. Each time a select menu of this type is opened or the user starts typing in the typeahead field, we'll send a request to your specified URL. Your app should return an HTTP 200 OK response, along with an application/json post body with an object containing either an \code{\link{option_object}} list, or an \code{\link{option_groups}} list. A select menu, just as with a standard HTML `<select>` tag, creates a drop down menu with a list of options for a user to choose. The select menu also includes type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param min_query_length When the typeahead field is used, a request will be sent on every character change. If you prefer fewer requests or more fully ideated queries, use the min_query_length attribute to tell Slack the fewest number of typed characters required before dispatch. The default value is 3.
#' @param initial_option A single \code{link{option_object}} that exactly match one or more of the options within options or option_groups. This option will be selected when the menu initially loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the select choices are submitted.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#external_select}
#' @family Elements
#' @export
external_select_menu <- function(placeholder, action_id, min_query_length = NULL, initial_option = NULL, confirm = NULL){
  
  type <- 'external_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(min_query_length) || min_query_length > 0)
  assertthat::assert_that(is.null(initial_option) || inherits(initial_option, 'slack.option.object'), msg = 'initial_option must be created using option_object() or option_object_list()')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.external_select.element'))
  
  obj
}

#' User List Select Menu Element
#' 
#' This select menu will populate its options with a list of Slack users visible to the current user in the active workspace. A select menu, just as with a standard HTML `<select>` tag, creates a drop down menu with a list of options for a user to choose. The select menu also includes type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_user The user ID of any valid user to be pre-selected when the menu loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the select choices are submitted.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#users_multi_select}
#' @family Elements
#' @export
users_select_menu <- function(placeholder, action_id, initial_user = NULL, confirm = NULL){
  
  type <- 'users_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.users_select.element'))
  
  obj
}

#' Conversations List Select Menu Element
#' 
#' This select menu will populate its options with a list of public and private channels, DMs, and MPIMs visible to the current user in the active workspace. A select menu, just as with a standard HTML `<select>` tag, creates a drop down menu with a list of options for a user to choose. The select menu also includes type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_conversation A list of one or more IDs of any valid conversations to be pre-selected when the menu loads. If default_to_current_conversation is also supplied, initial_conversation will take precedence.
#' @param default_to_current_conversation Pre-populates the select menu with the conversation that the user was viewing when they opened the modal, if available. Default is false.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the select choices are submitted.
#' @param response_url_enabled This field only works with menus in \code{\link{input_block}}s in \href{https://api.slack.com/surfaces/modals}{modals}. When set to true, the \href{https://api.slack.com/reference/interaction-payloads/views#view_submission}{view_submission payload} from the menu's parent view will contain a response_url. This response_url can be used for \href{https://api.slack.com/interactivity/handling#message_responses}{message responses}. The target conversation for the message will be determined by the value of this select menu.
#' @param filter A \code{\link{filter_object}} that reduces the list of available conversations using the specified criteria.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#conversation_select}
#' @family Elements
#' @export
conversations_select_menu <- function(placeholder, action_id, initial_conversations = NULL, default_to_current_conversation = NULL, confirm = NULL, response_url_enabled = NULL, filter = NULL){
  
  type <- 'conversations_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(default_to_current_conversation) || is.logical(default_to_current_conversation), msg = 'default_to_current_conversation must be logical')
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(response_url_enabled) || is.logical(response_url_enabled), msg = 'response_url_enabled must be logical')
  assertthat::assert_that(is.null(filter) || inherits(filter, 'slack.filter.object'))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.conversations_select.element'))
  
  obj
}


#' Public Channels List Select Menu Element
#' 
#' This multi-select menu will populate its options with a list of public channels visible to the current user in the active workspace. A select menu, just as with a standard HTML `<select>` tag, creates a drop down menu with a list of options for a user to choose. The select menu also includes type-ahead functionality, where a user can type a part or all of an option string to filter the list. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the menu. Maximum length for the text in this field is 150 characters.
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param initial_channel The ID of any valid public channel to be pre-selected when the menu loads.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the select choices are submitted.
#' @param response_url_enabled This field only works with menus in \code{\link{input_block}}s in \href{https://api.slack.com/surfaces/modals}{modals}. When set to true, the \href{https://api.slack.com/reference/interaction-payloads/views#view_submission}{view_submission payload} from the menu's parent view will contain a response_url. This response_url can be used for \href{https://api.slack.com/interactivity/handling#message_responses}{message responses}. The target channel for the message will be determined by the value of this select menu.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#channel_select}
#' @family Elements
#' @export
channels_select_menu <- function(placeholder, action_id, initial_channel = NULL, confirm = NULL){
  
  type <- 'channels_select'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(inherits(placeholder, 'slack.text.object'))
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.channels_select.element'))
  
  obj
}



#' Time Picker Element
#' 
#' An element which allows selection of a time of day. On desktop clients, this time picker will take the form of a dropdown list with free-text entry for precise choices. On mobile clients, the time picker will use native time picker UIs. To use interactive components, you will need to make some changes to prepare your app. Read our \href{https://api.slack.com/interactivity/handling}{guide to enabling interactivity}. Works with block types: Section Actions Input
#' 
#' @param action_id An identifier for the action triggered when a menu option is selected. You can use this when you receive an interaction payload to \href{https://api.slack.com/messaging/interactivity/enabling#understanding_payloads}{identify the source of the action}. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param placeholder 	A plain_text only \code{\link{text_object}} that defines the placeholder text shown on the timepicker Maximum length for the text in this field is 150 characters.
#' @param initial_time The initial time that is selected when the element is loaded. This should be in the format HH:mm, where HH is the 24-hour format of an hour (00 to 23) and mm is minutes with leading zeros (00 to 59), for example 22:25 for 10:25pm.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog that appears before the select choices are submitted.
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#timepicker}
#' @family Elements
#' @export
timepicker_element <- function(action_id, placeholder = NULL, initial_time = NULL, confirm = NULL){
  
  type <- 'timepicker'
  
  if(!inherits(placeholder, 'slack.text.object')) placeholder <- text_object(type = 'plain_text', text = placeholder)
  
  assertthat::assert_that(is.null(confirm) || inherits(confirm, 'slack.confirm.object'), msg = 'confirm must be created with confirm_object()')
  assertthat::assert_that(is.null(initial_time) || initial_time %>% grepl(pattern = '^\\d\\d:\\d\\d$'), msg = 'intial_time must be of the form HH:mm')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.element', 'slack.timepicker.element'))
  
  obj
}







