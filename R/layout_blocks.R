
#' Action Block
#' 
#' A block that is used to hold \href{https://api.slack.com/reference/block-kit/block-elements}{interactive elements}.
#' 
#' @param elements A list of interactive element objects - buttons, select menus, overflow menus, or date pickers. There is a maximum of 5 elements in each action block.
#' @param block_id A string acting as a unique identifier for a block. If not specified, a block_id will be generated. You can use this block_id when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @return A \href{https://api.slack.com/reference/block-kit/blocks#actions}{Slack Action Block}
#' @family Blocks
#' @export
actions_block <- function(elements, block_id = NULL){
  
  type <- 'actions'
  
  assertthat::assert_that(all(unlist(lapply(elements, function(x) inherits(x, 'slack.block.element')))), msg = 'elements must be of class slack.block.element')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.actions.block'))
  
  obj
}

#' Context Block
#' 
#' Displays message context, which can include both images and text.
#' 
#' @param elements A list of \code{\link{image_element}}s and \code{\link{text_object}}s. Maximum number of items is 10.
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @return A \href{https://api.slack.com/reference/block-kit/blocks#context}{Slack Context Block}
#' @family Blocks
#' @export
context_block <- function(elements, block_id = NULL){
  
  type <- 'context'
  
  assertthat::assert_that(all(unlist(lapply(elements, function(x) inherits(x, 'slack.image.element') | inherits(x, 'slack.text.object')))), msg = 'elements must be created with image_element() or a text_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.context.block'))
  
  obj
}


#' Divider Block
#' 
#' A content divider, like an `<hr>`, to split up different blocks inside of a message. The divider block is nice and neat, requiring only a type. Available in surfaces: Modals Messages Home tabs
#' 
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @seealso \url{https://api.slack.com/reference/block-kit/blocks#divider}
#' @family Blocks
#' @export
divider_block <- function(block_id = NULL){
  
  type <- 'divider'
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.divider.block'))
  
  obj
}


#' File Block
#' 
#' Displays a remote file. You can't add this block to app surfaces directly, but it will show up when retrieving messages that contain remote files. If you want to add remote files to messages, \href{https://api.slack.com/messaging/files/remote}{follow our guide}. Appears in surfaces: Messages
#' 
#' @param external_id The external unique ID for this file.
#' @param block_id  A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @seealso \url{https://api.slack.com/reference/block-kit/blocks#file}
#' @family Blocks
#' @export
file_block <- function(external_id, block_id = NULL){
  
  type <- 'file'
  source <- 'remote'
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.file.block'))
  
  obj
}


#' Header Block
#' 
#' A header is a plain-text block that displays in a larger, bold font. Use it to delineate between different groups of content in your app's surfaces. Available in surfaces: Modals Messages Home tabs
#' 
#' @param text The text for the block, in the form of a plain_text \code{\link{text_object}}. Maximum length for the text in this field is 150 characters.
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @seealso \url{https://api.slack.com/reference/block-kit/blocks#header}
#' @family Blocks
#' @export
header_block <- function(text, block_id = NULL){
  
  type <- 'header'
  
  if(is.character(text)) text <- text_object(type = 'plain_text', text = text)
  
  assertthat::assert_that(inherits(text, 'slack.text.object'))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.header.block'))
  
  obj
}


#' Image Block
#' 
#' A simple image block, designed to make those cat photos really pop. Available in surfaces: Modals Messages Home tabs
#' 
#' @param image_url The URL of the image to be displayed. Maximum length for this field is 3000 characters.
#' @param alt_text A plain-text summary of the image. This should not contain any markup. Maximum length for this field is 2000 characters.
#' @param title An optional title for the image in the form of a \code{\link{text_object}} that can only be of type: plain_text. Maximum length for the text in this field is 2000 characters.
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @seealso https://api.slack.com/reference/block-kit/blocks#image
#' @family Blocks
#' @export
image_block <- function(image_url, alt_text, title = NULL, block_id = NULL){
  
  type <- 'image'
  
  if(is.character(title)) title <- text_object(type = 'plain_text', text = title)
  
  assertthat::assert_that(inherits(title, 'slack.text.object'))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.image.block'))
  
  obj
}


#' Input Block
#'
#' A block that collects information from users - it can hold a \code{\link{plain_text_input_element}}, a \code{\link{checkbox_element}}, a \code{\link{radio_button_element}}, a \code{\link{select_menu_element}}, a \code{\link{multi_select_menu_element}}, or a \code{\link{datepicker_element}}. Read our guides to collecting input \href{https://api.slack.com/surfaces/modals#gathering_input}{in modals} or \href{https://api.slack.com/surfaces/tabs/using#gathering_input}{in Home tabs} to learn how input blocks pass information to your app. Available in surfaces: Modals Home tabs
#'
#' @param label A label that appears above an input element in the form of a \code{\link{text_object}} that must have type of plain_text. Maximum length for the text in this field is 2000 characters.
#' @param element A \code{\link{plain_text_input_element}}, \code{\link{checkbox_element}}, \code{\link{radio_button_element}}, \code{\link{select_menu_element}}, \code{\link{multi_select_menu_element}}, or \code{\link{datepicker_element}}.
#' @param dispatch_action A boolean that indicates whether or not the use of elements in this block should dispatch a block_actions payload. Defaults to false.
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @param hint An optional hint that appears below an input element in a lighter grey. It must be a \code{\link{text_object}} with a type of plain_text. Maximum length for the text in this field is 2000 characters.
#' @param optional A boolean that indicates whether the input element may be empty when a user submits the modal. Defaults to false.
#' @seealso https://api.slack.com/reference/block-kit/blocks#input
#' @family Blocks
#' @export
input_block <- function(label, element, dispatch_action = NULL, block_id = NULL, hint = NULL, option = NULL){
  
  type <- 'input'
  
  if(is.character(label)) label <- text_object(type = 'plain_text', text = label)
  if(is.character(hint)) hint <- text_object(type = 'plain_text', text = hint)
  
  assertthat::assert_that(inherits(label, 'slack.text.object'))
  assertthat::assert_that(inherits(element, 'slack.block.element'))
  assertthat::assert_that(inherits(hint, 'slack.text.object'))
  assertthat::assert_that(is.null(dispatch_action) || is.logical(dispatch_action))
  assertthat::assert_that(is.null(optional) || is.logical(optional))
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.input.block'))
  
  obj
}



#' Section Block
#' 
#' A section is one of the most flexible blocks available - it can be used as a simple text block, in combination with text fields, or side-by-side with any of the available \href{https://api.slack.com/reference/messaging/block-elements}{block elements}.
#' 
#' @param text The text for the block, in the form of a \code{\link{text_object}}. Maximum length for the text in this field is 3000 characters. This field is not required if a valid array of fields objects is provided instead.
#' @param block_id A string acting as a unique identifier for a block. If not specified, one will be generated. You can use this block_id when you receive an interaction payload to \href{https://api.slack.com/interactivity/handling#payloads}{identify the source of the action}. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @param fields Required if no text is provided. A list of \code{\link{text_objects}}. Any text objects included with fields will be rendered in a compact format that allows for 2 columns of side-by-side text. Maximum number of items is 10. Maximum length for the text in each item is 2000 characters.
#' @param accessory One of the available \href{https://api.slack.com/reference/messaging/block-elements}{element objects}.
#' @seealso \url{https://api.slack.com/reference/block-kit/blocks#section}
#' @family Blocks
#' @export
section_block <- function(text = NULL, block_id = NULL, fields = NULL, accessory = NULL){
  
  type <- 'section'
  
  if(is.character(text)) text <- text_object(type = 'plain_text', text = text)
  
  assertthat::assert_that(is.null(text) || inherits(text, 'slack.text.object'))
  assertthat::assert_that(is.null(fields) || all(unlist(lapply(fields, function(x) inherits(x, 'slack.text.object')))), msg = 'fields must be created with text_object()')
  assertthat::assert_that(!is.null(text) || !is.null(fields), msg = 'text or fields must be defined')
  assertthat::assert_that(is.null(accessory) || inherits(accessory, 'slack.block.element'), msg = 'An accessory must be a block element.')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), c('slack.block.object', 'slack.section.block'))
  
  obj
}
