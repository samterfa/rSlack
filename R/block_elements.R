
#' Create a Button Element
#' 
#' An interactive component that inserts a button. The button can be a trigger for anything from opening a simple link to starting a complex workflow.
#' 
#' @param type 	The type of element. In this case type is always button.
#' @param text A \code{\link{text_object}} that defines the button's text. Can only be of type: plain_text. Maximum length for the text in this field is 75 characters.
#' @param action_id An identifier for this action. You can use this when you receive an interaction payload to identify the source of the action. Should be unique among all other action_ids in the containing block. Maximum length for this field is 255 characters.
#' @param url A URL to load in the user's browser when the button is clicked. Maximum length for this field is 3000 characters. If you're using url, you'll still receive an \href{https://api.slack.com/interactivity/handling#payloads}{interaction payload} and will need to send an \href{https://api.slack.com/interactivity/handling#acknowledgment_response}{acknowledgment response}.
#' @param value The value to send along with the interaction payload. Maximum length for this field is 2000 characters.
#' @param style Decorates buttons with alternative visual color schemes. Use this option with restraint.primary gives buttons a green outline and text, ideal for affirmation or confirmation actions. primary should only be used for one button within a set. danger gives buttons a red outline and text, and should be used when the action is destructive. Use danger even more sparingly than primary. If you don't include this field, the default button style will be used.
#' @param confirm A \code{\link{confirm_object}} that defines an optional confirmation dialog after the button is clicked.
#' @return A button element
#' @seealso \url{https://api.slack.com/reference/block-kit/block-elements#button}
#' @export
button_element <- function(text, action_id = NULL, url = NULL, value = NULL, style = NULL, confirm = NULL, type = 'button'){
  
  assertthat::assert_that(type == 'button', 
                          'slack.text.object' %in% class(text))
  
  obj <- as.list(environment())
  class(obj) <- append(class(obj), 'slack.block.element')
  
  obj
}