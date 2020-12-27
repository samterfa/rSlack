
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
#' @export
text_object <- function(type, text, emoji = NULL, verbatim = NULL){
  
  assertthat::assert_that(type %in% c('plain_text', 'mrkdwn'))
  
  obj <- as.list(environment())
  class(obj) <- append(class(obj), 'slack.text.object')
  
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
#' @export
confirm_object <- function(title, text, confirm, deny, style = NULL){
  
  assertthat::assert_that('slack.text.object' %in% class(title),
                          'slack.text.object' %in% class(text),
                          'slack.text.object' %in% class(confirm),
                          'slack.text.object' %in% class(deny))
  
  obj <- as.list(environment())
  class(obj) <- append(class(obj), 'slack.confirm.object')
  
  obj
}