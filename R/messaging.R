
#' Message Object
#' 
#' Views are app-customized visual areas within modals and Home tabs.
#' 
#' @param text The usage of this field changes depending on whether you're using blocks or not. If you are, this is used as a fallback string to display in notifications. If you aren't, this is the main body text of the message. It can be formatted as plain text, or with mrkdwn. This field is not enforced as required when using blocks, however it is highly recommended that you include it as the aforementioned fallback.
#' @param blocks A list of @family{Blocks} in the same format as described in the \href{https://api.slack.com/block-kit/building}{building blocks guide}.
#' @param attachments A list of legacy secondary attachments. We recommend you use blocks instead.
#' @param The ID of another un-threaded message to reply to.
#' @param mrkdwn Determines whether the text field is rendered according to \href{https://api.slack.com/reference/surfaces/formatting#basics}{mrkdwn formatting} or not. Defaults to true.
#' @return A message object
#' @seealso \url{https://api.slack.com/reference/messaging/payload}
#' @family Messaging
#' @export
message_object <- function(text = NULL, blocks = NULL, attachments = NULL, thread_ts = NULL, mrkdown = NULL){
  
  assertthat::assert_that(all(unlist(lapply(blocks, function(x) inherits(x, 'slack.block.object')))), msg = 'blocks must be of class slack.block.object')
  assertthat::assert_that(all(unlist(lapply(attachments, function(x) inherits(x, 'slack.attachment.object')))), msg = 'attachments must be created using attachment_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), 'slack.message.object')
  
  obj
}

#' Message Attachment Object
#'
#' This feature is a legacy part of messaging functionality for Slack apps. We recommend you stick with layout blocks, but if you still want to use attachments, read our caveats.
#'
#' @param author_icon A valid URL that displays a small 16px by 16px image to the left of the author_name text. Will only work if author_name is present. 
#' @param author_link A valid URL that will hyperlink the author_name text. Will only work if author_name is present. 
#' @param author_name Small text used to display the author's name. 
#' @param blocks A list of @family{Blocks} in the same format as described in the \href{https://api.slack.com/block-kit/building}{building blocks guide}.
#' @param color Changes the color of the border on the left side of this attachment from the default gray. Can either be one of good (green), warning (yellow), danger (red), or any hex color code (eg. #439FE0)
#' @param fallback A plain text summary of the attachment used in clients that don't show formatted text (eg. IRC, mobile notifications). 
#' @param fields A list of field objects that get displayed in a table-like way (See the example above). For best results, include no more than 2-3 field objects. 
#' @param footer Some brief text to help contextualize and identify an attachment. Limited to 300 characters, and may be truncated further when displayed to users in environments with limited screen real estate. 
#' @param footer_icon A valid URL to an image file that will be displayed beside the footer text. Will only work if author_name is present. We'll render what you provide at 16px by 16px. It's best to use an image that is similarly sized. 
#' @param image_url A valid URL to an image file that will be displayed at the bottom of the attachment. We support GIF, JPEG, PNG, and BMP formats.Large images will be resized to a maximum width of 360px or a maximum height of 500px, while still maintaining the original aspect ratio. Cannot be used with thumb_url. 
#' @param mrkdwn_in A list of field names that should be formatted by mrkdwn syntax. 
#' @param pretext Text that appears above the message attachment block. It can be formatted as plain text, or with mrkdwn by including it in the mrkdwn_in field. 
#' @param text The main body text of the attachment. It can be formatted as plain text, or with mrkdwn by including it in the mrkdwn_in field. The content will automatically collapse if it contains 700+ characters or 5+ linebreaks, and will display a "Show more..." link to expand the content. 
#' @param thumb_url A valid URL to an image file that will be displayed as a thumbnail on the right side of a message attachment. We currently support the following formats: GIF, JPEG, PNG, and BMP.The thumbnail's longest dimension will be scaled down to 75px while maintaining the aspect ratio of the image. The filesize of the image must also be less than 500 KB.For best results, please use images that are already 75px by 75px. 
#' @param title Large title text near the top of the attachment. 
#' @param title_link A valid URL that turns the title text into a hyperlink. 
#' @param ts An integer Unix timestamp that is used to related your attachment to a specific time. The attachment will display the additional timestamp value as part of the attachment's footer.Your message's timestamp will be displayed in varying ways, depending on how far in the past or future it is, relative to the present. Form factors, like mobile versus desktop may also transform its rendered appearance.
#' @return A message attachment object
#' @seealso \url{https://api.slack.com/reference/messaging/payload}
#' @family Messaging
#' @export
attachment_object <- function(author_icon = NULL, author_link = NULL, author_name = NULL, blocks = NULL, color = NULL, fallback = NULL, fields = NULL, footer = NULL, footer_icon = NULL, image_url = NULL, mrkdwn_in = NULL, pretext = NULL, text = NULL, thumb_url = NULL, title = NULL, title_link = NULL, ts = NULL){
  
  assertthat::assert_that(all(unlist(lapply(blocks, function(x) inherits(x, 'slack.block.object')))), msg = 'blocks must be of class slack.block.object')
  assertthat::assert_that(all(unlist(lapply(fields, function(x) inherits(x, 'slack.attachment_field.object')))), msg = 'attachment fields must be created using attachment_field_object()')
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), 'slack.attachment.object')
  
  obj
}


#' Attachment Field Object
#' 
#' Create an attachment field object
#' 
#' @param title Shown as a bold heading displayed in the field object. It cannot contain markup and will be escaped for you.
#' @param value The text value displayed in the field object. It can be formatted as plain text, or with mrkdwn by using the mrkdwn_in option above.
#' @param short	Indicates whether the field object is short enough to be displayed side-by-side with other field objects. Defaults to false.
#' @return An attachment field object
#' @seealso \url{https://api.slack.com/reference/messaging/attachments#fields}
#' @family Messaging
#' @export
attachment_field_object <- function(title = NULL, value = NULL, short = NULL){
  
  assertthat::assert_that(is.null(short) || is.logical(short), msg = "short must be NULL or logical")
  
  obj <- as.list(environment()) %>% purrr::compact()
  class(obj) <- append(class(obj), 'slack.attachment_field.object')
  
  obj
} 





  