
#' Action Block
#' 
#' A block that is used to hold \href{https://api.slack.com/reference/block-kit/block-elements}{interactive elements}.
#' 
#' @param type The type of block. For an actions block, type is always actions.
#' @param elements A list of interactive element objects - buttons, select menus, overflow menus, or date pickers. There is a maximum of 5 elements in each action block.
#' @param block_id A string acting as a unique identifier for a block. If not specified, a block_id will be generated. You can use this block_id when you receive an interaction payload to identify the source of the action. Maximum length for this field is 255 characters. block_id should be unique for each message and each iteration of a message. If a message is updated, use a new block_id.
#' @return A \href{https://api.slack.com/reference/block-kit/blocks#actions}{Slack Action Block}
#' @export
actions_block <- function(elements, block_id = NULL, type = 'actions'){
  
  assertthat::assert_that(type == 'actions')
  assertthat::assert_that(all(unlist(lapply(elements, function(x) 'slack.block.element' %in% class(x)))), msg = 'elements must be of class slack.block.element')
  
  obj <- as.list(environment())
  class(obj) <- append(class(obj), 'slack.block.object')
  
  obj
}