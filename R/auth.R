
#' Verify Request
#' 
#' With the help of signing secrets, your app can more confidently verify whether requests from Slack are authentic.
#' 
#' @param request_timestamp The X-Slack-Request-Timestamp header on the HTTP request.
#' @param request_signature The X-Slack-Signature header on the request.
#' @param request_body_raw The raw request body from the request. Make sure that this request body contains no headers and is not deserialized in any way. Slack only uses the raw request payload.
#' @param signing_secret Your app's \href{https://api.slack.com/authentication/verifying-requests-from-slack#signing_secrets_admin_page}{signing secret} available in the app admin panel under Basic Info.
#' @param version The version number right now is always v0.
#' @return TRUE upon successful verification, otherwise an error is thrown.
#' @seealso \url{https://api.slack.com/authentication/verifying-requests-from-slack}
#' @export
verify_request <- function(request_timestamp, request_signature, request_body_raw, signing_secret, version = 'v0'){
  
  sig <- paste0(version,
                paste(version,
                      request_timestamp,
                      request_body_raw %>% rawToChar(),
                      sep = ':') %>%
                  digest::hmac(key = signing_secret,
                               object = .,
                               algo = 'sha256',
                               serialize = F)
  )
  
  if(abs(as.numeric(Sys.time()) - as.numeric(request_timestamp)) > 60*5){
    stop("Code 425 - Possible Replay Attack")
  }
  
  if(sig != request_signature){
    stop("Code 401 - Authentication required")
  }
  
  T
}