# environment to store credentials
.dstate <- new.env(parent = emptyenv())

# default cache path for the token RDS file
.default_token_cache <- ".rdrop2-token.rds"


#' Authentication for Dropbox
#'
#' This function authenticates you into Dropbox using OAuth 2.0 via the
#' \pkg{httr2} package. The documentation for the
#' \href{https://www.dropbox.com/developers/documentation}{Dropbox API v2}
#' provides more details.
#'
#' @param new_user Set to \code{TRUE} if you need to switch to a new user
#'   account or flush the existing cached token. Default is \code{FALSE}.
#' @param key Your application key. \code{rdrop2} ships with a default key, but
#'   for production use you should create your own Dropbox app and supply its
#'   credentials.
#' @param secret Your application secret.
#' @param cache Either \code{TRUE} (save token to \code{.rdrop2-token.rds} in
#'   the working directory), \code{FALSE} (do not cache), or a file path string
#'   specifying where to save the token RDS file.
#' @param rdstoken File path to a previously saved RDS token. In non-interactive
#'   (server) environments, create a token on a desktop machine with
#'   \code{drop_auth()}, save it with \code{saveRDS()}, and supply the path
#'   here. See examples.
#'
#' @return The \code{httr2_token} object, invisibly.
#'
#' @import httr2
#' @references \href{https://www.dropbox.com/developers/documentation/http/documentation#authorization}{API documentation}
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   # Open a browser to authenticate (and cache the token)
#'   drop_auth()
#'
#'   # Switch to a new user account
#'   drop_auth(new_user = TRUE)
#'
#'   # Save the token for later re-use
#'   token <- drop_auth()
#'   saveRDS(token, "/path/to/tokenfile.rds")
#'
#'   # Load a previously saved token
#'   drop_auth(rdstoken = "/path/to/tokenfile.rds")
#' }
drop_auth <- function(new_user = FALSE,
                      key = "mmhfsybffdom42w",
                      secret = "l8zeqqqgm1ne5z0",
                      cache = TRUE,
                      rdstoken = NA) {

  # resolve cache path
  cache_path <- if (isTRUE(cache)) {
    .default_token_cache
  } else if (is.character(cache)) {
    cache
  } else {
    NULL
  }

  # load token from explicit RDS path
  if (!isTRUE(new_user) && !is.na(rdstoken)) {
    if (!file.exists(rdstoken)) stop("token file not found")
    .dstate$token      <- readRDS(rdstoken)
    .dstate$cache_path <- rdstoken
    return(invisible(.dstate$token))
  }

  # load token from cache path (unless new_user)
  if (!isTRUE(new_user) && !is.null(cache_path) && file.exists(cache_path)) {
    .dstate$token      <- readRDS(cache_path)
    .dstate$cache_path <- cache_path
    return(invisible(.dstate$token))
  }

  # remove old cache if switching users
  if (isTRUE(new_user) && !is.null(cache_path) && file.exists(cache_path)) {
    message("Removing old cached credentials...")
    file.remove(cache_path)
  }

  # build OAuth2 client
  dropbox_client <- httr2::oauth_client(
    id        = key,
    secret    = secret,
    token_url = "https://api.dropbox.com/oauth2/token",
    name      = "dropbox"
  )

  # run auth-code flow (opens browser); request offline access so that
  # Dropbox returns a refresh_token alongside the short-lived access_token.
  # redirect_uri must match the URI registered for the Dropbox app (port 1410).
  dropbox_token <- httr2::oauth_flow_auth_code(
    client       = dropbox_client,
    auth_url     = "https://www.dropbox.com/oauth2/authorize",
    auth_params  = list(token_access_type = "offline"),
    redirect_uri = "http://localhost:1410/",
    pkce         = FALSE
  )

  if (is.null(dropbox_token$access_token)) {
    stop("Authentication failed: no access token returned. Please try again.")
  }

  # persist to cache
  if (!is.null(cache_path)) {
    saveRDS(dropbox_token, cache_path)
  }

  .dstate$token      <- dropbox_token
  .dstate$client     <- dropbox_client
  .dstate$cache_path <- cache_path
  invisible(.dstate$token)
}


#' Retrieve the Dropbox bearer token string
#'
#' Returns the access token string stored in the rdrop2 environment. If no
#' token is cached, \code{\link{drop_auth}} is called interactively. If the
#' stored token has expired and a refresh token is available, the token is
#' silently refreshed and the cache file is updated.
#'
#' @keywords internal
get_dropbox_token <- function() {
  if (!exists(".dstate") || is.null(.dstate$token)) {
    drop_auth()
  }

  token <- .dstate$token

  # Refresh if the access token has expired and we have a refresh token
  has_refresh  <- !is.null(token$refresh_token)
  has_expiry   <- !is.null(token$expires_at)
  is_expired   <- has_expiry && token$expires_at < Sys.time()

  if (has_refresh && is_expired) {
    client <- if (!is.null(.dstate$client)) {
      .dstate$client
    } else {
      httr2::oauth_client(
        id        = "mmhfsybffdom42w",
        secret    = "l8zeqqqgm1ne5z0",
        token_url = "https://api.dropbox.com/oauth2/token",
        name      = "dropbox"
      )
    }
    token <- httr2::oauth_token_refresh(client, token$refresh_token)
    .dstate$token <- token
    if (!is.null(.dstate$cache_path)) {
      saveRDS(token, .dstate$cache_path)
    }
  }

  token$access_token
}

