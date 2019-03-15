#' #' Get train station info from transportAPI
#' #'
#' #' R interface to transportAPI
#' #' See [developer.transportapi.com/docs](https://developer.transportapi.com/docs) for details.
#' #'
#' #' @details
#' #' Requires the internet and a transportapi.com API key.
#' #' transportapi.com does not yet work worldwide.
#' #'
#' #' You need to have an api key and app id for this code to run.
#' #' By default it uses the TRANSPORTAPI_app_id and  TRANSPORTAPI_app_key environment variable.
#' #' This can be set with `usethis::edit_r_environ()`.
#'
#' #' @param station_code The station code of the station of interest. Can be CRS or TIPLOC.
#' #' @param live Logical, use the live API
#' #' @param date The date of interest in yyyy-mm-dd format.
#' #' @param time The time of interest in hh:mm format.
#' #' @param called_at Only include services that call at the given station, before calling at the station of interest.
#' #' @param calling_at Only include services that call at the given station, after calling at the station of interest.
#' #' @param origin Only include services originating from the given station.
#' #' @param destination Only include services terminating at the given station.
#' #' @param from_offset Modifies the start of the time window for which services are retrieved. By default, this is coincides with the date/time of interest.
#' #' @param to_offset Modifies the end of the time window for which services are retrieved. By default, this is two hours in the future relative to the date/time of interest.
#' #' @param operator Only include services that are operated by the given operator.
#' #' @param service Only include services that have the given service code.
#' #' @param station_detail Add additional detail for the services returned at certain stations.
#' #' @param train_status Only include services having the specified train status.
#' #' @param type
#' #' @param silent Logical (default is FALSE). TRUE hides request sent.
#' #' @param app_id The app id used. By default this uses `Sys.getenv("TRANSPORTAPI_app_id")`.
#' #' @param app_key The app key used. By default this uses `Sys.getenv("TRANSPORTAPI_app_key")`.
#' #' @param base_url The base url from which to construct API requests
#' #' (with default set to main server)
#' #' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' #' @examples
#'
#' train_station = function(station_code,
#'                    live = FALSE,
#'                    date = NULL,
#'                    time = NULL,
#'                    called_at = NULL,
#'                    calling_at = NULL,
#'                    destination = NULL,
#'                    from_offset = NULL,
#'                    to_offset = NULL,
#'                    operator = NULL,
#'                    origin = NULL,
#'                    service = NULL,
#'                    station_detail = NULL,
#'                    train_status = NULL,
#'                    type = NULL,
#'                    silent = TRUE,
#'                    app_id = Sys.getenv("TRANSPORTAPI_app_id"),
#'                    app_key = Sys.getenv("TRANSPORTAPI_app_key"),
#'                    base_url = "http://transportapi.com/",
#'                    save_raw = FALSE
#' ) {
#'
#'   # Check inputs
#'   checkmate::assert_choice(type,c("arrival","departure","pass"), null.ok = TRUE)
#'   checkmate::assert_subset(station_detail,c("origin", "destination","called_at","calling_at"), null.ok = TRUE)
#'   checkmate::assert_choice(type,c("passenger","freight"), null.ok = TRUE)
#'
#'
#'
#'   if(live){
#'     # Routing by live API
#'     ft_string <- paste("v3/uk/station/",station_code,"/live.json" , sep = "")
#'     if(any(!is.null(date), !is.null(time))){
#'       message("date & time are not valid when using the 'live API', set 'live = FALSE' to use date & time")
#'     }
#'
#'   } else {
#'     # Routing by date/time API
#'     ft_string <- paste("v3/uk/station/",station_code,"/",date,"/",time,"/timetable.json" , sep = "")
#'
#'   }
#'
#'   stop("This code is unfinished")
#'
#'
#' }
