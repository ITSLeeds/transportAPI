#' Plan a journey with transportAPI
#'
#' R interface to the transportAPI journey planning API,
#' a route planner made by cyclists for cyclists.
#' See [developer.transportapi.com/docs](https://developer.transportapi.com/docs) for details.
#'
#' @details
#' Requires the internet and a transportapi.com API key.
#' transportapi.com does not yet work worldwide.
#'
#' You need to have an api key and app id for this code to run.
#' By default it uses the TRANSPORTAPI_app_id and  TRANSPORTAPI_app_key environment variable.
#' This can be set with `usethis::edit_r_environ()`.
#'
#'
#' @param from Longitude/Latitude pair, e.g. `c(-0.134649,51.529258)`
#' @param to Longitude/Latitude pair, e.g. `c(-0.088780,51.506383)`
#' @param apitype Type of routing can be car, cycle, public (DEFAULT)
#' @param modes (apitype = "public" only) Restricts the transport modes which can be used for routing to only the ones provided by this parameter.
#' @param not_modes (apitype = "public" only) Restricts the transport modes which can be used for routing to all modes except the ones provided by this parameter.
#' @param service (apitype = "public" only) Specifies the which backend system journey plans should be requested from ('region' param is an alias now deprecated)
#' @param date (apitype = "public" only) Specifies the date of travel.
#' @param time (apitype = "public" only) Specifies the time of travel.
#' @param type (apitype = "public" only) if 'at', then look for the next journey that departs at or after the specified date/time if 'by', then look for the last journey that arrives by or before the specified date/time
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param app_id The app id used. By default this uses `Sys.getenv("TRANSPORTAPI_app_id")`.
#' @param app_key The app key used. By default this uses `Sys.getenv("TRANSPORTAPI_app_key")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' @inheritParams json2sf_tapi
#' @seealso json2sf_tapi
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' r2 = journey(from, to, apitype = "car")
#' }
journey <- function(from, to,
                    apitype = "public",
                    modes = NULL,
                    not_modes = NULL,
                    service = NULL,
                    date = NULL,
                    time = NULL,
                    type = NULL,
                    silent = TRUE,
                    app_id = NULL,
                    app_key = NULL,
                    base_url = "http://transportapi.com/",
                    save_raw = FALSE
                    ) {

  if(is.null(app_id)) app_id = Sys.getenv("TRANSPORTAPI_app_id")
  if(is.null(app_key)) app_key = Sys.getenv("TRANSPORTAPI_app_key")
  orig <- paste0(from, collapse = ",")
  dest <- paste0(to, collapse = ",")

  if(!is.null(date) & !is.null(time) & !is.null(type) & apitype == "public"){
    # Routing by public transport at a specific time
    ft_string <- paste("v3/uk/",apitype,"/journey/from/lonlat:",orig,"/to/lonlat:",dest,"/",type,"/",date,"/",time,".json" , sep = "")


  }else if(is.null(date) & is.null(time) & is.null(type)){
    # Not using the time variaibles
    ft_string <- paste("v3/uk/",apitype,"/journey/from/lonlat:", orig , "/to/lonlat:" , dest, ".json" , sep = "")

  }else{
    stop("Error: When using date/time variaibles either specify date & time & type & apitype == 'public' or leave as NULL for immediate departure")
  }



  #Select Routing API
  if(apitype == "public"){
    httrmsg = httr::modify_url(
      base_url,
      path = ft_string,
      query = list(
        app_id = app_id,
        app_key = app_key,
        modes = modes,
        not_modes = not_modes,
        service = service
      )
    )
  }else if(apitype == "car"){
    httrmsg = httr::modify_url(
      base_url,
      path = ft_string,
      query = list(
        app_id = app_id,
        app_key = app_key
      )
    )
  }else if(apitype == "cycle"){
    httrmsg = httr::modify_url(
      base_url,
      path = ft_string,
      query = list(
        app_id = app_id,
        app_key = app_key
      )
    )
  }else{
    stop("Error: Invalid routing apitype, use 'car','public', or 'cycle'")
  }

  if (silent == FALSE) {
    print(paste0("The request sent to transportapi.com was: ", httrmsg))
  }

  httrreq <- httr::GET(httrmsg)

  if (grepl('application/json', httrreq$headers$`content-type`) == FALSE) {
    stop("Error: transportapi.com did not return a valid result")
  }

  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: transportapi.com did not return a valid result")
  }

  obj <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

  if (is.element("error", names(obj))) {
    stop(paste0("Error: ", obj$error))
  }

  if(length(obj$routes) == 0){
    warning("Error: transportapi.com was unable to find a route")
    return(NA)
  }else{
    if(!save_raw) {
      obj = json2sf_tapi(obj,apitype)
    }
    return(obj)
  }



}


#' Convert output from transportapi.com into sf object
#'
#' @param obj Object from transportapi.com read-in with
#' @param cols Columns to be included in the result, a character vector or `NULL` for all available columns (see details for default)
#' @export
#' @examples
#' None
#'
json2sf_tapi <- function(obj,apitype) {
  if(apitype == "public"){
    # Extract routes
    routes = obj$routes
    route_parts = routes$route_parts
    routes$route_parts = NULL
    names(routes) = paste0("route_",names(routes))

    names(route_parts) = 1:length(route_parts) # Name each route option
    route_parts = do.call("rbind",route_parts) # bind into a single df
    geometry = lapply(route_parts$coordinates, sf::st_linestring) %>% # convert to sf
      sf::st_sfc()
    route_parts$geometry = geometry #add to df
    route_parts$coordinates = NULL #remove old coordiantes
    route_parts$route_option = as.integer(sub('\\..*', '', rownames(route_parts)))
    route_parts$route_stage = as.integer(sub('.*\\.', '', rownames(route_parts)))

    # Add in global values

    routes$request_time = obj$request_time
    routes$source = obj$source
    routes$acknowledgements = obj$acknowledgements
    #routes$route_option = as.integer(rownames(routes))

    routes = routes[route_parts$route_option,]
    routes = cbind(routes,route_parts)

    routes = sf::st_as_sf(routes)
    st_crs(routes) = 4326

    return(routes)

  }else if(apitype %in% c("car","cycle")){
    # Extract routes
    routes = obj$routes
    geometry = lapply(routes$coordinates, sf::st_linestring) %>% # convert to sf
      sf::st_sfc()
    routes$geometry = geometry #add to df
    routes$coordinates = NULL #remove old coordiantes
    routes$request_time = obj$request_time
    routes$source = obj$source
    routes$acknowledgements = obj$acknowledgements

    routes = sf::st_as_sf(routes)
    st_crs(routes) = 4326

    return(routes)
  }else{
    stop("Error: Invalid routing apitype, use 'car','public', or 'cycle'")
  }
}
