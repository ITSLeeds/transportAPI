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
#' @param type Type of routing can be car, cycle, public (DEFAULT)
#' @param modes Restricts the transport modes which can be used for routing to only the ones provided by this parameter.
#' @param not_modes Restricts the transport modes which can be used for routing to all modes except the ones provided by this parameter.
#' @param service Specifies the which backend system journey plans should be requested from ('region' param is an alias now deprecated)
#' @param silent Logical (default is FALSE). TRUE hides request sent.
#' @param app_id The app id used. By default this uses `Sys.getenv("TRANSPORTAPI_app_id")`.
#' @param app_key The app key used. By default this uses `Sys.getenv("TRANSPORTAPI_app_key")`.
#' @param base_url The base url from which to construct API requests
#' (with default set to main server)
#' @param reporterrors Boolean value (TRUE/FALSE) indicating if cyclestreets (TRUE by default).
#' should report errors (FALSE by default).
#' @param save_raw Boolean value which returns raw list from the json if TRUE (FALSE by default).
#' @inheritParams json2sf_cs
#' @seealso json2sf_tapi
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' sf:::plot.sf(r1)
#' to = c(-2, 53.5) # towards manchester
#' r1 = journey(from, to)
#' r2 = journey(from, to, plan = "balanced")
#' plot(r1["busynance"], reset = FALSE)
#' plot(r2["busynance"], add = TRUE)
#' r3 = journey(from, to, silent = FALSE)
#' r4 = journey(from, to, save_raw = TRUE)
#' r5 = journey(from, to, cols = NULL)
#' }
journey <- function(from, to,
                    type = "public",
                    modes = NULL,
                    not_modes = NULL,
                    service = NULL,
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

  ft_string <- paste("v3/uk/",type,"/journey/from/lonlat:", orig , "/to/lonlat:" , dest, ".json" , sep = "")

  #Select Routing API
  if(type == "public"){
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
  }else if(type == "car"){
    httrmsg = httr::modify_url(
      base_url,
      path = ft_string,
      query = list(
        app_id = app_id,
        app_key = app_key
      )
    )
  }else if(type == "cycle"){
    httrmsg = httr::modify_url(
      base_url,
      path = ft_string,
      query = list(
        app_id = app_id,
        app_key = app_key
      )
    )
  }else{
    stop("Error: Invalid routing type, use 'car','public', or 'cycle'")
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

  if(!save_raw) {
    obj = json2sf_tapi(obj,type)
  }
  return(obj)
}


#' Convert output from transportapi.com into sf object
#'
#' @param obj Object from transportapi.com read-in with
#' @param cols Columns to be included in the result, a character vector or `NULL` for all available columns (see details for default)
#' @export
#' @examples
#' from = "Leeds Rail Station"
#' to = "University of Leeds"
#' # save result from the API call to journey.json
#' # res_json = stplanr::route_cyclestreet(from, to, silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/journey.json")
#' f = system.file(package = "cyclestreets", "extdata/journey.json")
#' obj = jsonlite::read_json(f, simplifyVector = TRUE)
#' rsf = json2sf_cs(obj)
#' sf:::plot.sf(rsf)
#' json2sf_cs(obj, cols = c("time", "busynance", "elevations"))
json2sf_tapi <- function(obj,type) {
  if(type == "public"){
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

  }else if(type %in% c("car","cycle")){
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
    stop("Error: Invalid routing type, use 'car','public', or 'cycle'")
  }
}
