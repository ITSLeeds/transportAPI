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
#' @param from Longitude/Latitude pair, e.g. `c(-0.134649,51.529258)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param to Longitude/Latitude pair, e.g. `c(-0.088780,51.506383)` or SF points of class "sfc_POINT" "sfc" and length one
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
journey = function(from, to,
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

  # Prepare the input format
  if("numeric" %in%  class(from)){
    # Plain Numers
    orig <- paste0(from, collapse = ",")
  }else if(all(c("sfc_POINT", "sfc") %in% class(from))){
    # SF Points
    if(length(from) == 1){
      if(is.na(st_crs(from)[[1]])){
        from = st_transform(from, 4326)
        message("Reprojecting from to lat/lng coordinates")
      }
      if(st_crs(from)[[1]] != 4326){
        from = st_transform(from, 4326)
        message("Reprojecting from to lat/lng coordinates")
      }

      orig <- paste0(c(from[1][[1]][1],from[1][[1]][2]), collapse = ",")
    }else{
      stop("Error: More than one point provided for from")
    }
  }else{
    stop("Error: Unknown input type for from, use numeric vector or sfc_POINT")
  }

  if("numeric" %in%  class(to)){
    # Plain Numers
    dest <- paste0(to, collapse = ",")
  }else if(all(c("sfc_POINT", "sfc") %in% class(to))){
    # SF Points
    if(length(to) == 1){
      if(is.na(st_crs(to)[[1]])){
        to = st_transform(to, 4326)
        message("Reprojecting to to lat/lng coordinates")
      }
      if(st_crs(to)[[1]] != 4326){
        to = st_transform(to, 4326)
        message("Reprojecting to to lat/lng coordinates")
      }

      dest <- paste0(c(to[1][[1]][1],to[1][[1]][2]), collapse = ",")
    }else{
      stop("Error: More than one point provided for to")
    }
  }else{
    stop("Error: Unknown input type for to, use numeric vector or sfc_POINT")
  }


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
  if(base_url == "http://fcc.transportapi.com/"){
    if(apitype == "public"){
      httrmsg = httr::modify_url(
        base_url,
        path = ft_string,
        query = list(
          modes = modes,
          not_modes = not_modes,
          service = service
        )
      )
    }else if(apitype == "car"){
      httrmsg = httr::modify_url(
        base_url,
        path = ft_string
      )
    }else if(apitype == "cycle"){
      httrmsg = httr::modify_url(
        base_url,
        path = ft_string
      )
    }else{
      stop("Error: Invalid routing apitype, use 'car','public', or 'cycle'")
    }
  }else{
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

  # Error checking and result returning

  if (is.element("error", names(obj))) {
    warning(paste0("Error: ", obj$error))
    return(obj$error)
  }else{
    if(length(obj$routes) == 0){
      warning(paste0("Error: transportapi.com was unable to find a route between ",orig," and ",dest))
      return(NA)
    }else{
      if(!save_raw) {
        obj = json2sf_tapi(obj,apitype)
      }
      return(obj)
    }
  }

}

#' Wrapper for jounrey fuction to perform multiple routings
#'
#' @param from Longitude/Latitude pair, e.g. `c(-0.134649,51.529258)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param to Longitude/Latitude pair, e.g. `c(-0.088780,51.506383)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param fromid optional vector of ids to associate with coordinates (makes resutls easier to understand when multiple routes are returned)
#' @param toid optional vector of ids to associate with coordinates
#' @export
#' @examples
#' None
#'
journey.batch = function(from, to, fromid = NULL, toid = NULL, save_raw = FALSE, ...){
  # check valid input types
  if(!all(class(from) %in% c("matrix","sfc_POINT","sfc"))){
    stop("Error: Invalid input type for from")
  }
  method.from = class(from)[1]

  if(!all(class(to) %in% c("matrix","sfc_POINT","sfc"))){
    stop("Error: Invalid input type for from")
  }
  method.to = class(to)[1]

  # Check the same number of Oigins and Destinations
  if(length(from) != length(to)){
    stop("Error: length(from) != length(to)")
  }


  if(method.from == "matrix"){
    niter = nrow(from)
  }else if(method.from == "sfc_POINT"){
    niter = length(from)
  }

  results = list()

  for(i in seq(1:niter)){
    #Extract the Oridign and Destiantion for this trip
    if(method.from == "matrix"){
      from.i = unlist(from[i,])
    }else if(method.from == "sfc_POINT"){
      from.i = from[i]
    }else{
      stop("Error: SERIOUS ERROR CODE 1")
    }

    if(method.to == "matrix"){
      to.i = unlist(to[i,])
    }else if(method.to== "sfc_POINT"){
      to.i = to[i]
    }else{
      stop("Error: SERIOUS ERROR CODE 2")
    }

    # Get routes
    routes = journey(from = from.i, to = to.i, save_raw = save_raw, ...)

    #If aviaible assing from and to ids
    if("logical" %in% class(routes)){#Check class as is.na() check every element in a dataframe and returns a warning
      # retunning NA means failed this route but try again
    }else if("character" %in% class(routes)){
      # Returning error message means rate limit reached, so stop trying
      if(grepl("usage limits are exceeded",routes)){
        warning("Rate limit cap detected aborting futher attempts")
        break
      }else{
        warning(paste0("Error: ", routes))
        routes = NA
      }
    }else{
      if(!is.null(fromid)){
        routes$fromid = fromid[i]
      }
      if(!is.null(toid)){
        routes$toid = toid[i]
      }
    }
    results[[i]] = routes
    rm(routes,from.i,to.i)
  }

  if(!save_raw){
    results <- results[!is.na(results)]
    suppressWarnings(results <- bind_rows(results))
    #rebuild the sf object
    results <- as.data.frame(results)
    results$geometry <- st_sfc(results$geometry)
    results <- st_sf(results)
    st_crs(results) <- 4326
  }

  return(results)
}






#' Convert output from transportapi.com into sf object
#'
#' @param obj Object from transportapi.com read-in with
#' @param apitype Type of routing can be car, cycle, public (DEFAULT)
#' @export
#' @examples
#' None
#'
json2sf_tapi = function(obj,apitype) {
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
