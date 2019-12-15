# Generate function to find pi


#' @title Plot City
#'
#' @description Compute an approximation of pi following a Monte-Carlo approach
#' with simulation of points.
#' @param B A \code{numeric} (integer) used to denote the number of
#' approximations.
#' @param seed An \code{integer} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} with the estimation of pi and the data frame containing
#' the points (x,y) used to estimate it as well as a boolean (True or False)
#' indicating whether the point is inside the circle.
#' @author Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
#' @import leaflet
#' @examples
#' estimate_pi(200, 65)
#' estimate_pi(700, 22)
#' 
#' 
#' 
plot_city <- function(x){
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = x$Longitude, lat = x$Latitude, popup = paste(paste0("Cinema: ", x$cinema), "<br/>",
                                                                paste0("Address: ", x$Address), "<br/>",
                                                                paste0("Telephone: ", x$Telephone), "<br/>",
                                                                paste0(  "Webpage: ", 
                                                                         "<a href='", x$Webpage , "'>", 
                                                                         x$Webpage, "</a>")))


}


#' @title Update markers of cities
#'
#' @description Compute an approximation of pi following a Monte-Carlo approach
#' with simulation of points.
#' @param B A \code{numeric} (integer) used to denote the number of
#' approximations.
#' @param seed An \code{integer} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} with the estimation of pi and the data frame containing
#' the points (x,y) used to estimate it as well as a boolean (True or False)
#' indicating whether the point is inside the circle.
#' @author Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
#' @import leaflet
#' @examples
#' estimate_pi(200, 65)
#' estimate_pi(700, 22)

adding_city <- function(mapID, x){
  leafletProxy(mapID) %>%
    clearMarkers() %>%
    addMarkers(lng = x$Longitude, lat = x$Latitude, popup = paste(paste0("Cinema: ", x$cinema), "<br/>",
                                                                  paste0("Address: ", x$Address), "<br/>",
                                                                  paste0("Telephone: ", x$Telephone), "<br/>",
                                                                  paste0(  "Webpage: ", 
                                                                           "<a href='", x$Webpage , "'>", 
                                                                           x$Webpage, "</a>")))
  
}