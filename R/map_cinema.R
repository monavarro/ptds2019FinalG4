# Functions for map generation

#' @title Plot City
#'
#' @description Uses a data base with latitude and longitude of movies to
#' generate the points in it, while also adding the markers for each point
#' regarding the information of the cinema where the movie is being displayed.
#' @param x A \code{database} with at least the colums of Latitude, Longitude,
#' cinema, Address, Telephone and Webpage related to several movies.
#' @return A plot with the map of the world and all the points
#' available in the data base as markers.
#' @author Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
#' @export
#' @examples
#' plot_city(x)
plot_city <- function(x){
  y <- unique(x[c("Latitude", "Longitude", "cinema", "Address", "Telephone", "Webpage")])
leaflet::leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = y$Longitude, lat = y$Latitude, popup = paste(paste0("Cinema: ", y$cinema), "<br/>",
                                                                paste0("Address: ", y$Address), "<br/>",
                                                                paste0("Telephone: ", y$Telephone), "<br/>",
                                                                paste0(  "Webpage: ",
                                                                         "<a href=\"http://", y$Webpage , "\"target=\"_blank\">",
                                                                         y$Webpage, "</a>")))


}


#' @title Update markers of cities
#'
#' @description This function updates the plot generated with function
#' \code{plot_city} with the new markers avaiable on a new data set, that's
#' created base in the reactive results of a shiny app.
#' @param mapId The ID of a map \code{plot} created with function
#' \code{plot_city} that has to be updated.
#' @param x A \code{database} with at least the colums of Latitude, Longitude,
#' cinema, Address, Telephone and Webpage related to several movies.
#' @return The updated plot of function \code{plot_city} with the
#' markers specified by the filters in a shiny app.
#' @author Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
#' @export
#' @examples
#' adding_city(mapID,x)devto
adding_city <- function(mapID, x){
  z <- unique(x[c("Latitude", "Longitude", "cinema", "Address", "Telephone", "Webpage")])
  leaflet::leafletProxy(mapID) %>%
    clearMarkers() %>%
    addMarkers(lng = z$Longitude, lat = z$Latitude, popup = paste(paste0("Cinema: ", z$cinema), "<br/>",
                                                                  paste0("Address: ", z$Address), "<br/>",
                                                                  paste0("Telephone: ", z$Telephone), "<br/>",
                                                                  paste0(  "Webpage: ",
                                                                           "<a href=\"http://", z$Webpage , "\"target=\"_blank\">",
                                                                           z$Webpage, "</a>")))

}
