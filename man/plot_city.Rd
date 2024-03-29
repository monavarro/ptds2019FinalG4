% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_cinema.R
\name{plot_city}
\alias{plot_city}
\title{Plot City}
\usage{
plot_city(x)
}
\arguments{
\item{x}{A \code{database} with at least the colums of Latitude, Longitude,
cinema, Address, Telephone and Webpage related to several movies.}
}
\value{
A plot with the map of the world and all the points
available in the data base as markers.
}
\description{
Uses a data base with latitude and longitude of movies to
generate the points in it, while also adding the markers for each point
regarding the information of the cinema where the movie is being displayed.
}
\examples{
plot_city(x)
}
\author{
Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
}
