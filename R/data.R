
#' Information of 252 cinemas in Switzerland.
#'
#' A dataset containing the coordinates and other attributes of 252 cinemas
#' in Switzerland.
#'
#' @format A data frame with 252 rows and 8 variables:
#' \describe{
#'   \item{cinema}{Name of the cinema}
#'   \item{town}{Town where the cinema is located}
#'   \item{Known as}{Alternative name of the cinema}
#'   \item{Address}{Adrdess of the cinema}
#'   \item{Webpage}{Webpage of the cinema when available}
#'   \item{Telephone}{Telephone of the cinema when available}
#'   \item{Latitude}{Latitude of the cinema}
#'   \item{Longitude}{Longitude of the cinema}
#' }
#' @source Own creation using info of cinemas in SWitzerland from
#' \url{http://www.cineman.ch/} and attributes from
#' \url{https://www.google.ch/maps}
"cinema_coordinates"


#' Information of all movies for 3 days in SWitzerland.
#'
#' A dataset containing the viewing times and other attributes of all movies
#' displayed in Switzerland for the next 3 days.
#'
#' @format A data frame with approximately 8677 rows and 42 variables:
#' \describe{
#'   \item{movie_title}{Name of the movie}
#'   \item{cinema}{Name of the cinema}
#'   \item{town}{Town where the cinema is located}
#'   \item{viewing_times}{Hour and minute when the movie will be displayed}
#'   \item{movie_date}{Date when the movie will be displayed}
#'   \item{genres}{Binary variables that indicated if the movie corresponds
#'   to that genre (if = 1)}
#'   \item{movie_rating}{Numeric variable from 1 to 5 that contains
#'   the rating of the movie}
#'   \item{movie_poster}{String with link for the movie poster}
#'   \item{Known as}{Alternative name of the cinema}
#'   \item{Address}{Adrdess of the cinema}
#'   \item{Webpage}{Webpage of the cinema when available}
#'   \item{Telephone}{Telephone of the cinema when available}
#'   \item{Latitude}{Latitude of the cinema}
#'   \item{Longitude}{Longitude of the cinema}
#' }
#' @source Scrapped from \url{http://www.cineman.ch/}
"allmovies_df"


#' Information of all genres of movies for 3 days in SWitzerland.
#'
#' A dataset containing all genres of all movies displayed in Switzerland for
#' the next 3 days.
#'
#' @format A data frame with approximately 8677 rows and 42 variables:
#' \describe{
#'   \item{genrelist}{All genres in movies for the nest 3 days}
#' }
#' @source Scrapped from \url{http://www.cineman.ch/}
"genrelist"
