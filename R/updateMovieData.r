
#' @title Update Movie Data
#'
#' @description Scraps 2 data sets from the webpage \url{https://www.cineman.ch}
#' that work directly with the shiny app loaded in the function
#' \code{runMovieApp}
#' @param updatemovies A binary \code{string} with options TRUE or FALSE to scrap
#' the movie data of the following 5 days.Default value is \code{TRUE}.
#' @return Datasets for all the genres available and for movies in Switzerland
#' for the following 5 days. Datasets are stored in ./data as .rda files.
#' @author Ines Guardans, Guillaume Lakah, Monica Navarro & Mathieu Schnyder
#' @export
#' @examples
#' updateMovieData(updatemovies = TRUE)
updateMovieData <- function(updatemovies = TRUE){

  library(magrittr)
url_scraping <- updatemovies
genre_scraping <- updatemovies
movie_scraping <- updatemovies


if(url_scraping){


  urls <- list()
  # # #
  # # # # first page scrap
  page1 <- xml2::read_html("https://www.cineman.ch/en/movie/now/")

  urls[[1]] <- page1 %>%
    rvest::html_nodes(".col-xs-8") %>%
    rvest::html_children() %>%
    rvest::html_attr('href') %>%
    na.omit()

  attributes(urls[[1]]) <- NULL
  # #
  npages <- page1 %>%
    rvest::html_nodes(".pages") %>%
    rvest::html_text() %>%
    magrittr::extract(1) %>%
    stringr::str_extract("\\d\\d") %>%
    as.numeric() %>%
    magrittr::subtract(1)
  # # #
  # # # # other pages scrap


  for(i in 1:npages) {
    page <- paste0(
      "https://www.cineman.ch/en/movie/now/?movie_list_292_page=",
      i
    ) %>%
      xml2::read_html()

    urls[[i + 1]] <- page %>%
      rvest::html_nodes(".col-xs-8") %>%
      rvest::html_children() %>%
      rvest::html_attr('href') %>%
      na.omit()

    attributes(urls[[i + 1]]) <- NULL

  }

  urls <- unlist(urls)

}


if(genre_scraping){
  # #Scrap all possible genre
  genrelist <- list()
  # #
  for (i in 1:length(urls)){
    Sys.sleep(2)
    genrelist[[i]] <- paste0("https://www.cineman.ch", urls[i])%>%
      xml2::read_html() %>%
      rvest::html_nodes(".label-genre") %>%
      rvest::html_text()
  }

  genrelist <- unique(unlist(genrelist))
  genrelist <- as.data.frame(genrelist)
  save(genrelist, file = "./data/genrelist.rda" )

}


if(movie_scraping){
  movieslist <- list()
  movie_dates <- list()
  dates <- c(lubridate::today(), lubridate::today()+1, lubridate::today()+2, lubridate::today()+3, lubridate::today()+4, lubridate::today()+5)
  # Construct big dataframe
  for (i in 1:length(urls)){
    movie <- paste0("https://www.cineman.ch", urls[i])%>%
      xml2::read_html()
    #
    # #   #TITLE
    movie_title <- movie %>%
      rvest::html_nodes(".col-sm-8 span") %>%
      rvest::html_text() %>%
      as.data.frame()

    names(movie_title)[1] <- "movie_title"

    #VIEWING TIME + CINEMA DF called "time_cinema"
    for (j in 1:length(dates)){
      Sys.sleep(3)
      moviedate <- paste0("https://www.cineman.ch", urls[i],"cinema.html?date=",dates[j]) %>%
        xml2::read_html()
?str_squish
      time_cinema <- moviedate %>%
        rvest::html_nodes("em .link , .col-xs-6 p:nth-child(1)") %>%
        rvest::html_text() %>%
        gsub(pattern = "\t", replacement = "", fixed = T) %>%
        gsub(pattern = "\n", replacement = "", fixed = T) %>%
        gsub(pattern = "(4K)", replacement = "", fixed = T) %>%
        gsub(pattern = "( 4K)", replacement = "", fixed = T) %>%
        gsub(pattern = "(4K   Dolby Atmos)", replacement = "", fixed = T) %>%
        gsub(pattern = "(4KDolbyAtmos)", replacement = "", fixed = T) %>%
        trimws() %>%
        stringr::str_split("\\,") %>%
        unlist() %>%
        stringr::str_squish() %>%
        as.data.frame()

      names(time_cinema)[names(time_cinema) == "."] <- "viewing_times"

      # Separate into 2 columns
      time_cinema <- time_cinema %>%
        dplyr::mutate(cinema = replace(viewing_times, !grepl("^[[:upper:]]", viewing_times), NA )) %>%
        tidyr::fill(cinema) %>%
        dplyr::filter(!grepl("^[[:upper:]]", viewing_times))%>%
        dplyr::filter(stringr::str_detect(string = viewing_times, pattern = "^\\d\\d\\:\\d\\d$"))%>%
        dplyr::distinct()


      #CINEMA NAME + TOWN
      cinema_town_raw <- moviedate %>%
        rvest::html_nodes("h5 .link") %>%
        rvest::html_text() %>% as.data.frame()

      cinema <- cinema_town_raw[-seq(0, nrow(cinema_town_raw), 2),] %>% as.data.frame()
      names(cinema)[1] <- "cinema"

      town <- cinema_town_raw[seq(0, nrow(cinema_town_raw), 2),] %>% as.data.frame()
      names(town)[1] <- "town"

      cinema_town <- cbind(cinema,town) # df with cinema and town together
      rm(cinema,town) #remove cinema and town df

      movie_times <- dplyr::left_join(cinema_town, time_cinema, by = "cinema") %>%
        na.omit()

      #Appending date
      if(nrow(movie_times) != 0){
        movie_times["movie_date"] <- dates[j]}


      movie_dates[[j]] <- movie_times

    }


    movie_df <- do.call(rbind, movie_dates)

    # Appending the movie title to the movie_df
    movie_title <- movie_title[rep(seq_len(nrow(movie_title)), dplyr::count(movie_df)), ] %>%
      as.data.frame()
    names(movie_title)[1] <- "movie_title"

    movie_df <- cbind(movie_title, movie_df)

    # #
    # #   #GENRE
    if(nrow(movie_df) != 0){

      genre <- movie %>%
        rvest::html_nodes(".label-genre") %>%
        rvest::html_text()

      genre_df <- as.data.frame(matrix(as.numeric(genrelist$genrelist %in% genre), nrow = 1))

      names(genre_df) <- genrelist$genrelist


      movie_df <- cbind(movie_df, genre_df)}
    #
    # # #MOVIE RATING
    movie_rating <- movie %>%
      rvest::html_nodes(".color-playstation") %>%
      rvest::html_text() %>%
      as.data.frame()

    names(movie_rating)[1] <- "movie_rating"

    movie_rating <- movie_rating[rep(seq_len(nrow(movie_rating)), dplyr::count(movie_df)), ] %>%
      as.data.frame()

    names(movie_rating)[1] <- "movie_rating"

    if(nrow(movie_df) != 0 & nrow(movie_rating) != 0){
      movie_df <- cbind(movie_df, movie_rating) }

    if (nrow(movie_rating) == 0) {
      movie_df <- movie_df %>%
        dplyr::mutate(movie_rating = 0)
    }

    #  #MOVIE POSTER
    movie_poster <- movie %>%
      rvest::html_node(".poster-zoom img") %>%
      rvest::html_attr('src')

    #movie_poster <- paste0("<img src=\"",movie_poster,"\" width=174px, length=240></img>") %>% as.data.frame()
    movie_poster <- paste0("<img src=\"",movie_poster,"\" height=\"129\"> <width=\"92\"></img>") %>% as.data.frame()

    movie_poster <- movie_poster[rep(seq_len(nrow(movie_poster)), dplyr::count(movie_df)), ] %>%
      as.data.frame()

    names(movie_poster)[1] <- "movie_poster"

    if(nrow(movie_df) != 0){
      movie_df <- cbind(movie_df, movie_poster)
      movie_df <- movie_df %>%
        dplyr::mutate(movie_title = as.character(movie_title),
               town = as.character(town),
               cinema = as.character(cinema))
    }


    movieslist[[i]] <- movie_df
  }

  allmovies_df <- do.call(rbind, movieslist)

  #Adding coordinates of cinemas to the data set

  allmovies_df <- dplyr::inner_join(allmovies_df,
                            cinema_coordinates,
                            by = c("cinema", "town"),
                            all = TRUE)


  #Cleaning movies data set and giving corresponding format

  allmovies_df <- allmovies_df[!(allmovies_df$movie_title=="Posted "),]


  allmovies_df$town <- allmovies_df$town %>%
                              gsub(pattern = "ü", replacement = "u")

  allmovies_df$town <- replace(as.character(allmovies_df$town),
                               allmovies_df$town == "Gen?ve", "Geneve")

  allmovies_df[,4] <- as.character(allmovies_df[,4])
  allmovies_df$movie_rating <- allmovies_df$movie_rating %>%
                              as.character() %>%
                              as.numeric()
  allmovies_df$movie_rating[is.na(allmovies_df$movie_rating)] <- 0
  allmovies_df$movie_poster <- as.character(allmovies_df$movie_poster)
  allmovies_df[,5] <- as.Date(allmovies_df[,5], format = "%y/%m/%d") %>%
    format("%d/%m/%y")

  #Saving in system file
  save(allmovies_df, file = "./data/allmovies_df.rda" )

}
}






