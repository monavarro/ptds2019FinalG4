##### Loading Packages

# devtools::install_github("nik01010/dashboardthemes", force = T)
# devtools::install_github("monavarro/ptds2019FinalG4", force = T)
library(ptds2019FinalG4)
library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(Hmisc)
library(rvest)
library(rapportools)
library(stringr)
library(magrittr)
library(writexl)
library(shiny)
library(leaflet)
require(shinyWidgets)
require(dashboardthemes)
require(shinydashboard)
library(DT)
library(htmltools)
library(knitr)
library(tableHTML)
library(mapsapi)
library(xml2)
library(chron)
library(htmlTable)
library(memisc)
library(lubridate)

# Define server logic to summarize and view selected dataset
server <- function(input, output, session) {

    ###### <----- Main Page
    output$img1 <- renderUI({
        tags$img(src = "https://cinerive.com/sites/default/files/styles/full/public/eio_2012-12-11_cosmo_salle1.jpg?itok=_SHnxBbs", height="10%", width="35%") })



    ###### <----- Page 2 & Sidepage
    filtered_df <- reactive({

        # Title
        if(!is.empty(input$title)){
            chosen_movie <- allmovies_df %>%
                filter(movie_title %in% input$title)}
        else {
            chosen_movie <- allmovies_df
        }

        # Location
        if (!is.empty(input$location)){
            chosen_location <- allmovies_df %>%
                filter(town %in% input$location)
        }
        else {
            chosen_location <- allmovies_df
        }

        # Cinema
        if (!is.empty(input$cinema)){
            chosen_cinema <- allmovies_df %>%
                filter(cinema %in% input$cinema)
        }
        else {
            chosen_cinema <- allmovies_df
        }

        # Filter location and movies according to the chosen values
        choices_df <- allmovies_df %>%
            filter(movie_title %in% chosen_movie$movie_title,
                   town %in% chosen_location$town,
                   cinema %in% chosen_cinema$cinema,
                   hour(as.chron(allmovies_df$viewing_times, "%H:%M")) >= input$availability[1],
                   hour(as.chron(allmovies_df$viewing_times, "%H:%M")) <= input$availability[2],
                   as.numeric(allmovies_df$movie_rating) >= input$ratings[1],
                   as.numeric(allmovies_df$movie_rating) <= input$ratings[2],
                   sub("\\/.*", "", allmovies_df$movie_date) == sub("\\/.*", "", input$date)
            )

        # Genre
        if(input$genre != "All"){
            choices_df <- choices_df %>%
                filter(UQ(as.symbol(input$genre)) == 1)
        }

        return(choices_df)

    })

    ##### MAP
    output$mymap <- renderLeaflet({
        plot_city(filtered_df())
    })

    # Change cities in map according to user input in city selection
    observe({
        selected_location <- filtered_df()
        adding_city(mapID = "mymap", selected_location)
    })

    observe({
        click <- input$mymap_marker_click
        zoom <- isolate(input$mymap_zoom)
        if(is.null(click))
            return()

        leafletProxy('mymap') %>%
            setView(click$lng, click$lat, zoom = 15)
    })

    ##### Table page 2
    output$table <- renderDataTable(filtered_df() %>%
                                        dplyr::select(movie_poster, movie_title, cinema, movie_date, viewing_times, movie_rating) %>%
                                        dplyr::group_by(movie_poster, movie_title, cinema, movie_date, movie_rating) %>%
                                        summarise(viewing_times = paste(viewing_times, collapse = ";  ")) %>%
                                        dplyr::select(movie_poster, movie_title, cinema, viewing_times, movie_date, movie_rating) %>%
                                        dplyr::rename("Movie Poster" = movie_poster,
                                                      "Movie Title" = movie_title,
                                                      "Movie Date" = movie_date,
                                                      "Cinema" = cinema,
                                                      "Viewing Times" = viewing_times,
                                                      "Movie Rating" = movie_rating
                                        ) %>%
                                        DT::datatable(escape = FALSE, rownames = FALSE))




    ###### <----- Page 3 Table page 3
    output$full_table <- renderDataTable(filtered_df() %>%
                                             dplyr::select(movie_poster, movie_title, cinema, movie_date, viewing_times, movie_rating) %>%
                                             dplyr::group_by(movie_poster, movie_title, cinema, movie_date, movie_rating) %>%
                                             summarise(viewing_times = paste(viewing_times, collapse = ", ")) %>%
                                             dplyr::select(movie_poster, movie_title, cinema, viewing_times, movie_date, movie_rating) %>%
                                             dplyr::rename("Movie Poster" = movie_poster,
                                                           "Movie Title" = movie_title,
                                                           "Movie Date" = movie_date,
                                                           "Cinema" = cinema,
                                                           "Viewing Times" = viewing_times,
                                                           "Movie Rating" = movie_rating
                                             ) %>%
                                             DT::datatable(escape = FALSE, rownames = FALSE))



    ###### <----- Contact Page 4
    output$img2 <- renderUI({
        tags$img(src = "https://i.ibb.co/v3h021W/Screen-Shot-2019-12-15-at-09-39-46.png", height="55%", width="55%") })

    output$img3 <- renderUI({
        tags$img(src = "https://i.ibb.co/wgV8V6Z/Photo-Monica.jpg", height="30%", width="30%") })

    output$img4 <- renderUI({
        tags$img(src = "https://i.ibb.co/xmRqW58/Foto-ines.jpg", height="30%", width="30%") })

    output$img5 <- renderUI({
        tags$img(src = "https://i.ibb.co/FgJf2X1/Photo-Guillaume.png", height="30%", width="30%") })

    output$img6 <- renderUI({
        tags$img(src = "https://i.ibb.co/qnt5tTY/photo-mathieu.jpg", height="30%", width="30%") })

}



