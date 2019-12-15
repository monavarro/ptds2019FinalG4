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

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {


    ## Main Page
    src = "https://cinerive.com/sites/default/files/styles/full/public/eio_2012-12-11_cosmo_salle1.jpg?itok=_SHnxBbs"
    output$picture <- renderText({c('<img src="',src,'">')})

    output$img2 <- renderUI({
        tags$img(src = "https://i.ibb.co/v3h021W/Screen-Shot-2019-12-15-at-09-39-46.png", height="70%", width="70%") })

    output$img3 <- renderUI({
        tags$img(src = "https://i.ibb.co/wgV8V6Z/Photo-Monica.jpg", height="70%", width="70%") })

    output$img4 <- renderUI({
        tags$img(src = "https://i.ibb.co/xmRqW58/Foto-ines.jpg", height="70%", width="70%") })

    output$img5 <- renderUI({
        tags$img(src = "https://i.ibb.co/FgJf2X1/Photo-Guillaume.png", height="70%", width="70%") })

    output$img6 <- renderUI({
        tags$img(src = "https://i.ibb.co/qnt5tTY/photo-mathieu.jpg", height="70%", width="70%") })


    ##S IDE-PAGE DASHBOARD PAGE 2
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

        # Filter location and movies according to the chosen values
        choices_df <- allmovies_df %>%
            filter(movie_title %in% chosen_movie$movie_title,
                   town %in% chosen_location$town,
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

        #choices_df <- htmlTable(choices_df)

        return(choices_df)

    })


    ##Custom the header Message
    #output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    #msgs <- apply(messageData, 1, function(row) {
    # messageItem(from = row[["from"]], message = row[["message"]])
    # })

    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    #dropdownMenu(type = "messages", .list = msgs)
    #})

    ##Map main Page 1
    # map that opens by default
    output$mymap <- renderLeaflet({
        plot_city(allmovies_df)
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


    # Output Table main page

    #output$table <- renderUI(filtered_df())
    # observe({
    output$table <- renderDataTable(filtered_df() %>%
                                        dplyr::select(movie_poster, movie_title, cinema, movie_date, viewing_times, movie_rating) %>%
                                        dplyr::rename("Movie Poster" = movie_poster,
                                                      "Movie Title" = movie_title,
                                                      "Movie Date" = movie_date,
                                                      "Cinema" = cinema,
                                                      "Viewing Times" = viewing_times,
                                                      "Movie Rating" = movie_rating
                                        ) %>%
                                        DT::datatable(escape = FALSE))
    # })

    # output$table <- DT::renderDataTable({
    #
    #   DT::datatable(table, escape = FALSE)
    # })
    #
    #output$table <- DT::renderDataTable({ DT::datatable(table(), escape = FALSE)})



    # Output Table full page
    # observe({
    output$full_table <- renderDataTable(filtered_df() %>%
                                             dplyr::select(movie_poster, movie_title, cinema, movie_date, viewing_times, movie_rating) %>%
                                             dplyr::rename("Movie Poster" = movie_poster,
                                                           "Movie Title" = movie_title,
                                                           "Movie Date" = movie_date,
                                                           "Cinema" = cinema,
                                                           "Viewing Times" = viewing_times,
                                                           "Movie Rating" = movie_rating
                                             ) %>%
                                             DT::datatable(escape = FALSE))
    # })


    # https://stackoverflow.com/questions/52914941/multiple-kable-tables-with-images
    # https://rstudio.github.io/leaflet/morefeatures.html
    # https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/
    # https://shiny.rstudio.com/articles/datatables.html
    # https://rdrr.io/cran/htmlTable/man/htmlTableWidget-shiny.html
    # https://cran.r-project.org/web/packages/tableHTML/vignettes/tableHTML.html
    # https://rstudio.github.io/DT/shiny.html
    # http://www.lyzander.com/r/2018/01/05/tablehtml-vesion-1.1.0


    #output$movie_table <- renderUI(movie_table)


    ##Map full Page 2 it is still not displaying full page
    output$mymap2 <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=6.626680, lat=46.521886, popup="Pathe Flon") %>%
            addMarkers(lng=6.630009, lat=46.518178, popup="Pathe Les Galeries") %>%
            addMarkers(lng=6.602678, lat=46.527177, popup="Cinetoile") %>%
            addMarkers(lng=6.627212, lat=46.523973, popup="Zinema Lausanne") %>%
            addMarkers(lng=6.635671, lat=46.518590, popup="Capitole") %>%
            addMarkers(lng=6.623950, lat=46.524160, popup="Cinema Oblo") %>%
            addMarkers(lng=6.630667, lat=46.532948, popup="Cinema Bellevaux") %>%
            addMarkers(lng=6.624720, lat=46.520530, popup="Cinemathèque suisse") %>%
            addMarkers(lng=6.658788, lat=46.512161, popup="City Club")
        ##ADD here the result of the API's to run the pinpoints on the map
    })

}





