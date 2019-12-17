#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#runApp("movie_app", display.mode = "showcase")

#library(devtools)
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

############# Define UI for application

##########Header
header <- dashboardHeader(

    title = "Movies Locator", titleWidth = 330
)

##########Side Bar
sidebar <- dashboardSidebar(shinyDashboardThemes(
    theme = "grey_light"
),

width = 360,

sidebarMenu(id = "sidebar1",
            menuItem("The Movie App!", tabName = "menu_1", icon = icon("film")#,
                     # collapsible =
                     #     menuSubItem("Welcome to the movie app!", tabName = "sub_1"),
                     # menuSubItem('Welcome to the movie app!', tabName = 'sub_2')
            )
),
sidebarMenu(id = "sidebar2",
            menuItem("Preferences", tabName = "menu_2", icon = icon("dashboard"),
                     collapsible =
                         menuSubItem('Your Preferences', tabName = 'sub_3'),
                     menuSubItem("Your Preferences", tabName = "sub_4"),
                     # Sidebar panel for inputs ----
                     selectInput(inputId = "date",
                                 label = "Date:",
                                 choices = c("All", paste(sort(unique(allmovies_df$movie_date)),sep = ",")),
                                 selected = "All"),

                     # Choice of title
                     multiInput(
                         inputId = "title",
                         label = "Movie title",
                         choices = NULL,
                         choiceNames = sort(unique(allmovies_df$movie_title)),
                         choiceValues = sort(unique(allmovies_df$movie_title)),
                         options = list(
                             enable_search = TRUE,
                             scrollX = FALSE,
                             non_selected_header = "Choose between:",
                             selected_header = "You have selected:"
                         ),
                         width = "110%"),


                     #Select box for Cinema
                     multiInput(
                         inputId = "cinema",
                         label = "Cinema Names",
                         choices = NULL,
                         choiceNames = sort(unique(allmovies_df$cinema)),
                         choiceValues = sort(unique(allmovies_df$cinema)),
                         options = list(
                             enable_search = TRUE,
                             non_selected_header = "Choose between:",
                             selected_header = "You have selected:"
                         ),
                         width = "110%"),


                     #Select box for Location and Genre
                     multiInput(
                         inputId = "location",
                         label = "Locations",
                         choices = NULL,
                         choiceNames = sort(unique(allmovies_df$town)),
                         choiceValues = sort(unique(allmovies_df$town)),
                         options = list(
                             enable_search = TRUE,
                             non_selected_header = "Choose between:",
                             selected_header = "You have selected:"
                         ),
                         width = "110%"),


                     selectInput(inputId = "genre",
                                 label = "Genre:",
                                 choices = c("All", paste(sort(genrelist$genrelist),sep = ",")),
                                 selected = "All"),


                     #Range level bottons for Range, Length and Availability
                     verticalTabPanel(

                         sliderInput("availability", label = "Time Preferences(Hours):", #Time range when customer is available
                                     min = min(hour(as.chron(allmovies_df$viewing_times, "%H:%M"))),
                                     max = max(hour(as.chron(allmovies_df$viewing_times, "%H:%M"))),
                                     step = 1,
                                     value=c(min(hour(as.chron(allmovies_df$viewing_times, "%H:%M"))),
                                             max(hour(as.chron(allmovies_df$viewing_times, "%H:%M")))))

                     ),

                     #Ratings circle
                     verticalTabPanel(

                         sliderInput("ratings", label = "Movie Ratings:", #Time range when customer is available
                                     min = min(allmovies_df$movie_rating),
                                     max = max(allmovies_df$movie_rating),
                                     step = 0.5,
                                     value=c(min(allmovies_df$movie_rating),
                                             max(allmovies_df$movie_rating)))
                     )
            )),

sidebarMenu(id = "sidebar3",
            menuItem("All Movies", tabName = "menu_3", icon = icon("film")#,
                     # collapsible =
                     #     menuSubItem("See Movies Available", tabName = "sub_5"),
                     # menuSubItem('See Movies Available', tabName = 'sub_6')
            )
),
sidebarMenu(id = "sidebar4",
            menuItem("Contact us", tabName = "menu_4", icon = icon("phone")#,
                     # collapsible =
                     #     menuSubItem("Contact us", tabName = "sub_7"),
                     # menuSubItem('Contact us', tabName = 'sub_8')
            )
)
)

# Body #############################
#css <- "#location+div div a {color: black;}"
body <- dashboardBody(width = 50, style = "border-style: solid; border-color: black",
                      tabItems(
                          #Page 1
                          tabItem(tabName = 'menu_1',
                                  fluidPage(
                                      h1('Welcome to The Movie App!'),
                                      br(),
                                      br(),
                                      h4("Are you a movie passionate that goes often to the cinema?"),
                                      h4("Then this interactive platform is made for you !"),
                                      br(),
                                      h4("This is the best platform to find the perfect movie tailored for your personal tastes"),
                                      h4("You can filter your the movies per region, cinema, genre, and ratings !"),
                                      h4("The goal of the platform is to offer the best selection of movie without having to go on each cinema's website."),
                                      br(),
                                      h4("We display the location of every cinema according to what you selected so that you know where to go !"),
                                      br(),
                                      br(),
                                      h4("Find out more about the App on this video !"),
                                      h4("Our personal recommendation is to go to the moderne underneath the trainstation"),
                                      br(),
                                      br(),
                                      h4("Enjoy the App the ride !!"),
                                      br(),
                                      br(),
                                      HTML('<iframe width="840" height="472.5" src="https://www.youtube.com/embed/1wb7meCy_7U" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                      br(),
                                      br(),
                                      br()
                                  )),

                          tabItem(tabName = 'sub_4',
                                  fluidPage(
                                      leafletOutput("mymap"),
                                      p(),#to print the map
                                      h2("List of Movies"), # taille du titre
                                      div(dataTableOutput('table'), style = "font-size:125%; font-weight:bold")
                                  )),

                          tabItem(tabName = 'menu_3',
                                  fluidPage(
                                      h2('All Movies'),
                                      div(dataTableOutput('full_table'), style = "font-size:125%; font-weight:bold")
                                  )),

                          tabItem(tabName = 'menu_4',
                                  fluidPage(
                                      h1('Contact Page'),
                                      br(),
                                      uiOutput("img2"),
                                      br(),
                                      h2("Meet our team !"),
                                      br(),
                                      h3("Monica AKA The Package Creator"),
                                      h4("Contact: monica.navarrocalvo@unil.ch"),
                                      uiOutput("img3"),
                                      br(),
                                      h3("Inés AKA The Shiny Lord"),
                                      h4("Contact: ines.guardansgonzalez@unil.ch"),
                                      uiOutput("img4"),
                                      br(),
                                      h3("Guillaume AKA The HTML handler"),
                                      h4("Contact: guillaume.lakah@unil.ch"),
                                      uiOutput("img5"),
                                      br(),
                                      h3("Mathieu AKA The Scrapping Freak"),
                                      h4("Contact: mathieu.schnyder@unil.ch"),
                                      uiOutput("img6")

                                  ))
                      )
)


ui <- dashboardPage(header, sidebar, body)


