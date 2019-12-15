#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#runApp("movie_app", display.mode = "showcase")

#library(devtools)
#install_github("nik01010/dashboardthemes")
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

    title = "Movies Locator", titleWidth = 250, dropdownMenu(type = "messages",
                                                             messageItem(
                                                                 from = "Assistant",
                                                                 message = "Welcome to the Movies Locator App."),
                                                             messageItem(
                                                                 from = "New User",
                                                                 message = "Open sidebar & select preferences",
                                                                 icon = icon("question"),
                                                                 time = paste(as.Date(Sys.Date(), format = "%dd-%mm-%yyyy"))
                                                             ),
                                                             messageItem(
                                                                 from = "Support",
                                                                 message = "Send us an email!",
                                                                 icon = icon("life-ring"),
                                                                 time = paste(as.Date(Sys.Date(), format = "%dd-%mm-%yyyy"))
                                                             )
    ),
    dropdownMenuOutput("messageMenu"),
    dropdownMenu(type = "notifications",
                 notificationItem(
                     text = "XXXXXX new movies this week",
                     icon("users")
                 ),
                 notificationItem(
                     text = "Blablabla",
                     icon("truck"),
                     status = "success"
                 ),
                 notificationItem(
                     text = "Overall movie rating XXXX %",
                     icon = icon("exclamation-triangle"),
                     status = "warning"
                 )
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
)

##########Side Bar
sidebar <- dashboardSidebar(shinyDashboardThemes(
    theme = "grey_light"
),

width = 290,

sidebarMenu(id = "sidebar1",
            menuItem("The Movie App!", tabName = "menu_1", icon = icon("film"),
                     collapsible =
                         menuSubItem("Welcome to the movie app!", tabName = "sub_1"),
                     menuSubItem('Welcome to the movie app!', tabName = 'sub_2')
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
                                 choices = sort(unique(ptds2019FinalG4:::allmovies_df$movie_date)),
                                 selected = sort(unique(ptds2019FinalG4:::allmovies_df$movie_date))[1]),

                     # Choice of title
                     multiInput(
                         inputId = "title",
                         label = "Movie title",
                         choices = NULL,
                         choiceNames = sort(unique(ptds2019FinalG4:::allmovies_df$movie_title)),
                         choiceValues = sort(unique(ptds2019FinalG4:::allmovies_df$movie_title)),
                         options = list(
                             enable_search = TRUE,
                             scrollX = FALSE,
                             non_selected_header = "Choose between:",
                             selected_header = "You have selected:"
                         )),


                     #Select box for Location and Genre
                     multiInput(
                         inputId = "location",
                         label = "Locations",
                         choices = NULL,
                         choiceNames = sort(unique(ptds2019FinalG4:::allmovies_df$town)),
                         choiceValues = sort(unique(ptds2019FinalG4:::allmovies_df$town)),
                         options = list(
                             enable_search = TRUE,
                             non_selected_header = "Choose between:",
                             selected_header = "You have selected:"
                         )),


                     selectInput(inputId = "genre",
                                 label = "Genre:",
                                 choices = c("All", sort(unique(ptds2019FinalG4:::genrelist$genrelist))),
                                 selected = "All"),


                     #Range level bottons for Range, Length and Availability
                     verticalTabPanel(

                         sliderInput("availability", label = "Time Preferences(Hours):", #Time range when customer is available
                                     min = 7, max = 24, step = 1, value=c(7, 24))

                     ),

                     #Ratings circle
                     verticalTabPanel(

                         sliderInput("ratings", label = "Movie Ratings:", #Time range when customer is available
                                     min = 1, max = 5, step = 0.5, value=c(1, 5))

                     )


            )),
sidebarMenu(id = "sidebar3",
            menuItem("All Movies", tabName = "menu_3", icon = icon("film"),
                     collapsible =
                         menuSubItem("See Movies Available", tabName = "sub_5"),
                     menuSubItem('See Movies Available', tabName = 'sub_6')
            )
),
sidebarMenu(id = "sidebar4",
            menuItem("Contact us", tabName = "menu_4", icon = icon("phone"),
                     collapsible =
                         menuSubItem("Contact us", tabName = "sub_7"),
                     menuSubItem('Contact us', tabName = 'sub_8')
            )
)
)
# Body #############################
#css <- "#location+div div a {color: black;}"
body <- dashboardBody(width = 50, style = "border-style: solid; border-color: black",
                      tabItems(
                          #Page 1
                          tabItem(tabName = 'sub_2',
                                  fluidPage(
                                      h1('Welcome to The Movie App!'),
                                      br(),
                                      br(),
                                      h4("Are you a movie passionate and do you go often to the cinema?"),
                                      h4("Then this App was made for you !"),
                                      br(),
                                      h4("This App is you best friend. Thanks to The Movie App you can look for all the movies you like in Switzerland."),
                                      h4("You can filter your movies
                   by location, genre, cinema's, ratings and many more options."),
                                      h4("The goal of the App is to offer the best selection of movie without having to go on
                   each cinema's websites."),
                                      br(),
                                      h4("Moreover, the App displays the location of every cinema where they show what you selected around Switzerland so that you know where to go!"),
                                      br(),
                                      br(),
                                      h4("Find out more about the App on this video !"),
                                      br(),
                                      tags$iframe(width="800", height="450", src="https://www.youtube.com/embed/T1-k7VYwsHg", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                                      br(),
                                      htmlOutput("picture", height = "350px", width = "50%")

                                  )),
                          tabItem(tabName = 'sub_4',
                                  fluidPage(
                                      leafletOutput("mymap"),
                                      p(),#to print the map
                                      dataTableOutput('table'),
                                      h2("List of Movies") # taille du titre

                                  )),

                          tabItem(tabName = 'sub_6',
                                  fluidPage(
                                      h2('All Movies'),
                                      dataTableOutput('full_table')

                                  )),

                          tabItem(tabName = 'sub_8',
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


