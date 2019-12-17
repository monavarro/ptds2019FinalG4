# Final project for Programming Tools in Data Science: Group 4 

Package: ptds2019FinalG4

Type: Package

Title: The Movie App

Version: 0.1.0

Author: Ines Guardans <ines.guardansgonzalez@unil.ch>
        Guillaume Lakah <guillaume.lakah@unil.ch>
        Monica Navarro <monica.navarrocalvo@unil.ch>
        Mathieu Schnyder <mathieu.schnyder@unil.ch>

Maintainer: Monica Navarro <monica.navarrocalvo@unil.ch>

Description: This package includes a shiny app that provides information on movies in Switzerland, 
    with a map presenting all the possible locations, which can be filtered by Movie Name,
    Town, Movie Genre, Viewing Times and Rating. Functions like 'plot_city', which gives an 
    initial map of the movies in a shiny app, and 'adding_city', that provides the map given 
    the new dataset created with the filters chosen are available.

Imports: shiny (>= 1.4.0),
    readxl,
    dplyr,
    Hmisc,
    rvest,
    rapportools,
    stringr,
    magrittr,
    writexl,
    leaflet,
    shinyWidgets,
    dashboardthemes,
    shinydashboard,
    DT,
    htmltools,
    knitr,
    tableHTML,
    mapsapi,
    xml2,
    chron,
    htmlTable,
    memisc,
    lubridate

License: GNU

Encoding: UTF-8

LazyData: true

RoxygenNote: 7.0.2


## Introduction
This github repository has been created as part of the course PROGRAMMING TOOLS IN DATA SCIENCE given by S.Orso and I.Rudnytskyi. Our project reflects the tools that our team has learned in class during the semester. The main goal was to test our skills and put them in practice. 
This repository is complementary to the package ptds2019FinalG4 that we created and that contains several functions.
Based on the data available on the internet, one of the main goal of our project was to create an interactive map of the Swiss Cinemas and to allow users to webscrap updated movies around Switzerland over the course of 6 days. 

## Video Presentation
To begin with, it is possible to understand how our project works by looking the following video https://www.youtube.com/watch?v=1wb7meCy_7U in which we make a presentation of our project.

## Methodology
We provide a function that allows the user to webscrap all movies currently available on cineman.ch. The movie title, cinemas where it is displayed, viewing times, ratings, poster and genre are obtained from the website and stored into a dataframe.
Based on the the choices of the user the dataframe is filtered dynamically to show in a map the cinemas where the movies complying with the chosen filters are displayed. In addition, a table with all the options is available under the map.
Finally the whole app was generated thanks to the Shiny package available on R.

### Main references
We used information coming from the internet and our previous knowledge. However, we can underline some sources that have been particularily usefull when doing this project:
- Cineman: https://www.cineman.ch/en/
- Leaflet package R: https://rstudio.github.io/leaflet/
