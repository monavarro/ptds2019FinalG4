# ptds2019FinalG4

Package: ptds2019FinalG4

Type: Package

Title: The Movie App

Version: 0.1.0

Author: Ines Guardans <"ines.guardans@unil.ch">
        Guillaume Lakah <"guillaume.lakah@unil.ch">
        Monica Navarro <"monica.navarrocalvo@unil.ch">
        Mathieu Schnyder <"mathieu.schnyder@unil.ch">

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
