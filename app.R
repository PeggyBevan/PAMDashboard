#ACOUSTIC MONITORING DASHBOARD
#PEGGY BEVAN, JUNE 2023
# This script creates a shiny dashboard app that can be used to display
# detection records from a PAM survey.
# In order to make this work, a csv containing species' detections
# needs to be in the 'data' folder of this directory.
# Only include one detection csv in the data folder!
# There should also be a csv called 'threatenedspecies.csv' in
# the data folder:
# this contains all the information about protection levels
# of certain species and can be updated if a species' status changes

# How to use this script:
# Before running this app for the first time,
# run the script called 'requirements'. This will make sure you have
# all the correct packages installed. You only need to do this once.
# Click Run App on the top right of the script window to get the
# app to run in the plot viewer or on your browser.
#
# Script layout:
# pre-amble - the first lines of this script load packages, the detections csv,
# and set the wilding colour palette. These shouldn't need much editing
# UI - This is the section that sets out the layout and appearence of the
# app
# Server - This is the section that builds the plots, gets important
# statistics from the data, and is passed back to the UI
# The final line uses the shinyApp() function to put the UI and server
# together

# Online tools for shiny:
# Introduction to shiny using R: z
# Introduction to shinydashboard: https://rstudio.github.io/shinydashboard/get_started.html

# Bringing the app online
# When you visualise your app in r, it is not a live website.
# To deploy the app online, you can host it at shinyapps.io, or contact
# you IT department to host it on the company server.
# Introduction to shinyapps.io: https://shiny.posit.co/r/articles/share/shinyapps/
# If you have successfully uploaded your app and need to update it,
# just run:
# rsconnect::deployApp()
# in your console.


library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(devtools)
library(ggthemr)
library(shinydashboard)
library(leaflet)


#ggthemr(palette = 'grass', type = 'outer')
wildingPalette = c(`Wilding light green` = '#95C11F', 'Wilding yellow' = '#FFD100', 
                   `Wilding grey` = '#6c6e74',
                   `Wilding mid green` = '#9CAF88',
                   `Wilding dark blue` = '#003E51',
                   `Wilding dark green` = '#5E6738',
                   `Wilding light blue` = '#3EB1CB')
wildingCols = function(...) {
  cols = c(...)
  if (is.null(cols)) {
    return(wildingPalette)
  }
  wildingPalette[cols]
  }
wildingCols('Wilding light green')


#Return function to interpolate a wilding color palette
#
# @param reverse Boolean indicating whether the palette should be reversed
# @param ... Additional arguments to pass to colorRampPalette()
#'
wilding_pal <- function(reverse = FALSE, ...) {
  pal <- wildingPalette
  
  if (reverse) {
    pal <- rev(pal)
    }
  
  colorRampPalette(pal, ...)
}
wilding_pal()(9) #gives smooth pallette!


# Color scale constructor for wilding colors
#
# @param discrete Boolean indicating whether color aesthetic is discrete or not
# @param reverse Boolean indicating whether the palette should be reversed
# @param ... Additional arguments passed to discrete_scale() or
#            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#
scale_color_wilding <- function(discrete = TRUE, reverse = FALSE, ...) {
  pal <- wilding_pal(reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("wilding_"), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for wilding colors
#'
# @param discrete Boolean indicating whether color aesthetic is discrete or not
# @param reverse Boolean indicating whether the palette should be reversed
# @param ... Additional arguments passed to discrete_scale() or
#            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_wilding <- function(discrete = TRUE, reverse = FALSE, ...) {
  pal <- wilding_pal(reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("wilding_"), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



#load data
files = list.files('data', full.names = T)

for (i in 1:length(files)) {
  if (grepl("threatenedspecies", files[i])) {
    specieslist = read.csv(files[i])
  } else {
    df = read.csv(files[i])
  }
}

#match species with their threatened status
df2 = left_join(df, specieslist, by = c('SCIENTIFIC.NAME' = 'Scientific.name'))
df2$protectedBIN = ifelse(df2$protection == 'None', 'Not protected','Protected')




# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'green',
  dashboardHeader(title = 'Acoustic monitoring dashboard',
                  titleWidth = 350,
                  tags$li(div(tags$img(src = 'RSK-Wilding-logo-strapline-screen.jpg',
                                       width = '250px'),
                              style = 'margin-right:5px;margin-top:5px;margin-bottom:5px;'),
                          class = 'dropdown'
                          )
                  ), #end header 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data summary", tabName = "summary", icon = icon("chart-simple")),
      menuItem("Species results", tabName = "species", icon = icon("crow")),
      menuItem("Mapping", tabName = 'mapping', icon = icon('location-dot')),
      tags$hr(),
      tags$div('Set date and time format of your',tags$br(), 'loaded csv here', style = 'padding-left:5px;'),
      radioButtons("timeformat", "Time Format",
                 choices = c('H:M:S'= "%H:%M:%S",
                             "H.M.S" = '%H.%M.%S',
                             "HMS" = "%H%M%S"),
                 selected = "%H:%M:%S"),
      radioButtons("dateformat", "Date Format",
                 choices = c('D/M/YYYY'= "%d/%m/%Y",
                             "D.M.YYYY" = "%d.%m.%Y",
                             "D/M/YY" = "%d/%m/%y",
                             "M/D/YYYY" = "%m/%d/%Y",
                             "YYYY-M-D" = "%Y-%m-%d"),
                 selected = "%Y-%m-%d")
  )), #end sidebar
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style("#mapbox{height:600px !important;}
                 #mymap{height:550px !important;")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "summary",
              tags$h1('Survey overview'),
              fluidRow(valueBoxOutput('species'),
                       valueBoxOutput('bocc'),
                       valueBoxOutput('annex2')),
              fluidRow(
                box(title = 'Summary', status = 'success', solidHeader = TRUE, width = 12,
                    htmlOutput('datasummary')
                    )
              ),
              fluidRow(
                box(status = 'success',
                  tableOutput("contents"), #table of observations
                    tags$caption('Note: Some bats are protected under Annex 2.', tags$br(),
                    'BOCC stands for Birds of Conservation Concern. Birds in this list can be listed as Amber or Red (highest concern)',
                    tags$br(),
                    'Schedule 1 refers to certain bird species that are protected by law'
                    )
                    ),
                box(status = 'success',
                    tags$div('Number of detections for each species:'),
                  selectInput('freqgroup', 'Filter by threatened status',
                                choices = c('All species' = 'empty',
                                            'All protected species' = 'None',
                                            'Birds of Conservation Concern - Amber list'= 'AmberBOCC',
                                            'Birds of Conservation Concern - Red list' = 'RedBOCC',
                                            'Schedule 1 birds' = 'Schedule1',
                                            'Annex 2 Bats' = 'Annex2'
                                ),
                                selected = 'empty'),
                  plotOutput('freqplot'),#frequency, coloured by taxa)
                  tags$div('Cumulative number of detections over the survey period for protected and non protected species:'),
                  plotOutput('cumsum')
              )
              )
      ), #end summary tab
      
      # Second tab content
      tabItem(tabName = "species",
              h2("Explore species activity"),
              fluidRow(
                box(status = 'success',
                    selectInput('species',
                                label = 'Choose a species',
                                choices = unique(df$ENGLISH.NAME))
                ),
              ),
              fluidRow(
                box(status = 'success', title = 'Daily activity pattern',
                    plotOutput('hourlyactivity'),
                    tags$caption('Dashed lines represent approximate sunrise and sunset times')
                ),
                box(status = 'success', title = 'Detections over time',
                    plotOutput('dailyactivity')
                    )
                )
              ), #end species tab
      tabItem(tabName = 'mapping',
              h2('Site map'),
              tags$div('Here you can explore the distribution of survey points'),
              box(id = 'mapbox', leafletOutput('mymap'), width = 12
                  )
              ) # end mapping tab
    )
  ) #end dashboard body
) # end ui

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #hold csv in server memory
  ItemDF = reactive(
    if(is.null(input$file1)){
      return(df2)
    } else {
      req(input$file1)
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        })
      df2 = df[df$SPECIES != 'No ID',]
    }
  ) #end reactive
  
  output$species <- renderValueBox({
    df = ItemDF()
    speciesno = unique(df$SCIENTIFIC.NAME)
    valueBox(
      length(speciesno),
      'Species detected',
      color = 'green', 
      icon =  icon('crow'))
  })
  output$bocc <- renderValueBox({
    df = ItemDF()
    boccno = unique(df$SCIENTIFIC.NAME[grepl('BOCC', df$protection)])
    valueBox(
      length(boccno),
      'Birds of conservation concern',
      color = 'green', 
      icon =  icon('triangle-exclamation'))
  })
  output$annex2 = renderValueBox({
    df = ItemDF()
    annexno = unique(df$SCIENTIFIC.NAME[df$protection=='Annex2'])
    valueBox(
      length(annexno),
      'Annex 2 bat species',
      color = 'green', 
      icon =  icon('triangle-exclamation'))
  })
  output$contents <- renderTable({
    #req(input$file1)
    df = ItemDF()
    sumtab = df %>%
      group_by('Taxa' = SPECIES.GROUP, 'Common Name' = ENGLISH.NAME, 'Latin Name' = SCIENTIFIC.NAME, 'Protection Level' = protection) %>%
      summarise(Frequency = n())
    return(sumtab)
  }) #end render table
  
  output$datasummary = renderText({
    #req(input$file1)
    df2 = ItemDF()
    dateform = input$dateformat
    df2$ACTUAL.DATE = as.Date(df2$ACTUAL.DATE, format = dateform)
    date1 = min(df2$ACTUAL.DATE)
    date2 = max(df2$ACTUAL.DATE)
    species = unique(df2$SCIENTIFIC.NAME)
    group = unique(df2$SPECIES.GROUP)
    protected = unique(df2$SCIENTIFIC.NAME[df2$protectedBIN=='Protected'])
    str1 = paste('You have uploaded', nrow(df2),'detections between', date1, 'and', date2, '.')
    str2 = paste('There are', length(species), 'species in your dataset, from ', length(group), 'taxonomic groups.')
    str3 = paste('This app found', length(protected), 'species that are protected under UK law.')
    str4 = paste('Look at the figures below to see which species we found!')
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  }) #end datasummary
  
  
  output$freqplot = renderPlot({
    #req(input$file1)
    df = ItemDF()
    if (input$freqgroup == 'empty') {
      sumtab = df %>%
        group_by(SPECIES.GROUP, ENGLISH.NAME, SCIENTIFIC.NAME) %>%
        summarise(Frequency = n())  
    } else if (input$freqgroup == 'None') {
      sumtab = df %>%
        filter(!(protection %in% input$freqgroup)) %>%
        group_by(SPECIES.GROUP, ENGLISH.NAME, SCIENTIFIC.NAME) %>%
        summarise(Frequency = n())
    } else {
      sumtab = df %>%
        filter(protection %in% input$freqgroup) %>%
        group_by(SPECIES.GROUP, ENGLISH.NAME, SCIENTIFIC.NAME) %>%
        summarise(Frequency = n())
    }
    
    ggplot(sumtab, aes(x = reorder(ENGLISH.NAME, -Frequency), y=Frequency, fill = SPECIES.GROUP)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      xlab('Common Name') +
      theme_bw() +
      scale_fill_wilding()+ #this will work up to 6 groups
      theme(legend.title = element_blank())
  }) #end renderPlot
  
  output$cumsum = renderPlot({
    #req(input$file1)
    df2 = ItemDF()
    df2$ACTUAL.DATE = as.Date(df2$ACTUAL.DATE, format = input$dateformat)
    cumu = df2 %>% group_by(protectedBIN, ACTUAL.DATE) %>% 
      summarise(n = n()) %>%
      mutate(n = cumsum(n))
      #get the number of observations per day
    ggplot(cumu, aes(x = ACTUAL.DATE, y = n, fill = protectedBIN)) +
      geom_col(position = position_dodge2(0.3)) +
      labs(y = 'Cumulative number of detections', x = 'Date') +
      scale_x_date(date_breaks = "1 week", date_minor_breaks = '1 day',
                   date_labels = "%d-%b") +
      theme_bw() +
      scale_fill_wilding() +
      theme(legend.title = element_blank())
  }) #end cumsum
  

  output$hourlyactivity = renderPlot({
    #req(input$file1)
    df = ItemDF()
    sub = df[df$ENGLISH.NAME==input$species,]
    
    sub$timestamp <- strptime(sub$TIME, format=input$timeformat)
    sub$hours <-  as.numeric(format(sub$timestamp, format="%H"))
    ggplot(data=sub) + 
      geom_histogram(aes(x=hours, y = ..density..), fill = '#95C11F') +
      geom_density(aes(x=hours), alpha = 0.2) +
      xlim(0,24) +
      xlab('Time of day') +
      ylab('Density of observations') +
      theme_bw() +
      scale_fill_wilding() +
      geom_vline(aes(xintercept = 5), linetype = 'dashed') +
      geom_vline(aes(xintercept = 8), linetype = 'dashed') +    
      geom_vline(aes(xintercept = 18), linetype = 'dashed') +
      geom_vline(aes(xintercept = 20), linetype = 'dashed')
  }) # end hourly activity
  
  output$dailyactivity = renderPlot({
    #req(input$file1)
    df = ItemDF()
    df$ACTUAL.DATE = as.Date(df$ACTUAL.DATE, format = input$dateformat)
    sub = df[df$ENGLISH.NAME==input$species,]
    
    ggplot(data=sub) + 
      geom_histogram(aes(x=ACTUAL.DATE), stat = 'count', fill = '#95C11F') +
      xlab('Date') +
      scale_x_date(date_breaks = "1 week", date_minor_breaks = '1 day',
                   date_labels = "%d-%b") +
      theme_bw()
  }) # end hourly activity
  
  output$mymap = renderLeaflet({
    countdata = ItemDF() %>%
      group_by(LONGITUDE,LATITUDE) %>%
      summarise(nspecies = n_distinct(SCIENTIFIC.NAME), specieslist = paste(ENGLISH.NAME, collapse = '<br>'))
    
    leaflet(countdata) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addCircleMarkers(~LONGITUDE, ~LATITUDE, radius = ~nspecies,
                       label = ~paste("Total species detected =", nspecies),
                       popup = ~specieslist) #label = hover over, popup = click on
     #label = hover over, popup = click on
  })
  
} #end server function

# Run the application 
shinyApp(ui = ui, server = server)
