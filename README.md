# PAMDashboard
A shiny app for displaying results of a PAM survey
The current working version of this can be seen here: 
https://peggybev.shinyapps.io/acousticdashboard/

This script creates a shiny dashboard app that can be used to display detection records from a PAM survey. 
In order to make this work, a csv containing species' detections needs to be in the 'data' folder of this directory. 

Only include one detection csv in the data folder! There should also be a csv called 'threatenedspecies.csv' in 
the data folder: this contains all the information about protection levels of certain species and 
can be updated if a species' status changes.

How to use this script:
Before running this app for the first time, run the script called 'requirements'. This will make sure you have all the correct packages
installed. You only need to do this once. Click Run App on the top right of the script window to get the app to run in the plot viewer or on your browser. 

app.R Script layout:
pre-amble - the first lines of this script load packages, the detections csv and sets the RSK wilding colour palette. These shouldn't need much editing unless
the structure of your input data or required colour palette changes.

UI - This is the section that sets out the layout and appearence of the app
Server - This is the section that builds the plots, gets important statistics from the data, and is passed back to the UI

The final line uses the shinyApp() function to put the UI and server together

Online tools for shiny:
Introduction to shiny using R: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html
Introduction to shinydashboard: https://rstudio.github.io/shinydashboard/get_started.html

Bringing the app online
When you visualise your app in r, it is not a live website. 
To deploy the app online, you can host it at shinyapps.io, or contact your IT department to host it on the company server.

Introduction to shinyapps.io: https://shiny.posit.co/r/articles/share/shinyapps/

If you have successfully uploaded your app and need to update it, just run:
rsconnect::deployApp()
in your console.
