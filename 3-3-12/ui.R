#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library('shiny')
# Define UI for application that draws charts
fluidPage(

    # Application title
    titlePanel("Gamma Exposure Profile"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bars",
                        "Number of bars:",
                        min = 1,
                        max = 390,
                        value = 5)
        ),

        # Show a highcharter grids
        #mainPanel(column(width = 12, highchartOutput("h",height="500px")))
        uiOutput("h")
    )
)