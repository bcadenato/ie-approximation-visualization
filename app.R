
library(shiny)
library(tidyverse)
library(ggplot2)
library(tuneR)
library(signal)
library(reshape2)

# Load module files
source("modules/lecture_01/euler-circle-app.R")
source("modules/lecture_01/euler-wave-app.R")
source("modules/lecture_02/sound-wave-app.R")

ui <- fluidPage(
    navbarPage("Lecture Visualizations",
               
               tabPanel("Lecture 01",
                        tabsetPanel(
                            tabPanel("Euler's Formula", eulerCircleAppUI("circle1")),
                            tabPanel("Wave Comparison", eulerWaveAppUI("wave1"))
                        )
               ),
               
               tabPanel("Lecture 02",
                        tabsetPanel(
                            tabPanel("Sound Wave Analyzer", soundAppUI("sound1"))
                        )
               )
               
               # Add more lectures here...
    )
)

server <- function(input, output, session) {
    eulerCircleAppServer("circle1")
    eulerWaveAppServer("wave1")
    soundAppServer("sound1")
}

shinyApp(ui, server)
