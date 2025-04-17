
library(shiny)
library(tidyverse)
library(ggplot2)
library(tuneR)
library(signal)
library(reshape2)

# Load module files
source("modules/lecture_01/euler-circle-app.R")
source("modules/lecture_01/euler-wave-app.R")
source("modules/lecture_02/fourier-explorer-app.R")
source("modules/lecture_02/spectrum-viewer-app.R")

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
                            tabPanel("Fourier Explorer", fourierExplorerAppUI("fourier1")),
                            tabPanel("Spectrum Viewer", spectrumViewerAppUI("spectrum1"))
                        )
               )
               
               # Additional lectures can go here...
    )
)

server <- function(input, output, session) {
    eulerCircleAppServer("circle1")
    eulerWaveAppServer("wave1")
    fourierExplorerAppServer("fourier1")
    spectrumViewerAppServer("spectrum1")
}

shinyApp(ui, server)
