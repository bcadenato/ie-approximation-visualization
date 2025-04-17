
library(shiny)
library(tidyverse)
library(ggplot2)
library(tuneR)
library(signal)
library(reshape2)
library(gganimate)
library(transformr)
library(gifski)

# Lecture 01
source("modules/lecture_01/euler-circle-app.R")
source("modules/lecture_01/euler-wave-app.R")

# Lecture 02
source("modules/lecture_02/fourier-explorer-app.R")
source("modules/lecture_02/spectrum-viewer-app.R")

# Lecture 05
source("modules/lecture_05/sampling-fourier-transform.R")
source("modules/lecture_05/sampling-aliasing.R")

# Lecture 06
source("modules/lecture_06/dft-basis.R")
source("modules/lecture_06/dft-epicycle.R")

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
               ),
               
               tabPanel("Lecture 05",
                        tabsetPanel(
                            tabPanel("Sampled FFT", sampledFftAppUI("fft1")),
                            tabPanel("Aliasing", aliasingAppUI("alias1"))
                        )
               ),
               
               tabPanel("Lecture 06",
                        tabsetPanel(
                            tabPanel("DFT Basis Vectors", dftBasisAppUI("dft1")),
                            tabPanel("DFT Epicycles", dftEpicycleAppUI("dft2"))
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
    sampledFftAppServer("fft1")
    aliasingAppServer("alias1")
    dftBasisAppServer("dft1")
    dftEpicycleAppServer("dft2")
}

shinyApp(ui, server)
