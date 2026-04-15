
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

# Lecture 03
source("modules/lecture_03/frequency-shifting.R")
source("modules/lecture_03/amplitude-modulation.R")

# Lecture 04
source("modules/lecture_04/passband-convolution.R")

# Lecture 05
source("modules/lecture_05/sampling-fourier-transform.R")
source("modules/lecture_05/sampling-aliasing.R")

# Lecture 06
source("modules/lecture_06/dft-basis.R")
source("modules/lecture_06/dft-epicycle.R")
source("modules/lecture_06/linear-circular-convolution.R")

# Lecture 07
source("modules/lecture_07/dct-ii.R")
source("modules/lecture_07/dct-basis-components.R")
source("modules/lecture_07/jpeg-compression.R")

# Lecture 08
source("modules/lecture_08/time-resolution-motivation.R")
source("modules/lecture_08/wft-explorer.R")
source("modules/lecture_08/wft-spectrogram.R")
source("modules/lecture_08/gabor-spectrogram.R")

# Lecture 09
source("modules/lecture_09/stft-vs-cwt.R")
source("modules/lecture_09/mra-pyramid.R")

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
               
               tabPanel("Lecture 03",
                        tabsetPanel(
                            tabPanel("Exponential Sinusoids", frequencyShiftingAppUI("freqShift1")),
                            tabPanel("Amplitude Modulation", amplitudeModulationAppUI("amMod1"))
                        )
               ),
               
               tabPanel("Lecture 04",
                        tabsetPanel(
                            tabPanel("Passband LTI", passbandLtiAppUI("passbandLti1"))
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
                            tabPanel("DFT Epicycles", dftEpicycleAppUI("dft2")),
                            tabPanel("Linear vs Circular Convolution", linearCircularConvAppUI("linCircConv1"))
                        )
               ),
               
               tabPanel("Lecture 07",
                        tabsetPanel(
                            tabPanel("DCT II", dctIiAppUI("dct1")),
                            tabPanel("DCT Basis Components", dctBasis2dAppUI("dct2d")),
                            tabPanel("JPEG Compression", jpegCompressionAppUI("imgDct"))
                        )
               ),
               
               tabPanel("Lecture 08",
                        tabsetPanel(
                            tabPanel("Time Resolution", fourierTransformBlocksAppUI("timeResolution")),
                            tabPanel("WFT Explorer", windowedFourierTransformAppUI("wftExplorer")),
                            tabPanel("STFT Viewer", wftViewerAppUI("stftViewer")),
                            tabPanel("Gabor Transform", manualSpectrogramAppUI("gaborTransform"))
                        )
               ),
               
               tabPanel("Lecture 09",
                        tabsetPanel(
                            tabPanel("STFT vs CWT", timeFrequencyAnalysisAppUI("stftCwt")),
                            tabPanel("MRA Decomposition", waveletDecompositionAppUI("mraPyramid"))
                        )
               )
               # Additional lectures can go here...
    )
)

server <- function(input, output, session) {
    
    # Lecture 01
    eulerCircleAppServer("circle1")
    eulerWaveAppServer("wave1")
    
    # Lecture 02
    fourierExplorerAppServer("fourier1")
    spectrumViewerAppServer("spectrum1")
    
    # Lecture 03
    frequencyShiftingAppServer("freqShift1")
    amplitudeModulationAppServer("amMod1")
    
    # Lecture 04
    passbandLtiAppServer("passbandLti1")
    
    # Lecture 05
    sampledFftAppServer("fft1")
    aliasingAppServer("alias1")
    
    # Lecture 06
    dftBasisAppServer("dft1")
    dftEpicycleAppServer("dft2")
    linearCircularConvAppServer("linCircConv1")
    
    # Lecture 07
    dctIiAppServer("dct1")
    dctBasis2dAppServer("dct2d")
    jpegCompressionAppServer("imgDct")
    
    # Lecture 08
    fourierTransformBlocksAppServer("timeResolution")
    windowedFourierTransformAppServer("wftExplorer")
    wftViewerAppServer("stftViewer")
    manualSpectrogramAppServer("gaborTransform")
    
    # Lecture 09
    timeFrequencyAnalysisAppServer("stftCwt")
    waveletDecompositionAppServer("mraPyramid")
}

shinyApp(ui, server)
