library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(viridis)
library(dplyr)
library(shinyBS)

options(warn = -1)
ui <- dashboardPage(skin = 'purple',
dashboardHeader(title = strong('Wireframe Heatmap')),
dashboardSidebar(disable = TRUE),
dashboardBody(
  useShinyjs(),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  fluidPage(
    column(width = 12, fileInput('primeData', label = h3('Please upload the Prime Item Data for the category of your choice:'), width = '100%')),
    # column(width = 4, fileInput('nodeMap', label = h3('Please Upload the Node Mapping File:'), width = '100%')),
    # column(width = 4, fileInput('clusterMap', label = h3('Please Upload the Cluster Mapping File:'), width = '100%')),
    # column(width = 4, fileInput('salesMap', label = h3('Please Upload the Cluster-Sales Mapping File:'), width = '100%')),

    column(width = 4, selectInput('cluster', label = h3('Please select a Cluster to view its corresponding Wireframe Heatmap'), choices = list(), width = '100%')),
    column(width = 4, selectInput('strCnt', label = h3('Please select a Variant of Store Count you would like to use for the Analysis'), choices = list(), width = '100%')),
    column(width = 4, selectInput('shades', label = h3('Please select a Color Scheme for the Wireframe Heatmap View'), choices = c('Viridis', 'Magma', 'Inferno', 'Plasma'), selected = 'Viridis', width = '100%')),
    column(width = 12, 
           h1('Wireframe Heatmap with darkest shade representing highest Performance Index value:'),
           plotOutput('wireframe', height = '700px', click = 'plotClick')),
    div(id = 'afterDataLoad', fluidPage(
      column(width = 12, h1(' ')),
      column(width = 1, 'Footnote:'),
      column(width = 11, h4('Color Scale for Performance Index used in the Wireframe Heatmap', align = 'center'),
             fluidRow(
               column(width = 1, h3('Low', align = 'right')),
               column(width = 10, plotOutput('colorScale', height = '50px', width = '100%')),
               column(width = 1, h3('High', align = 'left'))
             )),
      # column(h3('Low')),
      # column(width = 9, h4('Color Scale for Performance Index used in the Wireframe Heatmap', align = 'center'),
      #        plotOutput('colorScale', height = '30px', width = '100%')),
      # column(h3('High')),
      column(width = 12, h1(' ')),
      column(width = 12, h1(' ')),
      column(width = 12, h3(strong('Select a Node in the Wireframe to view its Items'))),
      div(id = 'table', {
        fluidPage(
          column(width = 12, 
                 sliderInput('storeSlider', label = h4('Please input a Store Count cutoff'), min = 0, max = 5000, step = 10, value = 0, width = '100%'),
                 dataTableOutput('itemProps'),
                 h1('')),
          column(width = 2, offset = 8, 
                 actionButton('save', label = strong('Save your Selection'), icon = icon('save'), width = '80%'),
                 bsTooltip(id = 'save', title = 'Your selection is Successfully saved', trigger = 'click', placement = 'top')),
          column(width = 2, downloadButton('download', label = strong('Download Saved Data')))
          )
      })
    ))
  )
))

