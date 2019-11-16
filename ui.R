library(shiny)

ui <- navbarPage("(Conditional) Power and Friends",
    tabPanel("Background",
        withMathJax(includeMarkdown('readme.md'))
    ),
    tabPanel("Explore!",
        sidebarLayout(
            sidebarPanel(
                sliderInput('mrv1', label = 'MRV', value = .0, min = 0, max = 1, step = .01),
                sliderInput('priorMean1', label = 'prior mean', value = .4, min = 0, max = 1, step = .01),
                sliderInput('priorSd1', label = 'prior sd', value = .1, min = .01, max = 5, step = .01),
                sliderInput('sampleSize1', label = 'n', value = 49, min = 10, max = 250, step = 1),
                uiOutput('slider'),
                sliderInput('sampleDelta', label = 'Delta, sample', value = .4, min = -1, max = 1, step = .01),
                actionButton('refreshHistogram', label = 'refresh conditional power sample', width = '100%')

            ),
            mainPanel(
                tabsetPanel(
                    tabPanel('Unconditional',
                         plotOutput('expectedPowerPlot')
                    ),
                    tabPanel('Conditional',
                        plotOutput('conditionalPowerPlot'),
                        plotOutput('conditionalPowerHistogramPlots')
                    )
                )
            )
        )
    )
)
