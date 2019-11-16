library(shiny)

ui <- navbarPage("(Conditional) Power and Friends",
    tabPanel("Background",
        withMathJax(includeMarkdown('readme.md'))
    ),
    tabPanel("Explore!",
        sidebarLayout(
            sidebarPanel(
                sliderInput('mrv1', label = 'MRV', value = .3, min = 0, max = .9, step = .01),
                sliderInput('priorMean1', label = 'prior mean', value = .4, min = 0, max = 1, step = .01),
                sliderInput('priorSd1', label = 'prior sd', value = .1, min = .01, max = 5, step = .01),
                sliderInput('sampleSize1', label = 'n', value = 100, min = 10, max = 500, step = 2)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel('Unconditional',
                         plotOutput('expectedPowerPlot'),
                         plotOutput('powerDistributionPlot')
                    ),
                    tabPanel('Conditional',
                        sliderInput('estimate', label = 'estimate', value = .5, min = -1, max = 1, step = .01),
                        sliderInput('n1', label = 'n1', value = 8, min = 2, max = 500, step = 2),
                    )
                )
            )
        )
    )
)
