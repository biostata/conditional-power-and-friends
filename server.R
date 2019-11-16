library(shiny)
library(tidyverse, warn.conflicts = FALSE)

source('functions.R')

server <- function(input, output) {

    output$expectedPowerPlot <- renderPlot({

        crit      <- qnorm(.975)
        prior     <- Normal(input$priorMean1, input$priorSd1)
        cprior    <- condition(prior, input$mrv1)
        ep        <- expected_power(prior, input$sampleSize1, crit, input$mrv1)
        pow       <- power(input$mrv1, input$sampleSize1, crit)
        tbl_power <- tibble(
            Delta  = seq(-.25, 1, by = .001),
            cprior = pdf(cprior, Delta),
            power  = power(Delta, input$sampleSize1, crit)
        )
        arg_power <- tbl_power$Delta[which.min(abs(tbl_power$power - pow))]
        arg_ep    <- tbl_power$Delta[which.min(abs(tbl_power$power - ep))]
        ggplot(tbl_power) +
            aes(Delta, power) +
            geom_ribbon(aes(ymin = 0, ymax = 1), fill = 'red', alpha = .1, data = tbl_power %>% filter(Delta < 0)) +
            geom_ribbon(aes(ymin = 0, ymax = 1), fill = 'green', alpha = .1, data = tbl_power %>% filter(Delta >= input$mrv1)) +
            geom_line(aes(alpha = cprior)) +
            geom_segment(x = -.25, xend = arg_power, y = pow, yend = pow) +
            geom_segment(x = -.25, xend = arg_ep, y = ep, yend = ep) +
            geom_point(x = arg_power, y = pow) +
            geom_point(x = arg_ep, y = ep) +
            coord_cartesian(xlim = range(tbl_power$Delta), ylim = c(0, 1), expand = FALSE) +
            theme_bw()

    })

    output$powerDistributionPlot <- renderPlot({

        crit  <- qnorm(.975)
        smpl  <- rnorm(10^5, input$priorMean1, input$priorSd1)
        smpl  <- smpl[smpl > input$mrv1]
        hist(power(smpl, input$sampleSize1, crit))

    })

}
