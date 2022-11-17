#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring confidence intervals"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("nobs1",
                        "Sample size",
                        min = 10,
                        max = 500,
                        value = 50,
                        step = 5),
            sliderInput("nsamp",
                        "Number of simulations",
                        min = 10,
                        max = 100,
                        value = 50,
                        step = 5),
            sliderInput("level1",
                        "Confidence level",
                        min = 0.7,
                        max = 0.99,
                        value = 0.9,
                        step = 0.01),
            sliderInput("popmean1",
                        "Population mean",
                        min = -2,
                        max = 2,
                        value = 1,
                        step = 0.25),
            sliderInput("popsd1",
                        "Population variance",
                        min = 0.1,
                        max = 5,
                        value = 1,
                        step = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlot({
      tibble(id = paste('sample', 1:input$nsamp, sep = ' '),
                     idnum = 1:input$nsamp) %>%
        mutate(data = map(id, ~ rnorm(input$nobs1, 
                                      mean = input$popmean1,
                                      sd = input$popsd1)),
               ttest = map(data, ~t.test(.x, conf.level = input$level1)),
               ci = map(ttest, ~.x$conf.int)) %>%
        unnest(ci) %>%
        mutate(bound = rep(c('lwr', 'upr'), input$nsamp)) %>%
        pivot_wider(id_cols = c(id, idnum), 
                    names_from = bound, 
                    values_from = ci) %>%
        mutate(covers = as.logical((lwr < input$popmean1)*(upr > input$popmean1))) %>%
        ggplot(aes(y = reorder(id, idnum))) +
        geom_errorbarh(aes(xmin = lwr, xmax = upr, color = covers)) +
        geom_vline(xintercept = input$popmean1, 
                   linetype = 'dashed', 
                   color = 'red') +
        theme_minimal() +
        xlim(c(-5, 5)) +
        labs(y = '', x = expr(hat(theta))) +
        guides(color = guide_none())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
