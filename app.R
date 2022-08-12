#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    # titlePanel("Autoregressive Processes"),

    # Sidebar with a slider input for number of bins 
    pageWithSidebar(
        headerPanel("Autoregressive Processes"),
        sidebarPanel(
            numericInput("a","a: Intercept", 1, min = -100, max = 100),
            sliderInput("rho","b: Coefficient*100", 0.005, min = -200, max = 200),
            numericInput("l","t: Time", 0, min = -100, max = 100)
        ),
        mainPanel(
            plotOutput("AR_Plot")
            )
        )
    )
# Define server logic required to draw a histogram
server <- function(input, output) {
    library("latex2exp")

    output$AR_Plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        N    <- 500
        a    <- input$a
        rho  <- input$rho/100
        l    <- input$l
        set.seed(04082022)
        v <- ts(rnorm(N,0,1))*10
        y <- ts(rep(0,N))*100
        
        for (t in 2:N) {
            y[t] <- a + l*time(y)[t] + rho*y[t-1] + v[t]
        }
        
        # draw the histogram with the specified number of bins
        plot(y, type = "l",
             col = "steelblue",
             ylab = "",
             xlab = "Date",
             main = latex2exp("$ y_t = a + t Time_{t} + b y_{t-1} + v_t $"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
