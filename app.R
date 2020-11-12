library(shiny)
library(dplyr)
library(readr)

weather <- read_csv("Weather.csv")
weather$`Precipitation (inch)` <- gsub('-', NA, weather$`Precipitation (inch)`, fixed=TRUE)
weather$`Snow (inch)` <- gsub('-', NA, weather$`Snow (inch)`, fixed=TRUE)
weather$`Snow Depth` <- gsub('-', NA, weather$`Snow Depth`, fixed=TRUE)
weather$`Precipitation (inch)` <- as.numeric(weather$`Precipitation (inch)`)
weather$`Snow (inch)` <- as.numeric(weather$`Snow (inch)`)
weather$`Snow Depth` <- as.numeric(weather$`Snow Depth`)
weather$Month <- factor(weather$Month, 
                        levels = c("December", "November", "October", 
                                   "September", "August", "July", "June", 
                                   "May", "April", "March", "February", "January"))
weather <- as.data.frame(weather)

weather.clean <- weather %>% filter(complete.cases(weather))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weather in Brookings, SD in 2019"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("cid", "Column", 
                        choices = colnames(weather.clean[,3:7])),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 15)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- weather.clean[,input$cid]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        titl <- paste("Histogram of", input$cid, sep = " ")
        h <- hist(as.numeric(x), breaks = bins, col = rainbow(10), border = 'white',
                  main = titl, xlim = c(min(x), max(x)), xlab = input$cid)
        yfit <- dnorm(bins, mean=mean(x), sd = sd(x))
        yfit <- yfit * diff(h$mids[1:2]) * length(x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
