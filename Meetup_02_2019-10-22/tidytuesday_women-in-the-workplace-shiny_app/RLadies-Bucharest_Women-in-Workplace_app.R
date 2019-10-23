#rladies #tidytuesday #22oct2019 #v7startupstudio
# as discussed in the first meetup, we agree to vote on a #tidytuesday use case
# movement website here: https://github.com/rfordatascience/tidytuesday
# our voting results here: https://docs.google.com/forms/d/e/1FAIpQLSdryqS57NMXyYH-7dlhofW3S4utK12jDBdTEsYsRAcrakbthg/viewanalytics
# this script was presented during the #2 meetup of R-Ladies Bucharest. A following

#Inspiration from https://www.youtube.com/watch?v=fv9SQ4IFNr4

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

# Define UI for application that draws a graph
ui <- fluidPage(

    # Application title
    titlePanel("Disparity Explorer @ R-Ladies Bucharest Meetup"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("major_category",
                    "Occupation Category",
                        choices=unique(jobs_gender$major_category))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("jobs_scatter", height="500px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$jobs_scatter <- renderPlotly({
        # generate bins based on input$bins from ui.R
        p    <- jobs_gender %>%
            filter(year==2016,
                   total_workers>=2000) %>%
            filter(major_category==input$major_category) %>%
            arrange(desc(wage_percent_of_male)) %>%
            ggplot(aes(workers_female/total_workers,
                       total_earnings_female/total_earnings_male,
                       color=minor_category,
                       size=total_workers,
                       label=occupation
            ))+
            geom_point()+
            scale_size_continuous(range = c(1,10),guide = FALSE)+
            labs(x="Total # workers",
                 y="%of workers reported as female",
                 title="Gender disparity and wage ",
                 color="minor category")+
            scale_x_continuous(labels=percent_format())+
        scale_y_continuous(labels=percent_format())

        ggplotly(p)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
