library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(glue)
library(googlesheets4)
options(shiny.autoreload = TRUE)

gs4_deauth()


# Read in the TSV file
data <-
    read_sheet(
        "https://docs.google.com/spreadsheets/d/1tzEXVleYYPfgmKesHbPilGaSYGloldkG4oQY_jn7UVw/edit?usp=sharing",
        col_types = "Dccccc"
        )

# change layout to do rows
ui <- fluidPage(# Application title
    titlePanel("COVID-19 Pandemic Lockdown Takeout"),
    
    fluidRow(column(
        4,
        sidebarPanel(
            selectInput(
                "select",
                label = p("Select y-axis for bar plot:"),
                choices = colnames(data)[-1],
                selected = "Restaurant"
            ),
            dateRangeInput(
                "dates",
                label = p("Filter table by date range:"),
                start = "2020-03-12",
                end = "2020-12-31"
            ),
            width = 12
        )
    ),
    column(8,
           plotOutput("plot")
           )
    ),
    fluidRow(
        column(6, plotOutput("timeplot")),
        column(6, DT::dataTableOutput("table"))
    )
)

# Define server logic
server <- function(input, output) {
    output$table <- renderDataTable({
        arrange(data, Date) %>%
            filter(Date >= input$dates[1],
                   Date <= input$dates[2])
    })
    output$plot <- renderPlot({
        data %>%
            mutate(fct = factor(!!sym(input$select)
            ) %>%
            fct_infreq() %>%
            fct_rev()) %>%
            drop_na(!!sym(input$select)) %>%
            ggplot(aes(fct)) +
            geom_bar() +
            coord_flip() +
            labs(x = input$select,
                 title = glue("Breakdown by {input$select}")) +
            theme_minimal() +
            theme(legend.position = "none")
    })
    output$timeplot <- renderPlot({
        data %>%
            mutate(month = month(Date, label = TRUE)) %>%
            filter(Date >= input$dates[1],
                   Date <= input$dates[2]) %>%
            arrange(month) %>%
            count(month) %>% 
            ggplot(aes(month, n, group = 1)) +
            geom_line() +
            geom_point() +
            labs(x = "Month", y = "count") +
            theme_bw()
    })
}

# Run the application
shinyApp(ui = ui, server = server)