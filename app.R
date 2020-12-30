library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(glue)
library(googlesheets4)
library(rlang)
library(plotly)

options(shiny.autoreload = TRUE)

gs4_deauth()


# Read in the TSV file
data <-
    read_sheet(
        "https://docs.google.com/spreadsheets/d/1tzEXVleYYPfgmKesHbPilGaSYGloldkG4oQY_jn7UVw/edit?usp=sharing",
        col_types = "Dccccc"
        )


jscode <- "shinyjs.refresh = function() { history.go(0); }"
ui <- fluidPage(# Application title
    shinydisconnect::disconnectMessage2(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("refresh")),
    titlePanel("COVID-19 Pandemic Lockdown Takeout"),
    fluidRow(column(
        4,
        sidebarPanel(
            selectInput(
                "select",
                label = p("Select y-axis for bar plot:"),
                choices = as_tibble(colnames(data)) %>%
                    filter(value != "Date") %>%
                    pull(value),
                selected = "Restaurant"
            ),
            # sliderInput("slider", label = h3("Select x-axis range for bar plot"), min = 0, max = 20, value = c(0, 20)),
        # find a way to make this 20 dynamic
            checkboxInput("colourBy", label = "Breakdown by colour", value = FALSE),
            conditionalPanel(
                condition = "input.colourBy == true",
                uiOutput("colourControls")
            ),
            dateRangeInput(
                "dates",
                label = p("Filter by date range:"),
                start = "2020-03-12",
                end = "2020-12-31"
            ),
            checkboxGroupInput(
                "types",
                label = p("Filter by type:"),
                choices = unique(pull(data, Type)),
                selected = "Food"
            ),
            fluidRow(
                column(6, actionButton("refresh", "Reset", icon = icon("refresh")), align = 'left'),
                column(6, downloadButton("download", "Download"), align = 'right'),
                width = 12
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
    observeEvent(input$refresh, {
        shinyjs::js$refresh()
    })
    output$colourControls <- renderUI({
        colourby <- as_tibble(colnames(data)) %>%
            filter(value != input$select, !value %in% c("Date", "Type")) %>%
            pull(value)
        selectInput(
            "colour",
            label = p("Colour bars by:"),
            choices = rev(colourby),
            selected = colourby[0]
        )
    })
    
    filtered <- reactive({
        filtered <- data
        if (!is.null(input$dates[1]) && !is.null(input$dates[2])) {
            filtered <- filtered %>%
                filter(Date >= input$dates[1],
                       Date <= input$dates[2])
        }
        if (!is.null(input$types)) {
            filtered <- filtered %>%
                filter(Type %in% input$types)
        }
        # if(is.null(input$types)) {
        #     return(NULL)
        # }
        
        filtered
    })
    output$table <- renderDataTable({
        # if (is.null(filtered())) {
        #     return(NULL)
        # }
        arrange(filtered(), desc(Date))
    })
    output$plot <- renderPlot({
        # if (is.null(filtered())) {
        #     return(NULL)
        # }
        if (input$colourBy == TRUE && !is.null(input$colour)) {
            item_order <- sort(unique(data$Item))
            item_order <- item_order[item_order != "Other"]
            item_order <- c(item_order, "Other")
            plot <- filtered() %>%
                mutate(fct = factor(!!sym(input$select)
                ) %>%
                    fct_infreq() %>%
                    fct_rev(),
                Item = factor(Item, levels = item_order)) %>%
                drop_na(!!sym(input$select)) %>%
                ggplot(aes(fct, fill = !!sym(rlang::as_string(input$colour)))) +
                geom_bar() +
                coord_flip() +
                labs(x = input$select,
                     title = glue("Breakdown by {input$select}")) +
                theme_minimal() +
                theme(text = element_text(size =15))
            # if(!is.null(input$slider[1]) && !is.null(input$slider[2])) {
            #     plot <- plot + ylim(input$slider[1], input$slider[2])
            # }
        } else {
            plot <- filtered() %>%
                mutate(fct = factor(!!sym(input$select)
                ) %>%
                    fct_infreq() %>%
                    fct_rev()) %>%
                drop_na(!!sym(input$select)) %>%
                ggplot(aes(fct)) +
                geom_bar(fill = "deepskyblue2") +
                coord_flip() +
                labs(x = input$select,
                     title = glue("Breakdown by {input$select}")) +
                theme_minimal() +
                theme(legend.position = "none") +
                theme(text = element_text(size =15))
            # if(!is.null(input$slider[1]) && !is.null(input$slider[2])) {
            #     plot <- plot + ylim(input$slider[1], input$slider[2])
            # }
        }
        plot
    })
    
    output$timeplot <- renderPlot({
        # if (is.null(filtered())) {
        #     return(NULL)
        # }
        filtered() %>%
            mutate(month = month(Date, label = TRUE),
                   year = year(Date)) %>%
            arrange(month,year) %>%
            count(month, year) %>% 
            ggplot(aes(month, n, group = 1)) +
            geom_line() +
            geom_point() +
            labs(x = "Month", y = "count") +
            theme_bw()  +
            labs(title = "Time Series", subtitle = glue("{input$dates[1] } to {input$dates[2]}")) +
            theme(text = element_text(size =15)) +
            facet_grid(~ year)
    })
    
    output$download <- downloadHandler(
        filename = function() {
            "covid-takeout.csv"
        },
        content = function(con) {
            write_csv(data, con)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)