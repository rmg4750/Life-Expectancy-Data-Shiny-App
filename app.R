library(shiny)
library(tidyverse)
library(renv)
library(readxl)
library(plotly)

life_exp<- read.csv(paste("Life_Expectancy_00_15.csv"), sep = ";")
          
ui <- fluidPage(
    
    titlePanel("Plot Any Variable vs. Any Variable"),
    
    sidebarLayout(
    
        sidebarPanel(
            selectInput(
                        inputId = "select_country",
                        label = "Choose Country",
                        choices = life_exp$Country),
            varSelectInput(
                        inputId = "select_y_variable",
                        label = "Choose Y Variable",
                        data = life_exp),
            varSelectInput(
                        inputId = "select_x_variable",
                        label = "Choose X Variable",
                        data = life_exp),
            
        ),
    
        mainPanel(
            plotlyOutput("plot")
        ),
        position = "left"
    )
)

server <- function(input, output, session) {

    data <- reactive({
        req(input$select_country)
        df <- life_exp %>% filter(Country %in% input$select_country)
    })

    observe({
        updateSelectInput(session, "select_country", choices = life_exp$Country)
        updateSelectInput(session, "select_x_variable", choices = c("Year", "Life.Expectancy", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
        updateSelectInput(session, "select_y_variable", choices = c("Life.Expectancy", "Year", "Population", "CO2.emissions", "Health.expenditure", "Electric.power.consumption", "Forest.area", "GDP.per.capita", "Individuals.using.the.Internet", "Military.expenditure", "People.practicing.open.defecation", "People.using.at.least.basic.drinking.water.services", "Obesity.among.adults", "Beer.consumption.per.capita" ))
    })

    output$plot <- renderPlotly({
        ggplotly(
            ggplot(data(), aes_string(x = input$select_x_variable, y = input$select_y_variable)) 
            + geom_point() 
            + geom_smooth(method = "lm", formula = y ~ x, fill = NA) 
            + theme_classic()
            + labs(caption = "Click on graph to interact")
        )
    })
}

shinyApp(ui = ui, server = server)
