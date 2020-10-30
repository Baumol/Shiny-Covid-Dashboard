

library(shiny)
library(shinydashboard)
library(ggthemes)
library(shinythemes)
library(sf)
library(tidyverse)
library(viridis)



load("covid_state_data.rda")
load("mapdaten.rda")



ui<-fluidPage(titlePanel(
    h1("Corona Dashboard", align = "center")), theme = shinytheme("cerulean"),
    tags$footer("von Philipp Conrad", align = "center"),
    fluidRow(
        column(4, box(width =12,plotOutput("distPlot"),title ="Die Bundesländer im Vergleich",solidHeader = T)),
        column(4, box(width =12,plotOutput("one"),title = "Entwicklung im ausgewählten Bundesland")),
        column(4, box(width =12,plotOutput("two"), title = "Aktuelle Deutschlandkarte"))),
    hr(),
    fluidRow(
        column(4, selectInput("x", "X-Axis", choices = names(covid_state_data),selected = "date"),
               selectInput("y","Y-Axis", choices = names(covid_state_data), selected = "cases_vs_population")),
        column(4, selectInput("z",label = "Bundesland", choices = unique(covid_state_data$state), selected = "Bayern")
               , selectInput("bot","Y-Axis", choices = names(covid_state_data), selected = "totalcases")),
        column(4,selectInput("s",label="Variabel", choices=names(covid_state_data), selected ="totalcases"),
               selectizeInput("t", label = "Datum", choices = unique(covid_state_data$date), selected = "2020-10-24", multiple = FALSE,
                              options = NULL))
    ),
    hr(),
    fluidRow(
        DT::dataTableOutput("mytable")
    ),
    tags$footer("von Philipp Conrad", align = "right")
)


server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        ggplot(covid_state_data,aes_string(x = input$x,y=input$y))+
            geom_point(aes(color =state))+
            labs(color="")+
            theme_minimal()
        
    })
    output$one <- renderPlot({
        covid_state_data %>%
            filter(state == input$z) %>% 
            ggplot(covid_state_data,mapping=aes(x=date))+
            geom_point(aes_string(y=input$bot))+
            labs(color="")+
            theme_minimal()+
            theme(legend.position = "none")
        
    })
    output$two <- renderPlot({
        mapdaten %>% 
            filter(date == input$t) %>% 
            ggplot(aes_string(fill = input$s)) +
            geom_sf()+
            scale_fill_viridis()+
            theme_map()+
            theme(legend.background = element_blank(), legend.position = "right")
    })
    output$mytable = DT::renderDataTable({
        covid_state_data})
}


# Run the application 
shinyApp(ui = ui, server = server)

