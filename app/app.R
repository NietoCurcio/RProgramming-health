library(tidyverse)
library(shiny)
library(leaflet)
library(geobr)
library(shinydashboard)

source("./global.R")

header <- dashboardHeader(title = "VAX*SIM - Study")

sidebar <- dashboardSidebar(
  sidebarMenu(
      menuItem("Interactive map", tabName = "interactiveMap"),
      menuItem("Data explorer", tabName = "dataExplorer"),
      menuItem("BoxPlot", tabName = "boxPlot")
    )
)

body <- dashboardBody(
  tags$header(
    includeCSS("www/styles.css"),
  ),
  tabItems(
    tabItem(
      tabName = "interactiveMap",
      tags$main(
        class = "map-content",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          right = 20,
          width = 330,
          h2("VAX*SIM"),
          selectInput(
            "indicator",
            "Indicator",
            choices = c(
              "Vaccination" = "total_vaccinated",
              "Breastfeeding" = "breastfeeding_complete"
            )
          ),
          selectInput(
            "year",
            "Year",
            choices = c(2020, 2019)
          ),
          selectizeInput(
            inputId = "muni",
            label = "Municipality",
            choices = c("Brasília"),
          ),
          # plotOutput("histMuniStats", height = 200),
          plotOutput("muniStats", height = 250),
        )
      )
    ),
    tabItem(
      tabName = "dataExplorer",
      DT::DTOutput("table")
    ),
    tabItem(
      tabName = "boxPlot",
      plotOutput("boxplot")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

showMuniPopup <- function(muni, lat, lng, indicator) {
  content <- str_glue(
    "
    <h4>Muni: {muni$name_muni}</h4>
    <strong>{indicator}: {muni[[indicator]]}</strong>
    <br>
    <span>Total population: {muni$total_population}</span>
    "
    ) %>% HTML

  leafletProxy("map") %>% addPopups(lng = lng, lat = lat, popup = content, layerId = muni$name_muni)
}

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = -14, lng = -51, zoom = 4)
  })

  reactiveDataByYear <- reactive({
    data %>% filter(year == input$year)
  })

  reactiveDataByMuni <- reactive({
    data %>% filter(name_muni == input$muni)
  })

  observe({
    indicator <- input$indicator

    data <- reactiveDataByYear()

    if (indicator == "Breastfeeding") {
      colorData <- data$breastfeeding_complete
      pal <- colorBin("viridis", colorData)
    } else {
      colorData <- data$total_vaccinated
      pal <- colorNumeric("viridis", colorData)
    }

    leafletProxy("map", data = data) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(
        weight = 2,
        fillOpacity = 0.7,
        dashArray = "3",
        fillColor = ~pal(colorData),
        layerId = ~name_muni
      ) %>% addLegend(
        position = "bottomleft",
        pal = pal,
        values = colorData,
        title = indicator,
      )
  })

  output$muniStats <- renderPlot({
    data <- reactiveDataByMuni() %>% select(!geom)
    ggplot(data = data, aes_string(x = "year", y = input$indicator)) +
      geom_line() +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
      )
  })

  observe({
    updateSelectizeInput(
      session = session,
      inputId = "muni",
      choices = data %>% pull(name_muni) %>% unique() %>% sort(),
      server = TRUE
    )
  })

  observe({
    event <- input$map_shape_click

    leafletProxy("map") %>% clearPopups()

    if (is.null(event)) return()

    isolate({
      muni <- reactiveDataByYear() %>% filter(name_muni == event$id)

      showMuniPopup(muni, event$lat, event$lng, input$indicator)
    })
  })

  ## Boxplot -----

  output$boxplot <- renderPlot({
    ggplot(reactiveDataByYear(), aes(x = name_region, y = total_vaccinated)) +
      geom_jitter(aes(color = total_population), alpha = 0.5) +
      geom_boxplot(alpha = 0.5) +
      ggtitle("Population vaccinated") +
      xlab(paste("Vaccinated -", input$year)) +
      theme(
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")
      )
  })

  ## Table -----

  output$table <- DT::renderDT({
    sample_data <- head(data, 100)

    DT::datatable(
      data = sample_data,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
