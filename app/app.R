library(tidyverse)
library(shiny)
library(leaflet)
library(geobr) # library sf
library(shinydashboard)

# source("./global.R")
source("./app/global.R")

header <- dashboardHeader(title = "observaInfancia - Study")

sidebar <- dashboardSidebar(
  sidebarMenu(
      menuItem("Interactive map", tabName = "interactiveMap"),
      menuItem("Data explorer", tabName = "dataExplorer"),
      menuItem("BoxPlot", tabName = "boxPlot")
    )
)

body <- dashboardBody(
  tags$header(
    # includeCSS("www/styles.css"),
    includeCSS("app/www/styles.css"),
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
          actionButton("reset_map", "Reset map"),
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

showStatePopup <- function(state, lat, lng, indicator) {
  content <- str_glue(
    "
    <h4>State: {state$name_state}</h4>
    <strong>{indicator}: {state[[indicator]]}</strong>
    <br>
    <span>Total population: {state$total_population}</span>
    "
    ) %>% HTML

  leafletProxy("map") %>% addPopups(lng = lng, lat = lat, popup = content, layerId = state$abbrev_state)
}

showMuniPopup <- function(muni, lat, lng, indicator) {
  content <- str_glue(
    "
    <h4>Muni: {muni$name_muni}</h4>
    <strong>{indicator}: {muni[[indicator]]}</strong>
    <br>
    <span>Total population: {muni$total_population}</span>
    "
    ) %>% HTML

  leafletProxy("map") %>% addPopups(lng = lng, lat = lat, popup = content, layerId = state$name_muni)
}

showStateMunicipalities <- function(state, indicator, colorData, pal, bounds) {
   leafletProxy("map", data = state) %>%
      clearShapes() %>% clearControls() %>%
      fitBounds(lng1 = bounds[["xmin"]], lat1 = bounds[["ymin"]], lng2 = bounds[["xmax"]], lat2 = bounds[["ymax"]]) %>%
      addPolygons(
        weight = 2,
        fillOpacity = 0.7,
        dashArray = "3",
        fillColor = pal(colorData),
        # v1. pra facilitar, apenas lidar com o evento de click para mostrar um estado
        # mostrar dados de um municipio apenas com o label
        # layerId = ~paste0("muni-", name_muni)
        # mais tarde sera utilizado o layerId
        label = ~paste0("Muni: ", name_muni)
      ) %>% addLegend(
        position = "bottomleft",
        pal = pal,
        values = colorData,
        title = indicator,
      )
}

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      # addTiles() %>%
      setView(lat = -14, lng = -51, zoom = 4)
  })

  reactiveDataStatesByYear <- reactive({
    dataStates %>% filter(year == input$year)
  })

  reactiveDataByYear <- reactive({
    data %>% filter(year == input$year)
  })

  reactiveDataByMuni <- reactive({
    data %>% filter(name_muni == input$muni)
  })

  reactiveIsShowingStates <- reactiveVal(TRUE)

  observe({
    if (isolate(reactiveIsShowingStates())) {
      indicator <- input$indicator
    } else {
      indicator <- isolate(input$indicator)
    }

    data <- reactiveDataStatesByYear()

    isolate({
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
        fillColor = pal(colorData),
        layerId = ~abbrev_state
      ) %>% addLegend(
        position = "bottomleft",
        pal = pal,
        values = colorData,
        title = indicator,
      )
    })
  })

  # output$muniStats <- renderPlot({
  #   data <- reactiveDataByMuni() %>% select(!geom)
  #   ggplot(data = data, aes_string(x = "year", y = input$indicator)) +
  #     geom_line() +
  #     theme(
  #       axis.title.x = element_text(size = 12),
  #       axis.title.y = element_text(size = 12),
  #       axis.text.x = element_text(size = 11),
  #       axis.text.y = element_text(size = 11),
  #     )
  # })

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

    if (is.null(event)) return()

    reactiveIsShowingStates(FALSE)

    indicator <- input$indicator

    isolate({
      state <- reactiveDataByYear() %>% filter(abbrev_state == event$id)

      bounds <-  sf::st_bbox(state)

      if (indicator == "Breastfeeding") {
        colorData <- state$breastfeeding_complete
        pal <- colorBin("viridis", colorData)
      } else {
        colorData <- state$total_vaccinated
        pal <- colorNumeric("viridis", colorData)
      }

      showStateMunicipalities(state, indicator, colorData, pal, bounds)
    })
  })

  observeEvent(input$reset_map, {
    indicator <- input$indicator

    reactiveIsShowingStates(TRUE)

    data <- reactiveDataStatesByYear()

    if (indicator == "Breastfeeding") {
      colorData <- data$breastfeeding_complete
      pal <- colorBin("viridis", colorData)
    } else {
      colorData <- data$total_vaccinated
      pal <- colorNumeric("viridis", colorData)
    }

    leafletProxy("map", data = data) %>%
      clearShapes() %>% clearControls() %>%
      setView(lat = -14, lng = -51, zoom = 4) %>%
      addPolygons(
        weight = 2,
        fillOpacity = 0.7,
        dashArray = "3",
        fillColor = pal(colorData),
        layerId = ~abbrev_state
      ) %>% addLegend(
        position = "bottomleft",
        pal = pal,
        values = colorData,
        title = indicator,
      )
  })

  ## Boxplot -----

  # output$boxplot <- renderPlot({
  #   ggplot(reactiveDataByYear(), aes(x = name_region, y = total_vaccinated)) +
  #     geom_jitter(aes(color = total_population), alpha = 0.5) +
  #     geom_boxplot(alpha = 0.5) +
  #     ggtitle("Population vaccinated") +
  #     xlab(paste("Vaccinated -", input$year)) +
  #     theme(
  #       axis.title.x = element_text(size = 13),
  #       axis.title.y = element_text(size = 13),
  #       axis.text.x = element_text(size = 12),
  #       axis.text.y = element_text(size = 12),
  #       plot.title = element_text(size = 14, face = "bold")
  #     )
  # })

  ## Table -----

  # output$table <- DT::renderDT({
  #   sample_data <- head(data, 100)

  #   DT::datatable(
  #     data = sample_data,
  #     options = list(pageLength = 10),
  #     rownames = FALSE
  #   )
  # })
}

shinyApp(ui, server)
