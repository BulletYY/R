library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tseries)
library(shinythemes)
library(stats)
library(scales)



dane_makroekonomiczne$DATE <- as.Date(dane_makroekonomiczne$DATE)

dane_makroekonomiczne_shiny <- dane_makroekonomiczne %>% 
  select(CPI_US, monthly_return_NQ, DATE, Year) %>%
  mutate(
    monthly_return_NQ = round(monthly_return_NQ, 2),
    CPI_US = round(CPI_US, 2)
  )

ui <- fluidPage(
  theme = shinytheme("cerulean"),  
  titlePanel("Analiza inflacji CPI i zwrotów z Nasdaqu"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtry danych"),  
      sliderInput("cpi_filter", "Inflacji CPI:", 
                  min = min(dane_makroekonomiczne_shiny$CPI_US), 
                  max = max(dane_makroekonomiczne_shiny$CPI_US), 
                  value = c(min(dane_makroekonomiczne_shiny$CPI_US), 
                            max(dane_makroekonomiczne_shiny$CPI_US))),
      sliderInput("return_filter", "Zwrotów z NQ:", 
                  min = min(dane_makroekonomiczne_shiny$monthly_return_NQ), 
                  max = max(dane_makroekonomiczne_shiny$monthly_return_NQ), 
                  value = c(min(dane_makroekonomiczne_shiny$monthly_return_NQ), 
                            max(dane_makroekonomiczne_shiny$monthly_return_NQ))),
      textInput("year_range", "Zakres lat (np. 2000-2024)", value = "2000-2024"),
      selectInput("year_filter", "Wybierz pojedynczy rok:", 
                  choices = unique(dane_makroekonomiczne_shiny$Year), 
                  selected = unique(dane_makroekonomiczne_shiny$Year)[1],
                  multiple = TRUE),
      downloadButton("downloadData", "Pobierz wybrane  dane", class = "btn btn-primary btn-lg"),
      actionButton("resetFilters", "Resetuj filtry", class = "btn btn-secondary btn-lg")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela danych", 
                 DTOutput("table")),
        tabPanel("Wykresy",
                 selectInput("plot_type", "Typ wykresu:", 
                             choices = c("Wykres liniowy" = "line", 
                                         "Wykres punktowy" = "scatter", 
                                         "Boxplot" = "boxplot", 
                                         "Regresja liniowa" = "regression")),  
                 
                 selectInput("x_axis", "Os X", 
                             choices = names(dane_makroekonomiczne_shiny)[!names(dane_makroekonomiczne_shiny) %in% "Year"], 
                             selected = "CPI_US"),
                 selectInput("y_axis", "Os Y", 
                             choices = names(dane_makroekonomiczne_shiny)[!names(dane_makroekonomiczne_shiny) %in% "Year"], 
                             selected = "monthly_return_NQ"),
                 
                 checkboxInput("facet_wrap", "Podział na lata", value = FALSE),
                 
                 plotOutput("plot", height = "400px")),
        tabPanel("Rozkłady", 
                 selectInput("distribution_variable", "Zmienna", 
                             choices = c("CPI_US", "monthly_return_NQ"), 
                             selected = "CPI_US"),
                 plotOutput("distribution_plot", height = "400px")),
        tabPanel("Analiza Statystyczna",
                 h4("Statystyki opisowe"),
                 tableOutput("descriptive_stats"),
                 h4("Korelacja między CPI_US a Monthly Return"),
                 verbatimTextOutput("correlation_result"),
                 h4("Test Shapiro-Wilka"),
                 verbatimTextOutput("shapiro_test"),
                 h4("Test Jarque-Bera"),
                 verbatimTextOutput("jarque_bera_test")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    year_range <- strsplit(input$year_range, "-")[[1]]
    if (length(year_range) == 2) {
      start_year <- as.integer(year_range[1])
      end_year <- as.integer(year_range[2])
    } else {
      start_year <- min(dane_makroekonomiczne_shiny$Year)
      end_year <- max(dane_makroekonomiczne_shiny$Year)
    }
    
    dane <- dane_makroekonomiczne_shiny %>%
      filter(CPI_US >= input$cpi_filter[1], 
             CPI_US <= input$cpi_filter[2],
             monthly_return_NQ >= input$return_filter[1], 
             monthly_return_NQ <= input$return_filter[2],
             Year >= start_year & Year <= end_year)
    
    if (length(input$year_filter) > 0) {
      dane <- dane %>%
        filter(Year %in% input$year_filter)
    }
    
    return(dane)
  })
  
  output$table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 25)) %>% 
      formatStyle(columns = names(filtered_data()),  color = "black")
  })
  
  output$plot <- renderPlot({
    data <- filtered_data()
    x <- input$x_axis
    y <- input$y_axis
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    plot <- ggplot(data, aes(x = !!sym(x), y = !!sym(y))) 
    
    if (input$plot_type == "line") {
      plot <- plot + geom_line(color = "green", linewidth = 1.5)
    } else if (input$plot_type == "scatter") {
      plot <- plot + geom_point(color = "blue", size = 3, alpha = 0.7)
    } else if (input$plot_type == "boxplot") {
      plot <- plot + geom_boxplot(fill = "purple", alpha = 0.7)
    } else if (input$plot_type == "regression") {
      plot <- plot + geom_point(color = "blue", size = 3) +
        geom_smooth(method = "lm", se = TRUE, color = "red", aes(group = 1))
    }
    
    if (input$facet_wrap && !is.null(data$Year) && length(unique(data$Year)) > 1) {
      plot <- plot + facet_wrap(~ Year)
    }
    
    plot <- plot +
      labs(x,y) +
      scale_y_continuous(name = y, breaks = pretty_breaks(n = 10)) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "navy"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text( hjust = 1, vjust = 1), 
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_line(color = "gray")
      )
    
    return(plot)
  })
  
  
  
  
  output$descriptive_stats <- renderTable({
    data <- filtered_data()
    stats <- data %>%
      summarize(
        Avg_CPI = mean(CPI_US),
        Median_CPI = median(CPI_US),
        StdDev_CPI = sd(CPI_US),
        Avg_Return = mean(monthly_return_NQ),
        Median_Return = median(monthly_return_NQ),
        StdDev_Return = sd(monthly_return_NQ,)
      )
    stats
  })
  
  output$correlation_result <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 1) {
      cor_result <- cor(data$CPI_US, data$monthly_return_NQ)
      paste("Korelacja :", round(cor_result, 2))
    } 
  })
  
  output$shapiro_test <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 2) {
      shapiro_cpi <- shapiro.test(data$CPI_US)
      shapiro_return <- shapiro.test(data$monthly_return_NQ)
      paste("Shapiro-Wilk CPI_US: ", round(shapiro_cpi$statistic, 3), ", p-value:", round(shapiro_cpi$p.value, 3),"-------",
        "Shapiro-Wilk NQ zwroty =", round(shapiro_return$statistic, 3), ", p-value:", round(shapiro_return$p.value, 3)
      )
    } 
  })
  
  output$jarque_bera_test <- renderPrint({
    data <- filtered_data()
    if (nrow(data) > 2) {
      jb_cpi <- jarque.bera.test(data$CPI_US)
      jb_return <- jarque.bera.test(data$monthly_return_NQ)
      paste(
        "Jarque-Bera CPI_US: Chi-sq =", round(jb_cpi$statistic, 3), ", p-value:", round(jb_cpi$p.value, 3),"-----", 
        "Jarque-Bera Monthly Return: Chi-sq =", round(jb_return$statistic, 3),  ", p-value:", round(jb_return$p.value, 3)
      )
    } 
  })
  
  output$distribution_plot <- renderPlot({
    data <- filtered_data()
    variable <- input$distribution_variable
    
    n_bins <- round(sqrt(nrow(data)))
    
    ggplot(data, aes_string(x = variable)) + 
      geom_histogram(bins = n_bins, fill = "darkblue", color = "white", alpha = 0.8) +
      labs(
        title = paste("Rozkład", variable),
        x = variable, 
        y = "Czestosc"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 12)
  
      ) 
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$resetFilters, {
    updateSliderInput(session, "cpi_filter", 
                      value = c(min(dane_makroekonomiczne_shiny$CPI_US), 
                                max(dane_makroekonomiczne_shiny$CPI_US)))
    updateSliderInput(session, "return_filter", 
                      value = c(min(dane_makroekonomiczne_shiny$monthly_return_NQ), 
                                max(dane_makroekonomiczne_shiny$monthly_return_NQ)))
    updateTextInput(session, "year_range", 
                    value = "2000-2024")
    updateSelectInput(session, "year_filter", 
                      selected = unique(dane_makroekonomiczne_shiny$Year)[1])
  })
}

shinyApp(ui = ui, server = server)
