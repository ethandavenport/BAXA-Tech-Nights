# Halloween Shiny

# Install packages
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

# Load Shiny and other necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load data
# Assuming the dataset is stored locally as "candy_data.csv" after download from Kaggle
candy_data <- read_csv("candy-data.csv")



########## Define UI ##########


ui <- fluidPage(
  # Custom CSS for Halloween theme
  tags$style(HTML("
    body { background-color: #1E1E1E; color: #FF7518; }
    h2, h3 { color: #FF6347; }
    .btn-custom { background-color: #FF7518; color: #1E1E1E; border-radius: 10px; }
    .plot-container { background-color: #333333; padding: 10px; border-radius: 8px; }
  ")),
  
  titlePanel("🎃 Halloween Candy Power Ranking Explorer 🎃"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Candy Filters"),
      # Candy Selector
      selectInput("candy", 
                  "Select a Candy:", 
                  choices = unique(candy_data$competitorname)),
      
      # Checkbox Group for Candy Characteristics
      checkboxGroupInput("attributes", 
                         "Candy Attributes:",
                         choices = list("Chocolate" = "chocolate",
                                        "Fruity" = "fruity",
                                        "Caramel" = "caramel",
                                        "Peanuty/Almondy" = "peanutyalmondy",
                                        "Hard" = "hard",
                                        "Bar" = "bar")),
      
      # Slider for Popularity Score
      sliderInput("popularity", 
                  "Minimum Popularity Score:", 
                  min = min(candy_data$winpercent), 
                  max = max(candy_data$winpercent),
                  value = min(candy_data$winpercent), 
                  step = 5),
      
      # Radio Buttons to Toggle Plot Type
      radioButtons("plot_type", 
                   "Plot Type:", 
                   choices = list("Ranking Scatter Plot" = "scatter", 
                                  "Popularity Bar Chart" = "bar"))
    ),
    
    mainPanel(
      h3("Candy Details"),
      textOutput("candy_name"),
      textOutput("chocolate"),
      textOutput("fruity"),
      textOutput("caramel"),
      textOutput("peanutyalmondy"),
      textOutput("hard"),
      textOutput("bar"),
      textOutput("sugar_percent"),
      textOutput("win_percent"),
      div(class = "plot-container", plotOutput("candy_plot"))
    )
  )
)


########## Define server logic ##########


server <- function(input, output, session) {
  # Display candy details based on selected candy
  # In the server function
  output$candy_name <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Candy:", selected_candy$competitorname[1])
  })
  
  output$chocolate <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Chocolate:", ifelse(selected_candy$chocolate[1] == 1, "Yes", "No"))
  })
  
  output$fruity <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Fruity:", ifelse(selected_candy$fruity[1] == 1, "Yes", "No"))
  })
  
  output$caramel <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Caramel:", ifelse(selected_candy$caramel[1] == 1, "Yes", "No"))
  })
  
  output$peanutyalmondy <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Peanuty/Almondy:", ifelse(selected_candy$peanutyalmondy[1] == 1, "Yes", "No"))
  })
  
  output$hard <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Hard:", ifelse(selected_candy$hard[1] == 1, "Yes", "No"))
  })
  
  output$bar <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Bar:", ifelse(selected_candy$bar[1] == 1, "Yes", "No"))
  })
  
  output$sugar_percent <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Sugar Percent:", selected_candy$sugarpercent[1])
  })
  
  output$win_percent <- renderText({
    selected_candy <- candy_data %>% filter(competitorname == input$candy)
    paste("Win Percent:", selected_candy$winpercent[1])
  })
  

  # Filter data based on selected attributes and popularity score
  filtered_data <- reactive({
    data <- candy_data %>%
      filter(winpercent >= input$popularity)
    
    # Apply attribute filters if selected
    if (!is.null(input$attributes)) {
      for (attribute in input$attributes) {
        data <- data %>% filter(!!sym(attribute) == 1)
      }
    }
    
    data
  })
  
  # Generate plots based on user selection
  output$candy_plot <- renderPlot({
    plot_data <- filtered_data()
    
    if (input$plot_type == "scatter") {
      ggplot(plot_data, aes(x = sugarpercent, y = pricepercent, color = competitorname)) +
        geom_point(size = 3) +
        labs(title = "Candy Ranking Scatter Plot", x = "Sugar Percent", y = "Price Percent") +
        theme_minimal(base_family = "Comic Sans MS") +
        theme(
          plot.background = element_rect(fill = "#333333"),
          plot.title = element_text(color = "#FF6347"),
          axis.text = element_text(color = "#FF7518"),
          axis.title = element_text(color = "#FF7518"),
          legend.position = "none"
        )
    } else {
      ggplot(plot_data, aes(x = reorder(competitorname, winpercent), y = winpercent, fill = competitorname)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Popularity Bar Chart", x = "Candy", y = "Win Percent") +
        theme_minimal(base_family = "Comic Sans MS") +
        theme(
          plot.background = element_rect(fill = "#333333"),
          plot.title = element_text(color = "#FF6347"),
          axis.text = element_text(color = "#FF7518"),
          axis.title = element_text(color = "#FF7518"),
          legend.position = "none"
        )
    }
  })
}


########## Run the application ##########
shinyApp(ui = ui, server = server)

