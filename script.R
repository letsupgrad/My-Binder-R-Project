library(shiny)
library(shinydashboard)
library(DT)
library(arules)
library(rsconnect)
# Sample data frame
df <- data.frame(
  inventoryId = c('Billboard1', 'Billboard2', 'Billboard3', 'Billboard1', 'Billboard2'),
  audienceType = c('Male', 'Female', 'Male', 'Female', 'Male'),
  timeOfDay = c('Morning', 'Afternoon', 'Morning', 'Evening', 'Afternoon'),
  impressions = c(100, 250, 150, 300, 400),
  mediaCost = c(10, 20, 15, 30, 25)
)

# Convert the data frame to a transactional format
df_trans <- as(df, "transactions")

# Run the apriori algorithm to find association rules
rules <- apriori(df_trans, parameter = list(support = 0.1, confidence = 0.5))

# View the first few rules
inspect(head(rules))

# Define UI for dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Association Rule Mining for Impressions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Association Rules", tabName = "rules", icon = icon("table")),
      menuItem("Adjust Parameters", tabName = "params", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab: Show association rules
      tabItem(tabName = "rules",
              h2("Association Rules"),
              DTOutput("rules_table")
      ),
      # Second tab: Adjust support and confidence parameters
      tabItem(tabName = "params",
              h2("Adjust Parameters"),
              sliderInput("support", "Support", min = 0.01, max = 0.5, value = 0.1),
              sliderInput("confidence", "Confidence", min = 0.01, max = 1, value = 0.5),
              actionButton("apply", "Apply Changes")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive function to apply apriori with selected support and confidence
  apply_rules <- reactive({
    # Get the user-defined support and confidence values
    support_value <- input$support
    confidence_value <- input$confidence
    
    # Run the Apriori algorithm with updated parameters
    rules <- apriori(df_trans, parameter = list(support = support_value, confidence = confidence_value))
    
    # Return the rules
    return(rules)
  })
  
  # Render the rules table
  output$rules_table <- renderDT({
    # Get the rules from the reactive function
    rules <- apply_rules()
    
    # Convert rules to a data frame for better display in the table
    rules_df <- as(rules, "data.frame")
    
    # Display the rules in an interactive table
    datatable(rules_df, options = list(pageLength = 10))
  })
  
  # Observe the Apply button to re-run the rules when parameters change
  observeEvent(input$apply, {
    output$rules_table <- renderDT({
      rules <- apply_rules()
      rules_df <- as(rules, "data.frame")
      datatable(rules_df, options = list(pageLength = 10))
    })
  })
}

# Run the shiny app
shinyApp(ui, server)
