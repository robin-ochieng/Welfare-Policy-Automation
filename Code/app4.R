library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Dropdown and Calculations in DataTable"),
  sidebarLayout(
    sidebarPanel(
      selectInput("monthSelect", "Select Month", choices = month.name),
      selectInput("event1", "Event 1 Description", choices = c("Select...", "Birth", "Wedding", "Loss of a Loved One", "No Incident", "Exit Package")),
      selectInput("event2", "Event 2 Description", choices = c("Select...", "Birth", "Wedding", "Loss of a Loved One", "No Incident", "Exit Package")),
      selectInput("event3", "Event 3 Description", choices = c("Select...", "Birth", "Wedding", "Loss of a Loved One", "No Incident", "Exit Package")),
      actionButton("updateBtn", "Update Events")
    ),
    mainPanel(
      DTOutput("contributionsTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Data preparation
  contributions <- reactiveVal(data.frame(
    Month = month.name,
    Employee_Contribution = rep(24000, 12),
    Employer_Contribution = rep(24000, 12),
    Monthly_Contribution = rep(48000, 12),
    Event_1_Description = rep("", 12),
    Event_2_Description = rep("", 12),
    Event_3_Description = rep("", 12),
    Event_Amount = rep(0, 12),  # Initialize with zeros
    stringsAsFactors = FALSE
  ))
  
  # Function to calculate event amount based on conditions
  calculateEventAmount <- function(event1, event2, event3) {
    amount1 <- ifelse(event1 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event1 == "Exit Package", 5000, 0))
    amount2 <- ifelse(event2 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event2 == "Exit Package", 5000, 0))
    amount3 <- ifelse(event3 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event3 == "Exit Package", 5000, 0))
    return(amount1 + amount2 + amount3)
  }
  
  # Render the DataTable
  output$contributionsTable <- renderDT({
    datatable(contributions(), selection = 'none', editable = TRUE)
  }, server = FALSE)  # Client-side processing for immediate feedback
  
  # Observe button click to update the data
  observeEvent(input$updateBtn, {
    data <- contributions()
    month <- match(input$monthSelect, month.name)
    data[month, 5:7] <- c(input$event1, input$event2, input$event3)
    data[month, 8] <- calculateEventAmount(input$event1, input$event2, input$event3)
    contributions(data)  # Update the reactive value
  })
}

# Run the application
shinyApp(ui = ui, server = server)
