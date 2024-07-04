library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Dropdown and Calculations in DataTable"),
  DTOutput("contributionsTable")
)

# Define server logic
server <- function(input, output, session) {
  
  # Data preparation
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  employee_contribution <- rep(24000, 12)
  employer_contribution <- rep(24000, 12)
  contributions <- data.frame(
    Month = months,
    Employee_Contribution = employee_contribution,
    Employer_Contribution = employer_contribution,
    Monthly_Contribution = employee_contribution + employer_contribution,
    Event_1_Description = rep("", 12),
    Event_2_Description = rep("", 12),
    Event_3_Description = rep("", 12),
    Event_Amount = rep(0, 12),  # Initialize with zeros
    stringsAsFactors = FALSE
  )
  
  # Function to calculate event amount based on conditions
  calculateEventAmount <- function(event1, event2, event3) {
    amount1 <- ifelse(event1 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event1 == "Exit Package", 5000, 0))
    amount2 <- ifelse(event2 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event2 == "Exit Package", 5000, 0))
    amount3 <- ifelse(event3 %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event3 == "Exit Package", 5000, 0))
    return(amount1 + amount2 + amount3)
  }
  
  # Add an editable DataTable
  output$contributionsTable <- renderDT({
    datatable(contributions, selection = 'none', editable = TRUE, options = list(
      columnDefs = list(
        list(targets = 5, defaultContent = "", render = JS(
          "function(data, type, row, meta){ return type === 'display' ? '<select>' +
            '<option value=\"\">Select...</option>' +
            '<option value=\"Birth\">Birth</option>' +
            '<option value=\"Wedding\">Wedding</option>' +
            '<option value=\"Loss of a Loved One\">Loss of a Loved One</option>' +
            '<option value=\"No Incident\">No Incident</option>' +
            '<option value=\"Exit Package\">Exit Package</option>' +
            '</select>' : data; }"
        )),
        list(targets = 6, defaultContent = "", render = JS(
          "function(data, type, row, meta){ return type === 'display' ? '<select>' +
            '<option value=\"\">Select...</option>' +
            '<option value=\"Birth\">Birth</option>' +
            '<option value=\"Wedding\">Wedding</option>' +
            '<option value=\"Loss of a Loved One\">Loss of a Loved One</option>' +
            '<option value=\"No Incident\">No Incident</option>' +
            '<option value=\"Exit Package\">Exit Package</option>' +
            '</select>' : data; }"
        )),
        list(targets = 7, defaultContent = "", render = JS(
          "function(data, type, row, meta){ return type === 'display' ? '<select>' +
            '<option value=\"\">Select...</option>' +
            '<option value=\"Birth\">Birth</option>' +
            '<option value=\"Wedding\">Wedding</option>' +
            '<option value=\"Loss of a Loved One\">Loss of a Loved One</option>' +
            '<option value=\"No Incident\">No Incident</option>' +
            '<option value=\"Exit Package\">Exit Package</option>' +
            '</select>' : data; }"
        ))
      )
    ))
  }, server = FALSE)  # Note server = FALSE for proper client-side rendering
  
  # Observe changes and update the data
  observeEvent(input$contributionsTable_cell_edit, {
    info <- input$contributionsTable_cell_edit
    row <- info$row
    col <- info$col
    contributions[row, col] <<- info$value
    # Update the event amount whenever an event description is edited
    if (col %in% c(5, 6, 7)) {  # Check if the change is in any event description columns
      contributions[row, 8] <<- calculateEventAmount(
        contributions[row, 5], contributions[row, 6], contributions[row, 7]
      )
    }
    replaceData(output$contributionsTable, contributions, resetPaging = FALSE)  # Update data table without resetting paging
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
