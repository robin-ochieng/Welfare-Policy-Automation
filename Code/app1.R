library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Dynamic Dropdown in DataTable"),
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
    stringsAsFactors = FALSE
  )
  
  # Add an editable DataTable
  output$contributionsTable <- renderDT({
    datatable(contributions, selection = 'none', editable = TRUE, options = list(
      columnDefs = list(
        list(targets = 4, defaultContent = "", render = JS(
          "function(data, type, row, meta){
            return type === 'display' ? '<select>' + 
              '<option value=\"Birth\">Birth</option>' +
              '<option value=\"Wedding\">Wedding</option>' +
              '<option value=\"Loss of a Loved One\">Loss of a Loved One</option>' +
              '<option value=\"No Incident\">No Incident</option>' +
              '<option value=\"Exit Package\">Exit Package</option>' +
              '</select>' : data;
          }"))
      )
    ))
  }, server = FALSE)  # Note server = FALSE for proper client-side rendering
  
  # Observe changes and update the data
  observeEvent(input$contributionsTable_cell_edit, {
    info <- input$contributionsTable_cell_edit
    str(info)
    contributions[info$row, info$col] <<- info$value
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
