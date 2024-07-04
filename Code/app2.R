library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Welfare Policy"),
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
    stringsAsFactors = FALSE
  )
  
  # Define dropdown HTML correctly using paste0
  dropdownHTML <- paste0('<select>',
                         '<option value="Birth">Birth</option>',
                         '<option value="Wedding">Wedding</option>',
                         '<option value="Loss of a Loved One">Loss of a Loved One</option>',
                         '<option value="No Incident">No Incident</option>',
                         '<option value="Exit Package">Exit Package</option>',
                         '</select>')
  
  # Add an editable DataTable
  output$contributionsTable <- renderDT({
    datatable(contributions, selection = 'none', editable = TRUE, options = list(
      columnDefs = list(
        list(targets = 5, defaultContent = "", render = JS(
          sprintf("function(data, type, row, meta){ return type === 'display' ? '%s' : data; }", dropdownHTML)
        )),
        list(targets = 6, defaultContent = "", render = JS(
          sprintf("function(data, type, row, meta){ return type === 'display' ? '%s' : data; }", dropdownHTML)
        )),
        list(targets = 7, defaultContent = "", render = JS(
          sprintf("function(data, type, row, meta){ return type === 'display' ? '%s' : data; }", dropdownHTML)
        ))
      )
    ))
  }, server = FALSE)  # Note server = FALSE for proper client-side rendering
  
  # Observe changes and update the data
  observeEvent(input$contributionsTable_cell_edit, {
    info <- input$contributionsTable_cell_edit
    contributions[info$row, info$col] <<- info$value
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
