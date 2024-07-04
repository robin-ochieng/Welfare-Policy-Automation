library(shiny)
library(DT)
library(bs4Dash)
library(bslib)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish")
)

# Define the UI using bs4Dash components with custom theme
ui <- bs4DashPage(
  freshTheme = my_theme,
  title = "Welfare Policy",
  header = bs4DashNavbar(
    status = "white",
    skin = "dark",
    title = "Kenbright Welfare Policy"
  ),
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        text = "Main",
        tabName = "main",
        icon = icon("dashboard")
      )
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "main",
        fluidRow(
          column(
            width = 8,
            box(
              title = "Inputs",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              selectInput("monthSelect", "Select Month", choices = month.name),
              uiOutput("dynamicEvents"),
              actionButton("updateBtn", "Update Events", class = "btn btn-primary",
                           style = "background-color: #007bff; color: white;")
            )
          ),
          column(
            width = 12,
            box(
              title = "Contributions Table",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12, # Full width of the column
              height = "100%", # Full height
              DTOutput("contributionsTable")
            )
          )
        )
      )
    )
  ),
  footer = bs4DashFooter(
    div(style = "text-align: center; padding: 10px;", 
        "© 2024 Welfare Policy | Powered by bs4Dash")
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Data preparation
  contributions <- reactiveVal(data.frame(
    Month = c(month.name, "Total"),
    Employee_Contribution = c(rep(24000, 12), 0),
    Employer_Contribution = c(rep(24000, 12), 0),
    Monthly_Contribution = c(rep(48000, 12), 0),
    Event_Description = c(rep("", 12), ""),
    Event_Amount = c(rep(0, 12), 0),
    stringsAsFactors = FALSE
  ))
  
  # Dynamic UI for events
  output$dynamicEvents <- renderUI({
    tagList(
      lapply(1:3, function(i) {
        selectInput(paste0("event", i), 
                    paste("Event", i, "Description"), 
                    choices = c("Select...", "Birth", "Wedding", "Loss of a Loved One", "No Incident", "Exit Package"))
      })
    )
  })
  
  # Calculate event amount
  calculateEventAmount <- function(events) {
    sapply(events, function(event) {
      ifelse(event %in% c("Birth", "Wedding", "Loss of a Loved One"), 10000, ifelse(event == "Exit Package", 5000, 0))
    }) %>% sum()
  }
  
  # Render DataTable
  output$contributionsTable <- renderDT({
    data <- contributions()
    # Calculate totals
    for (i in 2:6) {
      if (is.numeric(data[[i]])) {
        data[nrow(data), i] <- sum(data[1:(nrow(data)-1), i])
      }
    }
    datatable(data, selection = 'none', editable = TRUE, 
              options = list(
                autoWidth = TRUE,
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                pageLength = 15,
                scrollX = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all')),
                dom = 't'
              ),
              class = 'display nowrap compact',
              rownames = FALSE,
              callback = JS("table.rows().every(function(rowIdx, tableLoop, rowLoop){
                                var data = this.data();
                                if(data[0] === 'Total'){
                                  $(this.node()).css({'background-color': '#f2f2f2', 'font-weight': 'bold', 'color': '#000000'});
                                }
                             });")
    )
  }, server = FALSE)
  
  # Observe button click to update the data
  observeEvent(input$updateBtn, {
    data <- contributions()
    month <- match(input$monthSelect, month.name)
    events <- sapply(1:3, function(i) input[[paste0("event", i)]])
    event_descriptions <- paste(events[events != "Select..."], collapse = ";  ")
    event_amount <- calculateEventAmount(events)
    
    data[month, c(5, 6)] <- c(event_descriptions, event_amount)
    contributions(data)  # Update the reactive value
  })
}

# Run the application
shinyApp(ui = ui, server = server)