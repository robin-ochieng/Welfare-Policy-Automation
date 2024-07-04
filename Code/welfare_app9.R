library(shiny)
library(DT)
library(bs4Dash)
library(bslib)
library(scales)
library(magrittr)  # Ensure the pipe operator is available


# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# Define the UI using bs4Dash components with custom theme
ui <- bs4DashPage(
  useShinyjs(),
  freshTheme = my_theme,
  title = "Welfare Policy",
  header = bs4DashNavbar(
    status = "primary",
    skin = "dark",
    title = dashboardBrand(
      title = "Welfare Policy",
      color = "primary",
      href = "https://adminlte.io/themes/v3",
      image = "data:image/png;base64,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"
    ),
    actionButton("toggleControlbar", "Input Controls", class = "btn btn-primary")
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    bs4Card(
      title = "Navigation Menu",
      style = "max-width: 100%;",
      background = "info",
      width = 12,
      solidHeader = TRUE,  
      collapsible = TRUE,  
      collapsed = FALSE,
      status = "primary", 
      bs4SidebarMenu(
        bs4SidebarMenuItem(
          text = "Contribution Table",
          tabName = "main",
          icon = icon("dashboard")
        )
      )
    ),
    # Custom footer within the sidebar
    div(
      style = "background-color: #007bff; color: white; padding: 5px; text-align: center; position: absolute; bottom: 0; width: 100%; font-size: 12px",
      actionLink(inputId = "footerLink", label = "Welfare Policy", style = "color: white; text-decoration: none;")
    )
  ),
  controlbar = bs4DashControlbar(
    id = "controlbar",
    skin = "light",
    bs4Card(
      title = "Input Controls",
      background = "info",
      width = 12,
      height = 10,
      style = "max-width: 100%;",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      numericInput("numEmployees", "Number of Employees", value = 80, min = 1, max = 130),
      numericInput("contributionAmount", "Contribution Amount", value = 300, min = 100, max = 2000, step = 50),
      selectInput("monthSelect", "Select Month", choices = month.name),
      uiOutput("dynamicEvents"),
      actionButton("updateBtn", "Update Events", class = "btn btn-primary",
                   style = "background-color: #007bff; color: white;"),
      br(), 
      br(),
      actionButton("updateAllBtn", "Apply to All Months", class = "btn btn-primary",
                   style = "background-color: #007bff; color: white;")
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "main",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Contributions Table",
              closable = TRUE,
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
    div(style = "background-color: #007bff; color: white; text-align: center; padding: 8px;", 
        "© 2024 Welfare Policy | Powered by bs4Dash (Tech and Research Department - Kenbright)")
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$footerLink, {
    showModal(modalDialog(
      title = "Message",
      "Welfare Policy",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("controlbar")
  })
  # Function to calculate event amount
  calculateEventAmount <- function(events) {
    sum(sapply(events, function(event) {
      if (event == "Birth" || event == "Wedding" || event == "Loss of a Loved One") {
        return(10000)
      } else if (event == "Exit Package") {
        return(5000)
      } else {
        return(0)
      }
    }))
  }
  
  # Data preparation
  contributions <- reactiveVal(data.frame(
    Month = c(month.name, "Total"),
    Employee_Contribution = rep(0, 13),
    Employer_Contribution = rep(0, 13),
    Monthly_Contribution = rep(0, 13),
    Event_Description = c(rep("", 12), ""),
    Event_Amount = c(rep(0, 12), 0),
    Amount_Invested = rep(0, 13),  
    Investment_Return = rep(0, 13),  
    Any_Balance_Brought_Forward = rep(0, 13),
    Balance_After_Event_and_Investment = rep(0, 13),  # New column
    Balance_Carried_Forward = rep(0, 13),  # New column 
    Total_Amount = rep(0, 13),
    stringsAsFactors = FALSE
  ))
  
  # Update contribution calculations
  observe({
    data <- contributions()
    employee_contrib <- input$numEmployees * input$contributionAmount
    employer_contrib <- input$numEmployees * input$contributionAmount
    monthly_contrib <- employee_contrib + employer_contrib
    total_amount <- monthly_contrib + data$Any_Balance_Brought_Forward[1:12] 
    balance_after_event_and_investment <- total_amount - (data$Event_Amount[1:12] + data$Amount_Invested[1:12])
    balance_carried_forward <- balance_after_event_and_investment - data$Investment_Return[1:12]
    
    data[1:12, "Employee_Contribution"] <- employee_contrib
    data[1:12, "Employer_Contribution"] <- employer_contrib
    data[1:12, "Monthly_Contribution"] <- monthly_contrib
    data[1:12, "Total_Amount"] <- total_amount
    data[1:12, "Balance_After_Event_and_Investment"] <- balance_after_event_and_investment
    data[1:12, "Balance_Carried_Forward"] <- balance_carried_forward
    # Calculate and update the Total row separately
    data[13, c("Employee_Contribution", "Employer_Contribution", "Monthly_Contribution",
               "Event_Amount", "Amount_Invested", "Investment_Return", "Any_Balance_Brought_Forward",
               "Total_Amount", "Balance_After_Event_and_Investment", "Balance_Carried_Forward")] <-
      sapply(data[1:12, c("Employee_Contribution", "Employer_Contribution", "Monthly_Contribution",
                          "Event_Amount", "Amount_Invested", "Investment_Return", "Any_Balance_Brought_Forward",
                          "Total_Amount", "Balance_After_Event_and_Investment", "Balance_Carried_Forward")], sum)
    contributions(data)
  })
  
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
  
  # Render DataTable with JavaScript formatting
  output$contributionsTable <- renderDT({
    data <- contributions()
    datatable(data, options = list(
      autoWidth = TRUE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE,
      pageLength = 15,
      scrollX = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = '_all'),
        list(
          targets = c(2, 3, 4, 6, 7, 8, 9, 10, 11, 12),  # Specify the numeric columns you want to format
          render = JS(
            "function(data, type, row) {",
            "  if(type === 'display' && data !== null && data !== '') {",
            "    var num = parseFloat(data);",
            "    if(!isNaN(num)) {",
            "      return num.toLocaleString();", 
            "    }",
            "  }",
            "  return data;",
            "}"
          )
        )
      ),
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({",
        "    'background-color': '#007BFF',", 
        "    'color': '#FFFFFF'",  
        "  });",
        "}"
      ),
      createdRow = JS(
        "function(row, data, dataIndex) {",
        "  if (dataIndex === 12) {",  
        "    $(row).css({'background-color': '#add8e6', 'font-weight': 'bold', 'color': 'blue'});",
        "  }",
        "}"
      ),
      dom = 't'
    ), class = 'display nowrap compact')
  }, server = FALSE)
  
  
  
  # Observe button click to update the data
  observeEvent(input$updateBtn, {
    data <- contributions()
    month <- match(input$monthSelect, month.name)
    events <- sapply(1:3, function(i) input[[paste0("event", i)]])
    event_descriptions <- paste(events[events != "Select..."], collapse = ";  ")
    event_amount <- calculateEventAmount(events)
    
    data[month, "Event_Description"] <- event_descriptions
    data[month, "Event_Amount"] <- event_amount
    contributions(data)  # Update the reactive value
  })
  
  # Observe button click to apply events to all months
  observeEvent(input$updateAllBtn, {
    data <- contributions()
    events <- sapply(1:3, function(i) input[[paste0("event", i)]])
    event_descriptions <- paste(events[events != "Select..."], collapse = ";  ")
    event_amount <- calculateEventAmount(events)
    
    data[1:12, "Event_Description"] <- rep(event_descriptions, 12)
    data[1:12, "Event_Amount"] <- rep(event_amount, 12)
    contributions(data)  # Update the reactive value for all months
  })
}


# Run the application
shinyApp(ui = ui, server = server)