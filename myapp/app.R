#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load the Shiny library
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("num_participants", "Number of Participants (2-10):", value = 2, min = 2, max = 10),
      actionButton("add_participants", "Add Participant Names"),
      uiOutput("participant_names"),
      conditionalPanel(
        condition = "input.add_participants > 0",
        numericInput("num_items", "Number of Items to Distribute (1-100):", value = 1, min = 1, max = 100),
        actionButton("add_items", "Add Items"),
        uiOutput("item_names"),
        conditionalPanel(
          condition = "input.add_items > 0",
          textOutput("distribution_result"),
          actionButton("start_distribution", "Start Distribution")
        )
      )
    ),
    mainPanel(
      uiOutput("item_distribution")
    )
  )
)

server <- function(input, output, session) {
  participants <- reactiveVal()
  items <- reactiveVal()
  item_names <- reactiveVal()
  current_item <- reactiveVal(1)
  current_participant <- reactiveVal(1)
  distribution <- reactiveVal(matrix(0, ncol = 2))

  observeEvent(input$add_participants, {
    participants(replicate(input$num_participants, ""))
  })

  observeEvent(input$add_items, {
    items(1:input$num_items)
    item_names(replicate(input$num_items, ""))
    distribution(matrix(0, ncol = input$num_participants))
    current_item(1)
    current_participant(1)
  })

  output$participant_names <- renderUI({
    if (input$add_participants > 0) {
      lapply(1:input$num_participants, function(i) {
        textInput(paste0("participant_", i), label = paste("Participant", i, "Name:"), value = participants()[i])
      })
    }
  })

  output$item_names <- renderUI({
    if (input$add_items > 0) {
      lapply(1:input$num_items, function(i) {
        textInput(paste0("item_name_", i), label = paste("Item", i, "Name:"), value = item_names()[i])
      })
    }
  })

  output$item_distribution <- renderUI({
    if (input$start_distribution > 0) {
      tagList(
        numericInput(
          paste0("item_value_", current_item(), "_participant_", current_participant()),
          label = paste("Value for ", item_names()[current_item()], " by Participant ", participants()[current_participant()], ":"),
          value = 0, min = 0, max = 100
        ),
        actionButton("next_item", "Next Item")
      )
    }
  })

  observeEvent(input$next_item, {
    distribution()[current_participant(), current_item()] <- input[[paste0("item_value_", current_item(), "_participant_", current_participant())]]

    if (current_item() < input$num_items) {
      current_item(current_item() + 1)
    } else {
      if (current_participant() < input$num_participants) {
        current_participant(current_participant() + 1)
        current_item(1)
      } else {
        result <- rowSums(distribution()) / input$num_participants
        output$distribution_result <- renderText({
          paste("Result for item(s) distribution:", paste(result, collapse = ", "))
        })
      }
    }
  })
}

shinyApp(ui, server)

