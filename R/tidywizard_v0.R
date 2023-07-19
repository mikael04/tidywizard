library(shiny)
library(shinydashboard)
filter <- get("filter", asNamespace("dplyr"))
select <- get("select", asNamespace("dplyr"))

"%>%" <- magrittr::"%>%"
source("R/logical_condition.R")

df <- ggplot2::mpg

choices_fct <- c("select - select columns",
                 "filter - filter rows",
                 "mutate - create/change columns",
                 "arrange - order rows",
                 "count - count values of columns")

ui <- shinydashboard::dashboardPage(
  ## Header
  shinydashboard::dashboardHeader(
    title = "tidywizard - A dplyr helper"
  ),
  ## Sidebar
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "TABS",

      ## TAB 1
      shinydashboard::menuItem(tabName = "TAB_HOME",
        "Home", icon = icon("house")
      ),
      shinydashboard::menuItem(tabName = "TAB_DATA",
        "Data Manipulation", icon = icon("filter")
      ),
      shinydashboard::menuItem(tabName = "TAB_SHEET",
        "Cheat Sheet", icon = icon("note-sticky")
      )

    )
  ),

  ## Body
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "TAB_DATA",
                              fluidRow(
                                column(
                                  width = 4,
                                  fluidRow(
                                    box(
                                      height = "110",
                                      width = 12,
                                      title = "Choose a function",
                                      div(style = "display:inline-block",
                                          selectInput("SELEC_FCT", label = NULL,
                                                      width = "200px",
                                                      choices = choices_fct)),
                                      div(style = "display:inline-block",
                                          actionButton("CHECK_FCT", label = "Select the function",
                                                       style = "margin-top:-27px",
                                                       icon = icon("check")))
                                    )
                                  ),
                                  fluidRow(
                                    box(
                                      height = "320",
                                      width = 12,
                                      title = "Apply the function",
                                      uiOutput("FUNCTION_INPUTS"),
                                      uiOutput("EXTRA_INPUTS"),
                                      actionButton(inputId = "EXEC_FCT", label = "Execute a função", icon = icon("filter"),
                                                   style = "border-width: 2px; font-size:125%")
                                    )
                                  )
                                ),
                                column(
                                  width = 8,
                                  box(
                                    height = "450",
                                    width = NULL,
                                    title = "Your data",
                                    DT::DTOutput("DATA") %>%
                                      shinycssloaders::withSpinner()
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  box(
                                    height = "300",
                                    width = NULL,
                                    title = "R Code",
                                    verbatimTextOutput("DEBUG")
                                  )
                                )
                              )

      )

    )


  )
)

server <- function(input, output, session){
  data <- reactive(df)

  selected_function <- eventReactive(input$CHECK_FCT, {
    isolate({
      gsub("\\s*-.*", "", input$SELEC_FCT)
    })
  })

  reactive_code <- reactiveValues(code = "df")

  # if(selected_function == "select"){
  #   cols <- input$SELEC_COLS
  #   cols <- sprintf("%s", paste(sprintf('%s', cols), collapse = ", "))
  #
  #   reactive_code$code <- paste(reactive_code$code, "select(", cols, ")", sep = "")
  # }

  output$FUNCTION_INPUTS <- renderUI({
    req(input$CHECK_FCT)

    if (selected_function() == "select") {
      # Renderizar os inputs específicos para a função select()
      # Exemplo: input para selecionar as colunas
      selectInput(inputId = "SELEC_COLS",
                  label = "Select the columns:",
                  choices = colnames(data()),
                  multiple = TRUE)
    } else if (selected_function() == "filter") {
      # Renderizar os inputs específicos para a função filter()
      # Exemplo: inputs para selecionar a coluna, a condição e o valor
      tagList(
        selectInput(inputId = "SELEC_COL_FILTER",
                    label = "Select the column:",
                    choices = colnames(data())),
        selectInput(inputId = "SELEC_CONDT",
                    label = "Select a condition:",
                    choices = c("igual a", "maior que", "menor que", "maior ou igual a", "menor ou igual a", "diferente de"))
      )

    }
  })

  output$EXTRA_INPUTS <- renderUI({
    req(input$SELEC_COL_FILTER)

    if(isTruthy(input$SELEC_COL_FILTER)){
      selected_column <- input$SELEC_COL_FILTER
      column_class <- class(data()[[selected_column]])

      switch(column_class,
             "numeric" = numericInput(inputId = "FILTER_VALUE",
                                      label = "Value to filter by:",
                                      value = 0),
             "integer" = numericInput(inputId = "FILTER_VALUE",
                                      label = "Value to filter by:",
                                      value = 0),
             "character" = selectInput(inputId = "FILTER_VALUE",
                                       label = "Value to filter by:",
                                       choices = unique(data()[[selected_column]]),
                                       multiple = TRUE),
             "factor" = selectInput(inputId = "FILTER_VALUE",
                                    label = "Value to filter by:",
                                    choices = levels(data()[[selected_column]])),
             # Caso o tipo da coluna não seja numeric, character ou factor
             p("Aviso: Tipo de coluna não suportado para filtro.")
      )
    }


  })

  observe({
    if(selected_function() == "select"){
      # restoreInput("CHECK_FCT", 0)

      cols <- input$SELEC_COLS
      cols <- sprintf("%s", paste(sprintf('%s', cols), collapse = ", "))
      reactive_code$code <- paste0(isolate(reactive_code$code), " %>% select(", cols, ")")

      data <- data() %>%
        select(input$SELEC_COLS)
    }

    else if(selected_function() == "filter"){
      condition <- logical_condition(input$SELEC_CONDT)

      filtering <- sprintf("%s %s '%s'", input$SELEC_COL_FILTER, condition, input$FILTER_VALUE)
      filter_code <- paste0(isolate(reactive_code$code), " %>% filter(rlang::eval_tidy(rlang::parse_expr(filtering)))")
      reactive_code$code <- paste0(isolate(reactive_code$code), " %>% filter(", input$SELEC_COL_FILTER, " ", condition, " ", input$FILTER_VALUE, ")")

      # filter_code <- "data() %>% filter(rlang::eval_tidy(rlang::parse_expr(filtering)))"

      data <- rlang::eval_tidy(rlang::parse_expr(filter_code))
    }
  })

  output$DATA <- DT::renderDT({
    data() %>%
        DT::datatable(escape = F, rownames = F, class = "cell-border compact",
                      options = list(ordering = T, autowidth = F, scrollX = TRUE, scrollY = 300, paging = FALSE,
                                     language = list(lengthMenu = "_MENU_"), lengthChange = FALSE, dom = "<<\"datatables-scroll\"t>>",
                                     columnDefs = list(list(className = "dt-center", targets = "_all"))
                                     ),
                      filter = "top", selection = "none")
    })

  # output$CODE <- renderPrint({
  #   print(reactive_code$code)
  # })

  output$DEBUG <- renderPrint({
    print(str(reactiveValuesToList(reactive_code)))
  })
}

shinyApp(ui, server)

