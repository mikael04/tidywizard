library(shiny)
library(shinydashboard)
"%>%" <- magrittr::"%>%"

df <- ggplot2::mpg

choices_fct <- c("select - select columns",
                 "filter - filter rows",
                 "mutate - create/change columns",
                 "arrange - order rows",
                 "count - count values of columns")

# parameter_tabs <- tabsetPanel(
#   id = "FILTER_TYPES",
#   type = "hidden",
#   tabPanel("character",
#            numericInput("mean", "mean", value = 1),
#            numericInput("sd", "standard deviation", min = 0, value = 1)
#   ),
#   tabPanel("numeric",
#            numericInput("min", "min", value = 0),
#            numericInput("max", "max", value = 1)
#   )
# )

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

  # code <- eventReactive(input$EXEC_FCT, {
  #   if (selected_function == "select()") {
  #     data() %>%
  #       dplyr::select(input$SELEC_COLS)
  #   }
  # })


  output$FUNCTION_INPUTS <- renderUI({
    req(input$CHECK_FCT)

    # selected_function <- isolate({
    #   gsub("\\s*-.*", "", input$SELEC_FCT)
    # })

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
  })

  output$DATA <- DT::renderDT({
    if(input$EXEC_FCT == 0){
      data() %>%
        DT::datatable(escape = F, rownames = F, class = "cell-border compact",
                      options = list(ordering = T, autowidth = F, scrollX = TRUE, scrollY = 300, paging = FALSE,
                                     language = list(lengthMenu = "_MENU_"), lengthChange = FALSE, dom = "<<\"datatables-scroll\"t>>",
                                     columnDefs = list(list(className = "dt-center", targets = "_all"))
                                     ),
                      filter = "top", selection = "none")
    }

    else if(input$EXEC_FCT != 0){
      # fun <- get(selected_function(), asNamespace("dplyr"))

      if(selected_function() == "select"){
        data() %>%
          select(input$SELEC_COLS) %>%
          DT::datatable(escape = F, rownames = F, class = "cell-border compact",
                        options = list(ordering = T, autowidth = F, scrollX = TRUE, scrollY = 300, paging = FALSE,
                                       language = list(lengthMenu = "_MENU_"), lengthChange = FALSE, dom = "<<\"datatables-scroll\"t>>",
                                       columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ),
                        filter = "top", selection = "none")

        # do.call(fun, args = list(
        #   data(),
        #   input$SELEC_COLS
        # ))
      }

      else if(selected_function() == "filter"){
        condition <- switch(input$SELEC_CONDT,
                            "igual a" = "==",
                            "maior que" = ">",
                            "menor que" = "<",
                            "maior ou igual a" = ">=",
                            "menor ou igual a" = "<=",
                            "diferente de" = "!="
        )

        filtering <- sprintf("%s %s '%s'", input$SELEC_COL_FILTER, condition, input$FILTER_VALUE)

        code <- "data() %>% filter(rlang::eval_tidy(rlang::parse_expr(filtering)))"

        rlang::eval_tidy(rlang::parse_expr(code)) %>%
          DT::datatable(escape = F, rownames = F, class = "cell-border compact",
                        options = list(ordering = T, autowidth = F, scrollX = TRUE, scrollY = 300, paging = FALSE,
                                       language = list(lengthMenu = "_MENU_"), lengthChange = FALSE, dom = "<<\"datatables-scroll\"t>>",
                                       columnDefs = list(list(className = "dt-center", targets = "_all"))
                        ),
                        filter = "top", selection = "none")

        # do.call(fun, args = list(
        #   data(),
        #   rlang::eval_tidy(rlang::parse_expr(filtering))
        # ))
      }


    }
  })

  output$DEBUG <- renderPrint({
    print(str(reactiveValuesToList(input)))
  })
}

shinyApp(ui, server)

