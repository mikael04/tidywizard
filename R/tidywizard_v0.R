library(shiny)
library(shinydashboard)
library(rintrojs)
filter <- get("filter", asNamespace("dplyr"))
select <- get("select", asNamespace("dplyr"))

"%>%" <- magrittr::"%>%"
source("R/logical_condition.R")

intro <- readr::read_csv2("intro.csv",
                          locale = readr::locale(encoding = "latin1"))

df <- ggplot2::mpg

choices_fct <- c("select - Selecione colunas",
                 "filter - Filtre linhas",
                 "mutate - Crie/altere colunas",
                 "summarise - Resuma seus dados",
                 "arrange - Ordene as colunas",
                 "count - Conte os valores")

ui <- shinydashboard::dashboardPage(
  ## Header
  shinydashboard::dashboardHeader(
    titleWidth = "200",
    title = div(
      span(
        img(
          src = knitr::image_uri("www/logo_tidywizard.png"),
          width = "22%"
        ),
        "tidywizard"
      ),
      align = "left",
      style = "display:flex;"
    )
  ),
  ## Sidebar
  shinydashboard::dashboardSidebar(width = "200",
    shinydashboard::sidebarMenu(id = "TABS",
      # shinydashboard::menuItem(tabName = "TAB_HOME",
      #   "Home", icon = icon("house")
      # ),
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
    rintrojs::introjsUI(),
    tags$head(
      tags$style(HTML("
                      .tour_button {
                      font-weight: bold;
                      color:white;
                      }"))
    ),

    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "TAB_DATA",
                              fluidRow(
                                column(
                                  width = 4,
                                  fluidRow(
                                    rintrojs::introBox(data.step = 1, data.intro = intro$text[1],
                                                       data.position = "right",
                                                       box(
                                                         height = "110",
                                                         width = 12,
                                                         title = "Escolha uma função",
                                                         div(style = "display:inline-block",
                                                             selectInput("SELEC_FCT", label = NULL,
                                                                         width = "200px",
                                                                         choices = choices_fct)),
                                                         div(style = "display:inline-block",
                                                             actionButton("CHECK_FCT", label = "Selecione a função",
                                                                          style = "margin-top:-27px",
                                                                          icon = icon("check")))
                                                         )
                                                       )
                                  ),
                                  fluidRow(
                                    introBox(data.step = 2, data.intro = intro$text[2],
                                             data.position = "right",
                                             box(
                                               height = "350",
                                               width = 12,
                                               title = "Aplique a função",
                                               uiOutput("FUNCTION_INPUTS"),
                                               uiOutput("EXTRA_INPUTS"),
                                               rintrojs::introBox(data.step = 3, data.intro = intro$text[3],
                                                                  data.position = "right",
                                                                  actionButton(inputId = "EXEC_FCT", label = "Execute a função", icon = icon("filter"),
                                                                               style = "border-width: 2px;")
                                                        )
                                               )
                                             )
                                  )
                                ),
                                column(
                                  width = 8,
                                  box(
                                    height = "480",
                                    width = NULL,
                                    title = "Seus dados",
                                    rintrojs::introBox(data.step = 4, data.intro = intro$text[4],
                                                       data.position = "bottom",
                                                       DT::DTOutput("DATA") %>%
                                                         shinycssloaders::withSpinner()
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 12,
                                  box(
                                    height = "300",
                                    width = NULL,
                                    title = "Código de R",
                                    rintrojs::introBox(data.step = 5, data.intro = intro$text[5],
                                                       data.position = "top",
                                                       verbatimTextOutput("CODE")
                                             )
                                    )
                                  )
                                )
                              )
      )
    )
)

server <- function(input, output, session){
  observeEvent("", {
    showModal(modalDialog(
      # includeHTML("../intro_text.html"),
      HTML(paste0(htmltools::div(style = 'position:relative;',
                                 img(src = knitr::image_uri("www/logo_tidywizard.png"),
                                     style = 'display: block; margin-left: auto; margin-right: auto;',
                                     width = "30%")),
                  HTML('<h1 style="text-align: center;">Seja bem-vindo ao&nbsp;<code><strong>tidywizard</strong></code></h1>
                   <h4 style="text-align: center;">Uma aplicação com objetivo de facilitar a manipulação de dados enquanto o usuário aprende.</h4>
                   <hr />
                   <p style="text-align: left;font-size:20px;">O tidywizard está em processo de desenvolvimento, contudo ele já permite ao usuário fazer uso de funções corriqueiras do dia-a-dia da análise de dados, como <code>select</code> e <code>filter</code> do pacote <code>dplyr</code>.</a> </p>
                   <hr />
                   ')
      )),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "INTRO", label = "IR À INTRODUÇÃO", icon = icon("info-circle"))
      )
    ))
  })

  observeEvent(input$INTRO, {
    removeModal()
  })

  observeEvent(input$INTRO, {
    introjs(
      session,
      options = list("nextLabel" = "Próximo",
                     "prevLabel" = "Anterior",
                     "doneLabel" = "Finalizar")
    )
  })

  selected_function <- eventReactive(input$CHECK_FCT, {
    isolate({
      gsub("\\s*-.*", "", input$SELEC_FCT)
    })
  })

  reactive_values <- reactiveValues(data = df,
                                    code = "df")

  output$FUNCTION_INPUTS <- renderUI({
    req(input$CHECK_FCT)

    if (selected_function() == "select") {
      # Renderizar os inputs específicos para a função select()
      # Exemplo: input para selecionar as colunas
      selectInput(inputId = "SELEC_COLS",
                  label = "Selecione as colunas",
                  choices = colnames(reactive_values$data),
                  multiple = TRUE)
    } else if (selected_function() == "filter") {
      # Renderizar os inputs específicos para a função filter()
      # Exemplo: inputs para selecionar a coluna, a condição e o valor
      tagList(
        selectInput(inputId = "SELEC_COL_FILTER",
                    label = "Selecione a coluna",
                    choices = colnames(reactive_values$data),
                    selected = NULL),
        selectInput(inputId = "SELEC_CONDT",
                    label = "Selecione a condição",
                    choices = c("igual a", "maior que", "menor que", "maior ou igual a", "menor ou igual a", "diferente de"))
      )

    }
  })

  output$EXTRA_INPUTS <- renderUI({
    req(input$SELEC_COL_FILTER)

    if(isTruthy(input$SELEC_COL_FILTER)){
      selected_column <- input$SELEC_COL_FILTER
      column_class <- class(reactive_values$data[[selected_column]])

      switch(column_class,
             "numeric" = numericInput(inputId = "FILTER_VALUE",
                                      label = "Valor a filtrar",
                                      value = 0),
             "integer" = numericInput(inputId = "FILTER_VALUE",
                                      label = "Valor a filtrar",
                                      value = 0),
             "character" = selectInput(inputId = "FILTER_VALUE",
                                       label = "Valor a filtrar",
                                       choices = unique(reactive_values$data[[selected_column]]),
                                       multiple = TRUE),
             "factor" = selectInput(inputId = "FILTER_VALUE",
                                    label = "Valor a filtrar",
                                    choices = levels(reactive_values$data[[selected_column]])),
             # Caso o tipo da coluna não seja numeric, character ou factor
             p("Aviso: Tipo de coluna não suportado para filtro.")
      )
    }


  })

  observe({
    if(selected_function() == "select"){
      cols <- input$SELEC_COLS
      cols <- sprintf("%s", paste(sprintf("%s", cols), collapse = ", "))

      reactive_values$code <- paste0(isolate(reactive_values$code), " %>% select(", cols, ")")
      reactive_values$data <- reactive_values$data %>%
        select(input$SELEC_COLS)
    }

    else if(selected_function() == "filter"){
      condition <- logical_condition(input$SELEC_CONDT)
      filtering <- sprintf("%s %s '%s'", input$SELEC_COL_FILTER, condition, input$FILTER_VALUE)
      filter_code <- paste0(isolate(reactive_values$code), " %>% filter(rlang::eval_tidy(rlang::parse_expr(filtering)))")

      reactive_values$code <- paste0(isolate(reactive_values$code), " %>% filter(", input$SELEC_COL_FILTER, " ", condition, " '", input$FILTER_VALUE, "')")
      reactive_values$data <- rlang::eval_tidy(rlang::parse_expr(filter_code))
    }

    # updateSelectInput(inputId = "SELEC_COL_FILTER",
    #                   label = "Selecione a coluna",
    #                   choices = colnames(reactive_values$data),
    #                   selected = NULL)
  }) %>%
    bindEvent(input$EXEC_FCT)

  output$DATA <- DT::renderDT({
    rlang::eval_tidy(rlang::parse_expr(reactive_values$code))%>%
        DT::datatable(escape = F, rownames = F, class = "cell-border compact",
                      options = list(ordering = T, autowidth = F, scrollX = TRUE, scrollY = 300, paging = FALSE,
                                     language = list(lengthMenu = "_MENU_"), lengthChange = FALSE, dom = "<<\"datatables-scroll\"t>>",
                                     columnDefs = list(list(className = "dt-center", targets = "_all"))
                                     ),
                      filter = "top", selection = "none")
    })

  output$CODE <- renderPrint({
    cat(reactive_values$code)
  })

  # output$DEBUG <- renderPrint({
  #   print(str(reactiveValuesToList(reactive_values)))
  # })
}

shinyApp(ui, server)
