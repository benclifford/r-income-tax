library(shiny)

source("income-tax.R")

server <- function(input, output) {

  output$myPlot <- renderPlot(
    if(input$graphtype == "marginal") {
      marginal_graph(input)
    } else if(input$graphtype == "total"){
      total_graph(input)
    } else {
      overall_graph(input)
    }
   )

}

ui <- fluidPage(
  titlePanel("UK income tax explorer"),
  sidebarLayout(
    sidebarPanel(

# GUI controls
      sliderInput("maxincome", "Maximum income on graph/£:", min = 0, max = 1000000, value = 200000),
      selectInput("graphtype", "Graph type:", c("Marginal rate" = "marginal", "Total paid" = "total", "Overall rate" = "overall")),


# Income tax
      sliderInput("personal_allowance", "Personal allowance/£:", min = 0, max = 100000, value = realparam$personal_allowance),
      sliderInput("basic_rate", "Basic rate of income tax:", min = 0, max = 1, value = realparam$basic_rate),
      sliderInput("higher_rate_threshold", "Higher rate threshold/£:", min = 0, max = 100000, value = realparam$higher_rate_threshold),
      sliderInput("higher_rate", "Higher rate of income tax:", min = 0, max = 1, value = realparam$higher_rate),
      sliderInput("additional_rate_threshold", "Additional rate threshold/£:", min = 50000, max = 300000, value = realparam$additional_rate_threshold),
      sliderInput("additional_rate", "Additional rate of income tax:", min = 0, max = 1, value = realparam$additional_rate),

      sliderInput("personal_allowance_withdrawl_unit", "Personal allowance withdrawl unit/£ per £-income:", min = 0, max = 20, value = realparam$personal_allowance_withdrawl_unit),
      sliderInput("personal_allowance_withdrawl_limit", "Personal allowance withdrawl limit/£:", min = 0, max = 200000, value = realparam$personal_allowance_withdrawl_limit),

# National insurance
      p(paste("National Insurance: assuming class 1, category A")),

      sliderInput("primary_threshold", "NI primary threshold/£:", min = 0, max = 10000, value = realparam$primary_threshold),
      sliderInput("primary_rate", "NI primary rate:", min = 0, max = 1, value = realparam$primary_rate),
      sliderInput("upper_earnings_limit", "NI upper earnings limit/£:", min = 0, max = 10000, value = realparam$upper_earnings_limit),
      sliderInput("upper_rate", "NI upper rate:", min = 0, max = 1, value = realparam$upper_rate),


      p(paste("Student loans: assuming large Plan 2 loan")),
# Student loans
      sliderInput("student_loan_threshold", "Student loan threshold/£:", min = 0, max = 100000, value = realparam$student_loan_threshold),
      sliderInput("student_loan_rate", "Student loan rate:", min = 0, max = 1, value = realparam$student_loan_rate),

    p(a("Source/issue tracker", href="https://github.com/benclifford/r-income-tax"))

    ),
    mainPanel(plotOutput("myPlot"))
  )
)

shinyApp(ui = ui, server = server)
