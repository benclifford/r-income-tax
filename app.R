library(shiny)

source("income-tax.R")

server <- function(input, output) {

  output$marginalPlot <- renderPlot(marginal_graph(input))
  output$totalPlot <- renderPlot(total_graph(input))
  output$overallPlot <- renderPlot(overall_graph(input))

}

ui <- fluidPage(
  titlePanel("UK income tax explorer"),
  sidebarLayout(
    sidebarPanel(

# GUI controls
      sliderInput("maxincome", "Maximum income on graph (£/year)", min = 0, max = 1000000, value = 200000),

# Income tax
      sliderInput("personal_allowance", "Personal allowance (£/year)", min = 0, max = 100000, value = realparam$personal_allowance),
      sliderInput("basic_rate_percent", "Basic rate of income tax (%)", min = 0, max = 100, value = realparam$basic_rate * 100),
      sliderInput("higher_rate_threshold", "Higher rate threshold (£/year)", min = 0, max = 100000, value = realparam$higher_rate_threshold),
      sliderInput("higher_rate_percent", "Higher rate of income tax (%)", min = 0, max = 100, value = realparam$higher_rate * 100),
      sliderInput("additional_rate_threshold", "Additional rate threshold (£/year)", min = 50000, max = 300000, value = realparam$additional_rate_threshold),
      sliderInput("additional_rate_percent", "Additional rate of income tax (%)", min = 0, max = 100, value = realparam$additional_rate * 100),

      sliderInput("personal_allowance_withdrawl_unit", "Personal allowance withdrawl unit (£1 withdrawn per £n of additional income)", min = 0, max = 20, value = realparam$personal_allowance_withdrawl_unit),
      sliderInput("personal_allowance_withdrawl_limit", "Personal allowance withdrawl limit (£/year)", min = 0, max = 200000, value = realparam$personal_allowance_withdrawl_limit),

# National insurance
      p(paste("National Insurance: assuming class 1, category A")),

      sliderInput("primary_threshold", "NI primary threshold (£/month)", min = 0, max = 10000, value = realparam$primary_threshold),
      sliderInput("primary_rate_percent", "NI primary rate (%)", min = 0, max = 100, value = realparam$primary_rate * 100),
      sliderInput("upper_earnings_limit", "NI upper earnings limit (£/month)", min = 0, max = 10000, value = realparam$upper_earnings_limit),
      sliderInput("upper_rate_percent", "NI upper rate (%)", min = 0, max = 100, value = realparam$upper_rate * 100),


      p(paste("Student loans: assuming large Plan 2 loan")),
# Student loans
      sliderInput("student_loan_threshold", "Student loan threshold (£/year)", min = 0, max = 100000, value = realparam$student_loan_threshold),
      sliderInput("student_loan_rate_percent", "Student loan rate (%)", min = 0, max = 100, value = realparam$student_loan_rate * 100),

    p(a("Source/issue tracker", href="https://github.com/benclifford/r-income-tax"))

    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Marginal rate", plotOutput("marginalPlot")),
        tabPanel("Total", plotOutput("totalPlot")),
        tabPanel("Overall rate", plotOutput("overallPlot"))
      )
    )
  )
)

shinyApp(ui = ui, server = server)
