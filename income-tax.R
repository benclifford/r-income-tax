# source this file with source('income-tax.R')

library(ggplot2)
library(reshape2)

# parameters defined by government:

realparam = list()

# INCOME TAX #

# all of these sterling amounts are per annum.

# you get this much tax free:
realparam$personal_allowance <- 11500

# after which you're taxed at the Basic Rate:
realparam$basic_rate <- 0.2

# basic rate tax band is up to this much (after allowances):
realparam$higher_rate_threshold <- 33500

# at which point you are taxed at the Higher Rate:
realparam$higher_rate <- 0.4

# and then when you reach this much (after allowances):
realparam$additional_rate_threshold <- 150000

# you are taxed at the Additional Rate:
realparam$additional_rate <- 0.45 

# Personal allowance progressive withdrawl

# Your personal allowance goes down by one pound for
# every n pounds that your gross income is over the
# limit:
realparam$personal_allowance_withdrawl_unit <- 2
realparam$personal_allowance_withdrawl_limit <- 100000


# NATIONAL INSURANCE #

# these figures are class 1, regular employee amounts
# The UK government gives these in monthly, not annual, amounts
# so all of these sterling amounts are per calendar month

# the primary threshold is:
realparam$primary_threshold <- 680

# above the primary threshold
# to the upper earnings limit
# you pay:
realparam$primary_rate <- 0.12

# the upper earnings limit is:
realparam$upper_earnings_limit <- 3750

# above the upper earnings limit
# you pay:
realparam$upper_rate <- 0.02


# STUDENT LOAN #

# if your income is above (per annum)
realparam$student_loan_threshold = 21000

# then you will repay this much of your
# income over that:
realparam$student_loan_rate = 0.09

# End of government parameters #

basic_tax <- function(adj, param) { 
  max(0, adj * param$basic_rate)
}

higher_tax <- function(adj, param) {
  max(0, (adj - param$higher_rate_threshold) * (param$higher_rate - param$basic_rate))
}

additional_tax <- function(adj, param) {
  max(0, (adj - param$additional_rate_threshold) * (param$additional_rate - param$higher_rate))
}

basic_tax_pa <- function(gross, param) {
  basic_tax(gross - param$personal_allowance, param)
}

higher_tax_pa <- function(gross, param) {
  higher_tax(gross - param$personal_allowance, param)
}

additional_tax_pa <- function(gross, param) {
  additional_tax(gross - param$personal_allowance, param)
}

tax_function <- function(gross, allowance, param) {
  post_allowance <- gross - allowance
  basic_tax(post_allowance, param) + higher_tax(post_allowance, param) + additional_tax(post_allowance, param)
}


# this doesn't work so well as a cumulation of rates, because the basic
# rate is 12%, and then higher than that is effectively a -10% on top
# of that. So will compute it as a single value, rather than tiers.
national_insurance_basic <- function(gross, param) {
  max(0, (gross - (param$primary_threshold * 12)) * param$primary_rate)
}

# this one looks a bit different to the others because it *can* give
# a negative contribution. max is moved to a different position, that
# might also work in the others?
national_insurance_higher <- function(gross, param) {
  max(0,(gross - (param$upper_earnings_limit * 12))) * (param$upper_rate - param$primary_rate)
}

national_insurance <- function(gross, param) {
  national_insurance_basic(gross, param) + national_insurance_higher(gross, param)
}


allowance_withdrawl <- function(gross, param) {
 withdrawn_allowance <- max(0, min(param$personal_allowance, (gross - param$personal_allowance_withdrawl_limit) / param$personal_allowance_withdrawl_unit))
 remaining_allowance <- param$personal_allowance - withdrawn_allowance
 tax_function(gross, remaining_allowance, param) - tax_function(gross, param$personal_allowance, param) 
}

student_loan <- function(gross, param) {
  max(0, (gross - param$student_loan_threshold) * param$student_loan_rate)
}

fr <- function(incomes, param) {
  x <- data.frame(incomes,
                        sapply(incomes, function(i) basic_tax_pa(i,param)),
                        sapply(incomes, function(i) higher_tax_pa(i,param)),
                        sapply(incomes, function(i) additional_tax_pa(i,param)),
                        sapply(incomes, function(i) allowance_withdrawl(i,param)),
                        sapply(incomes, function(i) national_insurance(i,param)),
                        sapply(incomes, function(i) student_loan(i,param))
                       )
  colnames(x) <- c('income', "Basic","Higher","Additional","Allowance withdrawal", "National Insurance", "Student loan")
  x
}


income_range<- function(param) {
 seq(1,param$maxincome, length.out=500)
}

tax_frame_fn <- function(param) {
  fr(income_range(param), param)
}
marginal_tax_matrix_normalised <- function(param) {

  tax_frame <- tax_frame_fn(param)

  tax_matrix <- as.matrix(tax_frame)

  marginal_tax_matrix <- tax_matrix[-1,] - tax_matrix[-(dim(tax_matrix)[1]),]

  m <- marginal_tax_matrix / marginal_tax_matrix[,1]

  # put the original incomes back in:

  m[,1] <- tax_matrix[-1,1]
  m
}

input_param <- function(input) {
  param <- realparam
  param$maxincome <- input$maxincome
  param$personal_allowance <- input$personal_allowance
  param$basic_rate <- input$basic_rate_percent / 100
  param$higher_rate <- input$higher_rate_percent / 100
  param$higher_rate_threshold <- input$higher_rate_threshold
  param$additional_rate <- input$additional_rate_percent / 100
  param$additional_rate_threshold <- input$additional_rate_threshold
  param$personal_allowance_withdrawl_unit <- input$personal_allowance_withdrawl_unit
  param$personal_allowance_withdrawl_limit <- input$personal_allowance_withdrawl_limit
  param$primary_threshold <- input$primary_threshold
  param$primary_rate <- input$primary_rate_percent / 100
  param$upper_earnings_limit <- input$upper_earnings_limit
  param$upper_rate <- input$upper_rate_percent / 100
  param$student_loan_threshold <- input$student_loan_threshold
  param$student_loan_rate <- input$student_loan_rate_percent / 100
 
  param
}

marginal_graph <- function(input) {
  param <- input_param(input)

  ggplot(melt(as.data.frame(marginal_tax_matrix_normalised(param)), id.vars='income'), aes(x=income, y=value, fill=variable)) + geom_area() + scale_y_continuous(labels = scales::percent) + xlab("Gross income, GBP/year") + ylab("Marginal tax rate")  + scale_fill_manual(values=c("#FF0000","#FF5555","#FFAAAA", "#AA0000", "#44AA44", "#4444AA")) + labs(fill = "Component")

  }

# produces a stacked plot of how each much tax comes from each
# component in total
total_graph <- function(input) {
  param <- input_param(input)

  ggplot(melt(tax_frame_fn(param), id.vars='income'),
       aes(x=income, y=value, fill=variable)) + geom_area() + ylab("Total tax paid, GBP/year") + xlab("Gross income, GBP/year") + labs(fill = "Component")
}

overall_graph <- function(input) {
  param <- input_param(input)

  tax_frame <- tax_frame_fn(param)

  total_tax <- rowSums(tax_frame[-1])
  total_tax_fraction_of_income <- data.frame(tax_frame[1], total_tax / tax_frame[1])

  ggplot(melt(total_tax_fraction_of_income, id.vars='income'), aes(x=income, y=value)) + geom_area() + scale_y_continuous(labels = scales::percent) + ylab("Total tax as percentage of income") + xlab("Gross income, GBP/year")
}

