# source this file with source('income-tax.R')

library(ggplot2)
library(reshape2)

# this should be an array of gross incomes over which we
# want to plot our data

# few_incomes is a replacement for gross_incomes which can be
# used for quick experimentations - graphs plot much faster with
# this much smaller dataset.
few_incomes <- c(100,10000, 15000, 15100, 44999,45001, 46000, 60000, 149000, 152000, 200000)

gross_incomes <- seq(1,200000, by=50)

# parameters defined by government:

# INCOME TAX #

# all of these sterling amounts are per annum.

# you get this much tax free:
personal_allowance <- 11500

# after which you're taxed at the Basic Rate:
basic_rate <- 0.2

# basic rate tax band is up to this much (after allowances):
higher_rate_threshold <- 33500

# at which point you are taxed at the Higher Rate:
higher_rate <- 0.4

# and then when you reach this much (after allowances):
additional_rate_threshold <- 150000

# you are taxed at the Additional Rate:
additional_rate <- 0.45 

# Personal allowance progressive withdrawl

# Your personal allowance goes down by one pound for
# every n pounds that your gross income is over the
# limit:
personal_allowance_withdrawl_unit <- 2
personal_allowance_withdrawl_limit <- 100000


# NATIONAL INSURANCE #

# these figures are class 1, regular employee amounts
# The UK government gives these in monthly, not annual, amounts
# so all of these sterling amounts are per calendar month

# the primary threshold is:
primary_threshold <- 680

# above the primary threshold
# to the upper earnings limit
# you pay:
primary_rate <- 0.12

# the upper earnings limit is:
upper_earnings_limit <- 3750

# above the upper earnings limit
# you pay:
upper_rate <- 0.02


# STUDENT LOAN #

# if your income is above (per annum)
student_loan_threshold = 21000

# then you will repay this much of your
# income over that:
student_loan_rate = 0.09

# End of government parameters #

basic_tax <- function(adj) { 
  max(0, adj * 0.2)
}

higher_tax <- function(adj) {
  max(0, (adj - higher_rate_threshold) * (higher_rate - basic_rate))
}

additional_tax <- function(adj) {
  max(0, (adj - additional_rate_threshold) * (additional_rate - higher_rate))
}

basic_tax_pa <- function(gross) {
  basic_tax(gross - personal_allowance)
}

higher_tax_pa <- function(gross) {
  higher_tax(gross - personal_allowance)
}

additional_tax_pa <- function(gross) {
  additional_tax(gross - personal_allowance)
}

tax_function <- function(gross, allowance) {
  post_allowance <- gross - allowance
  basic_tax(post_allowance) + higher_tax(post_allowance) + additional_tax(post_allowance)
}


# this doesn't work so well as a cumulation of rates, because the basic
# rate is 12%, and then higher than that is effectively a -10% on top
# of that. So will compute it as a single value, rather than tiers.
national_insurance_basic <- function(gross) {
  max(0, (gross - (primary_threshold * 12)) * primary_rate)
}

# this one looks a bit different to the others because it *can* give
# a negative contribution. max is moved to a different position, that
# might also work in the others?
national_insurance_higher <- function(gross) {
  max(0,(gross - (upper_earnings_limit * 12))) * (upper_rate - primary_rate)
}

national_insurance <- function(gross) {
  national_insurance_basic(gross) + national_insurance_higher(gross)
}


allowance_withdrawl <- function(gross) {
 withdrawn_allowance <- max(0, min(personal_allowance, (gross - personal_allowance_withdrawl_limit) / personal_allowance_withdrawl_unit))
 remaining_allowance <- personal_allowance - withdrawn_allowance
 tax_function(gross, remaining_allowance) - tax_function(gross, personal_allowance) 
}

student_loan <- function(gross) {
  max(0, (gross - student_loan_threshold) * student_loan_rate)
}

fr <- function(incomes) {
  x <- data.frame(incomes,
                        sapply(incomes, basic_tax_pa),
                        sapply(incomes, higher_tax_pa),
                        sapply(incomes, additional_tax_pa),
                        sapply(incomes, allowance_withdrawl),
                        sapply(incomes, national_insurance),
                        sapply(incomes, student_loan)
                       )
  colnames(x) <- c('income', "Basic","Higher","Additional","Allowance withdrawal", "National Insurance", "Student loan")
  x
}

tax_frame <- fr(gross_incomes)
few_tax_frame <- fr(few_incomes)

# produces a stacked plot of how each much tax comes from each
# component in total
ggplot(melt(tax_frame, id.vars='income'),
       aes(x=income, y=value, fill=variable)) + geom_area()

# i would like each row to be replaced with the difference
# with the previous row, in tax_frame (except column 1, incomes)
# and then normalised by the difference in column 1.

tax_matrix <- as.matrix(tax_frame)
marginal_tax_matrix <- tax_matrix[-1,] - tax_matrix[-(dim(tax_matrix)[1]),]

marginal_tax_matrix_normalised <- marginal_tax_matrix / marginal_tax_matrix[,1]

# put the original incomes back in:

marginal_tax_matrix_normalised[,1] <- tax_matrix[-1,1]

ggplot(melt(as.data.frame(marginal_tax_matrix_normalised), id.vars='income'), aes(x=income, y=value, fill=variable)) + geom_area() + scale_y_continuous(labels = scales::percent) + ylab("Marginal tax rate")  + scale_fill_manual(values=c("#FF0000","#FF5555","#FFAAAA", "#FFFF00", "#FF00FF", "#00FFFF"))


total_tax <- rowSums(tax_frame[-1])
total_tax_fraction_of_income <- data.frame(tax_frame[1], total_tax / tax_frame[1])

ggplot(melt(total_tax_fraction_of_income, id.vars='income'), aes(x=income, y=value)) + geom_area() + scale_y_continuous(labels = scales::percent) + ylab("Percentage of income taken as tax")

