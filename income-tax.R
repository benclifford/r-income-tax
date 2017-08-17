# source this file with source('income-tax.R')

library(ggplot2)
library(reshape2)

# this should be an array of gross incomes over which we
# want to plot our data

few_incomes <- c(100,10000, 15000, 44999,45001, 46000, 60000, 149000, 152000, 200000)

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

fr <- function(incomes) {
  data.frame(incomes,  
                        sapply(incomes, basic_tax_pa),
                        sapply(incomes, higher_tax_pa),
                        sapply(incomes, additional_tax_pa),
                        sapply(incomes, national_insurance)
                       )
}

tax_frame <- fr(gross_incomes)
few_tax_frame <- fr(few_incomes)

ggplot(melt(tax_frame, id.vars="incomes"),
       aes(x=incomes, y=value, fill=variable)) + geom_area()

