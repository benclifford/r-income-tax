# source this file with source('income-tax.R')

library(ggplot2)
library(reshape2)

# this should be an array of gross incomes over which we
# want to plot our data

few_incomes <- c(100,10000, 15000, 45000, 46000, 60000, 149000, 152000, 200000)

gross_incomes <- seq(1,200000, by=50)

# parameters defined by government:
# you get this much tax free:
personal_allowance <- 11500

# after which you're taxed at the Basic Rate:
basic_rate <- 0.2

# until you earn this much gross:
higher_rate_threshold <- 45000

# at which point you are taxed at the Higher Rate:
higher_rate <- 0.4

# and then when you reach this much gross:
additional_rate_threshold <- 150000

# you are taxed at the Additional Rate:
additional_rate <- 0.45 

basic_tax <- function(gross) { 
  max(0, (gross - personal_allowance) * 0.2)
}

higher_tax <- function(gross) {
  max(0, (gross - higher_rate_threshold) * (higher_rate - basic_rate))
}

additional_tax <- function(gross) {
  max(0, (gross - additional_rate_threshold) * (additional_rate - higher_rate))
}

tax_function <- function(gross) {
  basic_tax(gross) + higher_tax(gross) + additional_tax(gross)
}

taxes_for_incomes <- sapply(gross_incomes, tax_function)


fr <- function(incomes) {
  data.frame(incomes,  
                        sapply(incomes, basic_tax),
                        sapply(incomes, higher_tax),
                        sapply(incomes, additional_tax)
                       )
}

tax_frame <- fr(gross_incomes)
few_tax_frame <- fr(few_incomes)

ggplot(melt(tax_frame, id.vars="incomes"),
       aes(x=incomes, y=value, fill=variable)) + geom_area()

