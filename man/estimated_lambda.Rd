\name{estimated_lambda}
\alias{estimated_lambda}
\title{Estimating lambda value from a univariate table}
\usage{
estimated_lambda(censoredtable, upperbound, quasipoisson_phi)
}
\arguments{
\item{censoredtable}{A frequency table (censored and/or uncensored).  A data.frame and matrix are acceptable table classes.}

\item{upperbound}{A numeric class value to represent a right-truncated point.  The value cannot be less than the highest category value (e.g. the upper bound cannot be 90 if a table has a '>100' category).}

\item{quasipoisson_phi}{A numeric class value to help with overdispersion found in the censored table.  If the value = 1, the original Poisson mean-variance relationship holds (mean = variance).  When the value is > 1, the user is accounting for overdispersion (variance becomes proportional to the mean by quasipoisson_phi value).  The value must strictly be >= 1.}
}

\value{
The estimated_lambda function outputs a numeric class lambda (average) estimate from the provided censored and/or uncensored frequency table.
}

\description{
A censored frequency table is fit to a Poisson or Quasi-Poisson truncated distribution using a likelihood function customized to handle left, right, and interval censored data.  estimated_lambda outputs the lambda value that maximizes the function.  It is recommended for a user to read the Details section below and more information can be found in the vignettes.
}

\details{

\strong{Tables:}
The censored table for the estimated_lambda function has restrictions. The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with the frequencies in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G). 

\itemize{
\item left censoring: <, L, <=, LE
\item interval censoring: - or I (symbol has to be placed in the middle of the two category values)
\item right censoring: >, >=, +, G, GE
\item uncensored: no symbol (only provide category value)
}

Below are three correctly formatted tables.  

\tabular{cc}{
    Category\tab Frequency\cr
    <=6\tab 11800 \cr
    7-12\tab 57100 \cr
    13-19\tab 14800 \cr
    20+\tab 3900 \cr
  }
  
  \tabular{cc}{
    Category\tab Frequency\cr
    LE6\tab 11800 \cr
    7I12\tab 57100 \cr
    13I19\tab 14800 \cr
    GE20\tab 3900 \cr
  }
  
    \tabular{cc}{
    Category\tab Frequency\cr
    <7\tab 11800 \cr
    7I12\tab 57100 \cr
    13-19\tab 14800 \cr
    >=20\tab 3900 \cr
  }

\strong{Scalar for Quasi-Poisson:} \cr
In a Poisson distribution, the variance equals the mean \eqn{\mu} \cr

\eqn{Var(Y) = E(Y) = \mu}

To combat overdispersion, this function considers the option where the variance is proportional to the mean by a scalar value of \eqn{\phi}.  This changes Poisson to Quasi-Poisson. 

\eqn{Var(Y) = \phi E(Y) = \phi \mu}

\itemize{
\item {If \eqn{\phi} = 1, the variance equals the mean and the original Poisson mean-variance relationship holds.}
\item{Overdispersion in data refers to when the variance is larger than the mean.  To accommodate this issue, set \eqn{\phi} > 1. }
\item{Underdispersion in data refers to when the variance is smaller than the mean.  However, underdispersion is a rare case and is not accommodated for in this package.}
}

\strong{Upper Bound:} \cr
The upper bound represents a right-truncated point for the table.  Values that exceed the upper bound are assumed to have very small probabilities worth refining to zero, and thus these values are omitted in calculating the fitted Poisson (\eqn{\phi} = 1) or Quasi-Poisson (\eqn{\phi} > 1) truncated distribution.  Typically, setting the upper bound value between +10 and +20 of the last categorical value is sufficient.  
}

\examples{
# replicate results
set.seed(123)

# create frequency table that follows a Poisson distribution
testdata<-table(rpois(100000,lambda= 10))
testdata<-data.frame(testdata)

# lambda value from uncensored table is 10 
# table follows Poisson distribution so quasipoisson_phi = 1
estimated_lambda(testdata, 40, 1)

# censor the table above
testdata<-cbind(as.character(c("<=5", "6-10", "11-15", "16-20", ">20")), 
  c(6718,51329,37041,4732, 180))
estimated_lambda(testdata, 40, 1)

# same lambda result using different symbols for censoring
testdata<-cbind(as.character(c("LE5", "6I10", "11I15", "16I20", "G20")), 
  c(6718,51329,37041,4732, 180))
estimated_lambda(testdata, 40, 1)
}


\keyword{Poisson}
\keyword{univariate table}
\keyword{frequency table}
\keyword{count data}
\keyword{censored}
\keyword{lambda}
\keyword{Quasi-Poisson}
\keyword{overdispersion}
\keyword{truncated}
