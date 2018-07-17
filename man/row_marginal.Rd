\name{row_marginal}
\alias{row_marginal}
\title{Row marginal table from contingency table}
\usage{
row_marginal(contingencytable)
}

\arguments{
\item{contingencytable}{ A censored contingency table.  Accepted table classes are a data.frame or matrix.}
}

\value{A data.frame univariate frequency table from the row marginals. }

\description{This function inputs a contingency table and produces a univariate frequency table from the row marginals.}



\details{
\strong{Table Format:}\cr
The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G). 

\itemize{
\item left censoring: <, L, <=, LE
\item interval censoring: - or I (symbol has to be placed in the middle of the two category values)
\item right censoring: >, >=, +, G, GE
\item uncensored: no symbol (only provide category value)
}

The column names should be the Y category values. Row names should never be placed in this table, the default should always be 1:n.  The first column should be the X category values. The inside of the table are X * Y cross tabulation, which are either positive frequency values or percentages. The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The table below is a formatted example with percentages as the cross tabulations and the bottom right corner as a total sum.  

  \tabular{ccccc}{
    NA \tab <20 \tab 20-30 \tab >30\tab NA\cr
    <5 \tab 0.18 \tab 0.19 \tab 0.08\tab 0.45\cr
    5-9 \tab 0.13 \tab 0.08 \tab 0.12\tab 0.33\cr
    >=10 \tab 0.07 \tab 0.05 \tab 0.10\tab 0.21\cr
    NA \tab 0.38 \tab 0.32 \tab 0.31\tab 1.00\cr
  }
}




\examples{
# contingencytable.csv is a preloaded contingency table example
row_marginal(contingencytable.csv)
}
