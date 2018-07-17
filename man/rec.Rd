\name{rec}
\alias{rec}
\title{Reverse engineering censored and decoupled variables}
\usage{
 rec(X, Y, Xlowerbound, Xupperbound, Ylowerbound,Yupperbound, 
     quasipoisson_phiX, quasipoisson_phiY, seed_estimation_method, lambda_range)
}


\arguments{
\item{X}{Argument can be an average, a univariate frequency table, or a censored contingency table.  The average value should be a numeric class while a data.frame or matrix are acceptable table classes.  If a user inputs a censored contingency table, make Y = 0.}

\item{Y}{Same description as X but this argument is for the Y variable.  Make X = 0 if Y argument is a censored contingency table.}

\item{Xlowerbound}{A numeric class value to represent a right-shift variable for X.  The value must strictly be >= 0 and cannot be greater than the lowest category/average value provided for X (e.g. the lower bound cannot be 6 if a table has '>= 5' as a X or row category). }

\item{Xupperbound}{A numeric class value to represent a right-truncated point for X.  The value cannot be less than the highest category/average value provided for X (e.g. the upper bound cannot be 90 if a table has '> 100' as a X or row category).}

\item{Ylowerbound}{Same description as Xlowerbound but this argument is for Y (column variable in contingency table). }

\item{Yupperbound}{Same description as Xupperbound but this argument is for Y (column variable in contingency table).  }

\item{quasipoisson_phiX}{A numeric class value to help with overdispersion found in X.  If the value = 1, the original Poisson mean-variance relationship for X (row of contingency table) holds, which implies mean = variance.  When the  is > 1, the user is accounting for overdispersion in X (variance becomes proportional to the mean by quasipoisson_phiX value).  The value must strictly be >= 1.}

\item{quasipoisson_phiY}{Same description as quasipoisson_phiX but this argument is for Y (column variable in contingency table).}

\item{seed_estimation_method}{A character string indicating which method is used for updating the cross tabulations. The choices are: "ipfp", "ml", "chi2", or "lsq".}  

\item{lambda_range}{A numeric class value to represent a range on the X lambda and Y lambda.  The value is added (lambda.plus) and subtracted (lambda.minus) from the given (average) or estimated (maximum likelihood function) X lambda and Y lambda (lambda.original).  Set to 0 if generating one table (lambda.original) is the desired output.  Set value > 0 to calculate three separate uncensored contingency tables their corresponding X and Y marginal lambdas (lambda.minus, lambda.original, lambda.plus). }

}

\value{
The rec function produces an uncensored contingency table.  By an increment of one, the rows range from Xlowerbound:Xupperbound and the columns range from Ylowerbound:Yupperbound. If the lambda_range argument = 0 then one uncensored contingency table is provided in a data.frame format.  If the user sets the lambda_range argument > 0, then a list of three uncensored contingency tables is provided (lambda.minus, lambda.original, and lambda.plus).   
}

\description{
The rec function assumes that the provided data (e.g decoupled averages, decoupled univariate tables, a combination of an average and univariate table, or a censored contingency table) relies on a Poisson or Quasi-Poisson distribution to estimate an uncensored contingency table.  It is recommended for a user to read the Details section below and more information can be found in the vignettes.      
}


\details{


\strong{Overview:}\cr
The rec function handles four cases.

\itemize{

\item \emph{\strong{Case I.}} When provided an average for both X and Y, the averages represent lambda values.  These lambdas create right-shifted and right-truncated Poisson or Quasi-Poisson X and Y probability densities for uncensored vectors ranging from Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding normalized density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding normalized density values represents the new column marginal.  This is a decoupled case, and thus the seed (initial cross tabulation matrix to be updated) is a matrix of ones.  The mipfp R package then estimates cross tabulations with a selected seed estimation method, new uncensored marginals, and seed matrix.  The final result is an uncensored contingency table with rows ranging from Xlowerbound:Xupperbound and columns ranging from Ylowerbound:Yupperbound.  

\item \emph{\strong{Case II.}} When provided an univariate frequency table for both X and Y, lambda values are estimated with a customized maximum likelihood function.  The methods listed in Case I are then implemented.    

\item \emph{\strong{Case III.}} When provided a combination of an average and frequency table (X and Y could be either), the same methods stated in Case I and II are implemented. 

\item \emph{\strong{Case IV.}} When provided a censored X*Y contingency table, the row marginals create a univariate X frequency table while the column marginals create a univariate Y frequency table.  Both tables estimate a lambda value with a customized maximum likelihood function.  These lambdas then create right-shifted and right-truncated Poisson or Quasi-Poisson X and Y probability densities for uncensored vectors ranging from Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding normalized density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding normalized density values represents the new column marginal.  This is not a decoupled case, and thus the seed repeats the cross tabulations in the censored contingency table for the newly created and compatible uncensored cross tabulations.  The mipfp R package then estimates cross tabulations with a selected seed estimation method, new uncensored marginals, and seed matrix.  The final result is an uncensored contingency table with rows ranging from Xlowerbound:Xupperbound and columns ranging from Ylowerbound:Yupperbound.    

}

\strong{Table Format:}\cr
The table(s) for Case II and III has restrictions. The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with the frequencies in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G). 

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
  
  
The table for Case IV also has restrictions.  The censored symbols should follow the requirements listed above.  The table's class can be a data.frame or a matrix.  The column names should be the Y category values. Row names should never be placed in this table, the default should always be 1:n.  The first column should be the X category values. The inside of the table are X * Y cross tabulation, which are either nonnegative if seed_estimation_method is "ipfp" or strictly positive when method is "ml", "lsq" or "chi2".  The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The table below is a formatted example with percentages as the cross tabulations and the bottom right corner as a total sum.  

  \tabular{ccccc}{
    NA \tab <20 \tab 20-30 \tab >30\tab NA\cr
    <5 \tab 0.18 \tab 0.19 \tab 0.08\tab 0.45\cr
    5-9 \tab 0.13 \tab 0.08 \tab 0.12\tab 0.33\cr
    >=10 \tab 0.07 \tab 0.05 \tab 0.10\tab 0.21\cr
    NA \tab 0.38 \tab 0.32 \tab 0.31\tab 1.00\cr
  }



\strong{Bounds:}\cr
Ideally, the four bounds should be chosen based off prior knowledge and expert elicitation, but they can also be selected intuitively with a brute force method.  If rec outputs a final contingency table with higher probabilities near the edge(s) of the table, then it would make sense to increase the range of the bound(s).  For both the X and Y variables, this would just involve making the lower bound less, making the upper bound more, or doing a combination of the two.  The opposite holds true as well.  If the final contingency table has very low probabilities near the edge(s) of the table, then a user should decrease the range of the particular bound(s).  
\cr \cr
Recall that the lower bounds mathematically represent a right-shift while the upper bounds play a key role in creating right-truncated probability densities.  


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


\strong{Estimating Seed:} \cr
This function implements the \emph{mipfp} R package, which offers four methods to estimate cross tabulations when provided fixed marginals.  

\tabular{cc}{
  
\emph{\strong{Method}} \tab \emph{\strong{Abbreviation}} \cr

Iterative proportional fitting procedure \tab ipfp \cr

Maximum likelihood method\tab ml \cr

Minimum chi-squared\tab chi2 \cr 

Weighted least squares \tab lsq \cr
  }

For a summary and understanding of all methods please refer to the vignettes and/or the papers by Little et al. (1991) and Suesse et al. (2017).  


\strong{Lambda Range:} \cr
Recall that a provided average represents a lambda value while other cases estimate a lambda value from a univariate censored table.  With the lambda_range argument, the user has the option to put a range on the X and Y lambdas, which are used in creating the uncensored marginals.  This argument is added and subtracted to both lambdas and three separate sets of X and Y marginals are calculated.  The lambda.original table refers to a table that used the X and Y marginals calculated from the provided average(s) or estimated lambda(s).  This table is the exact same as the single table provided when setting the lambda_range = 0.  The lambda.minus table refers to a table that used the X and Y marginals calculated from the provided average(s) or estimated lambda(s) subtracted by the lambda_range input. The lambda.plus table refers to a table that used the X and Y marginals calculated from the sum of the provided average(s) or estimated lambda(s) and the lambda_range input.  
}


\examples{
  # provide two averages
  # assume Poisson mean-variance relationship (both quasipoisson_phi variables = 1)
  # output three table (lambda_range = .5)
  
  # different seed_estimation_method arguments take longer to run
  twoaverages.results<-rec(X= 4.4,
     Y = 571.3,
     Xlowerbound = 1,
     Xupperbound = 15,
     Ylowerbound = 540,
     Yupperbound = 590,
     quasipoisson_phiX = 1,
     quasipoisson_phiY =  1,
     seed_estimation_method= "ipfp",
     lambda_range =.5)
  
  
  # provide one average and one table
  # assume Poisson mean-variance relationship (both quasipoisson_phi variables = 1)
  # output three table (lambda_range = .5)
  
  # create a censored univariate table
  Y.table = cbind(as.character(c("<7", "7-12", "13-19", ">19")), 
    c(11800,57100,14800,3900))
  
  # different seed_estimation_method arguments take longer to run 
  combo.results<-rec(X= 2.3,
     Y = Y.table,
     Xlowerbound = 1,
     Xupperbound = 15,
     Ylowerbound = 1,
     Yupperbound = 30,
     quasipoisson_phiX = 1,
     quasipoisson_phiY =  1,
     seed_estimation_method= "ipfp",
     lambda_range =.5)
   
   
  # provide a censored contingency table 
  # assume overdispersion in X (quasipoisson_phiX != 1)
  # output one table

  # create a censored contingency table
  contingencytable<-matrix(c(6185,9797,16809,11126,6156,3637,908,147,69,4,
                         5408,12748,26506,21486,14018,9165,2658,567,196,78,
                         7403,20444,44370,36285,23576,15750,4715,994,364,136,
                         4793,17376,44065,40751,28900,20404,6557,1296,555,228,
                         2354,11143,32837,33910,26203,19301,6835,1438,618,245,
                         1060,6038,19256,21298,17774,13864,4656,1039,430,178,
                         273,2521,9110,11188,9626,7433,2608,578,196,112,
                         119,1130,4183,5566,5053,3938,1367,318,119,66,
                         33,388,1707,2367,2328,1972,719,171,68,37,
                         38,178,1047,1672,1740,1666,757,193,158,164),
                           nrow=10,ncol=10, byrow=TRUE)
  rowmarginal<-apply(contingencytable,1,sum)
  contingencytable<-cbind(contingencytable, rowmarginal)
  colmarginal<-apply(contingencytable,2,sum)
  contingencytable<-rbind(contingencytable, colmarginal)
  row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
  contingencytable<-data.frame(c("1","2","3","4","5","6", "7", "8","9","10+", NA),
    contingencytable)
  colnames(contingencytable)<-c(NA,"<20","20-29","30-39","40-49","50-69","70-99",
                                "100-149","150-199","200-299","300+", NA)

  # the contingencytable input could be put in X or Y (opposing argument = 0)
  # different seed_estimation_method arguments take longer to run
  # creating the seed in this case takes longer to run 
  contingencytable.results<-rec(X= contingencytable,
     Y = 0,
     Xlowerbound = 1,
     Xupperbound = 15,
     Ylowerbound = 10,
     Yupperbound = 310,
     quasipoisson_phiX = 3,
     quasipoisson_phiY =  1,
     seed_estimation_method= "ipfp",
     lambda_range =0)
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
\keyword{contingency}


\references{
Johan Barthelemy and Thomas Suesse (2018). mipfp: Multidimensional Iterative Proportional Fitting and Alternative Models. R package version 3.2. https://CRAN.R-project.org/package=mipfp 
  
Little, R. J., Wu, M. M. (1991) Models for contingency tables with known margins when target and sampled populations differ. \emph{Journal of the American Statistical Association}, 86(413): 87-95. doi: https://doi.org/10.2307/2289718

Suesse, T., Namazi-Rad, M., Mokhtarian, P., & Barthelemy, J. (2017). Estimating Cross-Classified Population Counts of Multidimensional Tables: An Application to Regional Australia to Obtain Pseudo-Census Counts, \emph{Journal of Official Statistics}, 33(4), 1021-1050. doi: https://doi.org/10.1515/jos-2017-0048
}
