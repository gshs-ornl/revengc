# revengc: An R package to reverse engineer decoupled and censored data

An issue occurs when authors may have privy too but normally do not reveal clear information.  Decoupled variables (e.g. separate averages) and numeric censoring (e.g. between ages 10-15) are reoccurring instances found in areas ranging from demographic and epidemiological data to ecological inference problems.  Decoupled variables provide no availability for cross tabulations while censoring obscures the true underlying values.  The revengc R package was developed to reverse engineer this unclear information that is continually reported by many well-established organizations (e.g. World Health Organization (WHO), Centers for Disease Control and Prevention (CDC), World Bank, and various national censuses).  There are two main functions in revengc and both fit data to a Poisson or Quasi-Poisson distribution.  The estimated_lambda function takes a univariate censored frequency table and approximates its lambda (average) value.  The rec function estimates an uncensored bivariate table from decoupled and summarized arguments.

## Overview
The workflow for estimated_lambda is straightfoward.  A censored frequency table is fit to a Poisson or Quasi-Poisson truncated distribution using a likelihood function customized to censored data and estimated_lambda outputs the lambda value that maximizes the function. However, rec is more complex.  rec handles four cases and a summary workflow for each is provided below.  It is highly recommended for a user to read the vignettes for more information.     

* Case I. When provided an average for both X and Y, the averages represent lambda values.  These lambdas create right-shifted and right-truncated Poisson or Quasi-Poisson X and Y probability densities for uncensored vectors ranging from Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding normalized density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding normalized density values represents the new column marginal.  This is a decoupled case, and thus the seed (initial cross tabulation matrix to be updated) is a matrix of ones.  The mipfp R package then estimates cross tabulations with a selected seed estimation method, new uncensored marginals, and seed matrix.  The final result is an uncensored contingency table with rows ranging from Xlowerbound:Xupperbound and columns ranging from Ylowerbound:Yupperbound.  

* Case II. When provided an univariate frequency table for both X and Y, lambda values are estimated with a customized maximum likelihood function.  The methods listed in Case I are then implemented.    

* Case III. When provided a combination of an average and frequency table (X and Y could be either), the same methods stated in Case I and II are implemented. 

* Case IV. When provided a censored X*Y contingency table, the row marginals create a univariate X frequency table while the column marginals create a univariate Y frequency table.  Both tables estimate a lambda value with a customized maximum likelihood function.  These lambdas then create right-shifted and right-truncated Poisson or Quasi-Poisson X and Y probability densities for uncensored vectors ranging from Xlowerbound:Xupperbound and Ylowerbound:Yupperbound, respectively.  The Xlowerbound:Xupperbound vector with its corresponding normalized density values represents the new row marginal.  The Ylowerbound:Yupperbound vector with its corresponding normalized density values represents the new column marginal.  This is not a decoupled case, and thus the seed repeats the cross tabulations in the censored contingency table for the newly created and compatible uncensored cross tabulations.  The mipfp R package then estimates cross tabulations with a selected seed estimation method, the uncensored marginals, and seed matrix.  The final result is an uncensored contingency table with rows ranging from Xlowerbound:Xupperbound and columns ranging from Ylowerbound:Yupperbound.               


## Getting Started 
 
You can install the latest development version from github with

```
devtools::install_github("GIST-ORNL/revengc")
library(revengc)
```

or the the latest release version from CRAN with
```
install.packages("revengc")
library(revengc)
```

## Usage
`estimated_lambda()` has the following format 
```
estimated_lambda(censoredtable, upperbound, quasipoisson_phi)
``` 

where a description of each argument is found below 

* **censoredtable** - A frequency table (censored and/or uncensored).  A data.frame and matrix are acceptable table classes.

* **upperbound** - A numeric class value to represent a right-truncated point.  The value cannot be less than the highest category value (e.g. the upper bound cannot be 90 if a table has a '>100' category).
	
* **quasipoisson** - A numeric class value to help with overdispersion found in the censored table.  If the value = 1, the original Poisson mean-variance relationship holds (mean = variance).  When the value is >, the user is accounting for overdispersion (variance becomes proportional to the mean by quasipoisson_phi value).  The value must strictly be >=1.


`rec()` has the following format 
```
 rec(X, Y, Xlowerbound, Xupperbound, Ylowerbound,Yupperbound, 
     quasipoisson_phiX, quasipoisson_phiY, seed_estimation_method, lambda_range)
``` 

where a description of each argument is found below 

* **X** - Argument can be an average, a univariate frequency table, or a censored contingency table.  The average value should be a numeric class while a data.frame or matrix are acceptable table classes.  If a user inputs a censored contingency table, make Y = 0.

* **Y** - Same description as X but this argument is for the Y variable.  Make X = 0 if Y argument is a censored contingency table.

* **Xlowerbound** - A numeric class value to represent a right-shift variable for X.  The value must strictly be >= 0 and cannot be greater than the lowest category/average value provided for X (e.g. the lower bound cannot be 6 if a table has '>= 5' as a X or row category). 

* **Xupperbound** - A numeric class value to represent a right-truncated point for X.  The value cannot be less than the highest category/average value provided for X (e.g. the upper bound cannot be 90 if a table has '> 100' as a X or row category).

* **Ylowerbound** - Same description as Xlowerbound but this argument is for Y (column variable in contingency table). 

* **Yupperbound** - Same description as Xupperbound but this argument is for Y (column variable in contingency table).  

* **quasipoisson_phiX** - A numeric class value to help with overdispersion found in X.  If the value = 1, the original Poisson mean-variance relationship for X (row of contingency table) holds, which implies mean = variance.  When the  is > 1, the user is accounting for overdispersion in X (variance becomes proportional to the mean by quasipoisson_phiX value).  The value must strictly be >= 1.

* **quasipoisson_phiY** - Same description as quasipoisson_phiX but this argument is for Y (column variable in contingency table).

* **seed_estimation_method** - A character string indicating which method is used for updating the cross tabulations. The choices are: "ipfp", "ml", "chi2", or "lsq".  

* **lambda_range** - A numeric class value to represent a range on the X lambda and Y lambda.  The value is added (lambda.plus) and subtracted (lambda.minus) from the given (average) or estimated (maximum likelihood function) X lambda and Y lambda (lambda.original).  Set to 0 if generating one table (lambda.original) is the desired output.  Set value > 0 to calculate three separate uncensored contingency tables with their corresponding X and Y marginal lambdas (lambda.minus, lambda.original, lambda.plus). 


## Details

### Tables
The univariate frequency table, which can be a data.frame or matrix class, must have two columns and n number of rows.  The categories must be in the first column with the frequencies in the second column.  Row names should never be placed in this table (the default row names should always be 1:n).  Column names can be any character string.  The only symbols accepted for censored data are listed below.  Note, less than or equal to (<= and LE) is not equivalent to less than (< and L) and greater than or equal to (>=, +, and GE) is not equivalent to greater than (> and G). 


* left censoring: <, L, <=, LE
* interval censoring: - or I (symbol has to be placed in the middle of the two category values)
* right censoring: >, >=, +, G, GE
* uncensored: no symbol (only provide category value)

The formatted example below is made with the following code.

```
univariatetable<-cbind(as.character(c("<=6", "7-12", "13-19", "20+")), c(11800,57100,14800,3900))
```

<=6  | 11800
---- | ----
7-12 | 57100
13-19| 14800
 20+ |3900


The contingency table has restrictions.  The censored symbols should follow the requirements listed above.  The table's class can be a data.frame or a matrix.  The column names should be the Y category values. Row names should never be placed in this table, the default should always be 1:n.  The first column should be the X category values. The inside of the table are X * Y cross tabulation, which are either nonnegative if seed_estimation_method is "ipfp" or strictly positive when method is "ml", "lsq" or "chi2".  The row and column marginal totals corresponding to their X and Y category values need to be placed in this table. The top left, top right, and bottom left corners of the table should be NA or blank.  The bottom right corner can be a total cross tabulation sum value, NA, or blank. The formatted example below is made with the following code.

```
contingencytable<-matrix(c(.18, .13, .07, .19, .08, .05, .08, .12, .10), nrow = 3, ncol = 3)
  rowmarginal<-apply(contingencytable,1,sum)
  contingencytable<-cbind(contingencytable, rowmarginal)
  colmarginal<-apply(contingencytable,2,sum)
  contingencytable<-rbind(contingencytable, colmarginal)
  row.names(contingencytable)[row.names(contingencytable)=="colmarginal"]<-""
  contingencytable<-data.frame(c("<5", "5I9", "G9", NA), contingencytable)
  colnames(contingencytable)<-c(NA,"<=19","20-30",">=31", NA)
```

  NA | <=19 | 20-30 | >=31 | NA 
 -----|------|-------|------|-----
  <5 | 0.18 |0.19 | 0.08 | 0.45
  5I9 | 0.13 |0.08 | 0.12 | 0.33
  G9 | 0.07 | 0.05 | 0.10 | 0.22
  NA | 0.38 | 0.32 | 0.30 | 1.00 

### quasipoisson_phi
In a Poisson distribution, the variance equals the mean.  To combat overdispersion, this function considers the option where the variance is proportional to the mean by a scalar value of phi.  This changes Poisson to Quasi-Poisson.  

* If phi = 1, the variance equals the mean and the original Poisson mean-variance relationship holds.
* Overdispersion in data refers to when the variance is larger than the mean.  To accommodate this issue, set phi > 1. 
* Underdispersion in data refers to when the variance is smaller than the mean.  However, underdispersion is a rare case and is not accommodated for in this package.

### Bounds
The upper bound in estimating_lambda represents a right-truncated point for the table.  Values that exceed the upper bound are assumed to have very small probabilities worth refining to zero, and thus these values are omitted in calculating the fitted Poisson (phi = 1) or Quasi-Poisson (phi > 1) truncated distribution.  Typically, setting the upper bound value between +10 and +20 of the last categorical value is sufficient.  

Ideally, the four bounds for rec should be chosen based off prior knowledge and expert elicitation, but they can also be selected intuitively with a brute force method.  If rec outputs a final contingency table with higher probabilities near the edge(s) of the table, then it would make sense to increase the range of the bound(s).  For both the X and Y variables, this would just involve making the lower bound less, making the upper bound more, or doing a combination of the two.  The opposite holds true as well.  If the final contingency table has very low probabilities near the edge(s) of the table, then a user should decrease the range of the particular bound(s).

## Examples of Applying functions to Census Data

### Nepal
A Nepal Living Standards Survey [1] provides a censored table and average for urban household size. Using the censored table, the estimated_lambda function calculates a close approximation to the provided average household size (4.4 people).  Below we set the upper bound to 15 (a reasonable guess from the provided table) and assume the household size table follows the original Poisson mean-variance relationship (quasipoisson_phi = 1). 

```
# revengc has the Nepal houshold table preloaded as univariatetable.csv   
# result = 4.37
estimated_lambda(censoredtable = univariatetable.csv, 
  upperbound = 15, quasipoisson_phi = 1)
```

### Indonesia
In 2010, the Population Census Data - Statistics Indonesia provided over 60 censored contingency tables containing Floor Area of Dwelling Unit (square meter) by Household Member Size. The tables are separated by province, urban, and rural.  Here we use the household size by area contingency table for Indonesia's rural Aceh Province to show the multiple coding steps and functions implemented inside rec.  This allows the user to see a methodology workflow in code form.  The final uncensored household size by area contingency table, which was estimated with the "ipfp" method, has rows ranging from 1 (Xlowerbound) to 15 (Xupperbound) people and columns ranging from 10 (Ylowerbound) to 310 (Yupperbound) square meters.  For this example, we assume the original Poisson mean-variance relationship (both quasipoisson_phi arguments = 1) and only estimate one uncensored contingency table (lambda_range = 0).   



```
# data = Indonesia 's rural Aceh Province censored contingency table
# preloaded as 'contingencytable.csv'
contingencytable.csv 

# provided upper and lower bound values for table
# X=row and Y=column
Xlowerbound=1
Xupperbound= 15
Ylowerbound=10
Yupperbound=310

# table of row marginals provides lambda x 
row.marginal.table<-row_marginal(contingencytable.csv)
lambdax<-estimated_lambda(row.marginal.table, upperbound=Xupperbound, quasipoisson_phi=1)
# table of column marginals provides lambda y 
column.marginal.table<-column_marginal(contingencytable.csv)
lambday<-estimated_lambda(column.marginal.table, upperbound=Yupperbound, quasipoisson_phi=1)

# incorporate right shift from lower bounds
rowrange<-0:(Xupperbound-Xlowerbound)
rowrange<-rowrange+ Xlowerbound
colrange<-0:(Yupperbound-Ylowerbound)
colrange<-colrange+Ylowerbound

# new uncensored row marginal table
new.row.marginals<-dqpois_trunc(rowrange,lambda=lambdax, quasipoisson_phi=1, upperbound=15)
# row marginal == column marginal == 1 (marginal sums have to be equal)
new.row.marginals<-new.row.marginals/sum(new.row.marginals)
# new uncensored column margin table
new.column.marginals<-dqpois_trunc(colrange,lambda=lambday, quasipoisson_phi=1, upperbound=310)
# column marginal == row marginal == 1 (marginal sums have to be equal)
new.column.marginals<-new.column.marginals/sum(new.column.marginals)

# create uncensored seed from censored table
seed.output<-seedmatrix(contingencytable.csv , Xlowerbound=1, 
  Xupperbound=15, Ylowerbound=10, Yupperbound=310)

# run mipfp
# store the new margins in a list
tgt.data<-list(new.row.marginals, new.column.marginals)
# list of dimensions of each marginal constrain
tgt.list<-list(1,2)
# calling the estimated function 
## seed has to be in array format for mipfp package
## ipfp is the selected seed_estimation_method
final1<-Estimate(array(seed.output,dim=c(length(Xlowerbound:Xupperbound), 
  length(Ylowerbound:Yupperbound))), tgt.list, tgt.data, method="ipfp")$x.hat

# rec function outputs the same table
final2<-rec(X= contingencytable.csv,
  Y = 0,
  Xlowerbound = 1,
  Xupperbound = 15,
  Ylowerbound = 10,
  Yupperbound = 310,
  quasipoisson_phiX = 1,
  quasipoisson_phiY =  1,
  seed_estimation_method= "ipfp",
  lambda_range =0)

# check that both data.frame results have same values
all(final1 == final2)
```


## Legal
[1] National Planning Commissions Secretariat, Government of Nepal. (2011). *Nepal Living Standards Survey*. Retrieved from: <http://siteresources.worldbank.org/INTLSMS/Resources/3358986-1181743055198/3877319-1329489437402/Statistical_Report_Vol1.pdf>

[2] Population Census Data - Statistics Indonesia. (2010). *Household by Floor Area of Dwelling Unit and Households Member Size*. Retrieved from: <http://sp2010.bps.go.id/index.php/site/tabel?wid=1100000000&tid=334&fi1=586&fi2=>

