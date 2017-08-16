# revengc: Reverse Engineering Censored, Decoupled Residential Data for Population Density Estimation

A wealth of open source information is available that points to building usage including floor area of building size and likely occupancy. In the case of residential structures, census data provides a number of attributes including two values important to interior density estimation: household size (hhs) and area.  If a national census revealed the raw data or provided a full uncensored contingency table (hhs x area), computing interior density as people/area would be straightforward. However, agencies rarely report this contingency table. Rather hhs and area are often decoupled and reported as separate univariate frequency tables, average values, or a combination of the two.  In addition, the decoupled or contingency tables provided are typically left (<, <=), right (>, >=), and interval (-) censored.  This type of information becomes problematic in estimating interior residential occupancy for numerous reasons.  How can the people/area ratio be calculated when no affiliation between the variables exist?  If a census reports a hhs average of 5.3, then how many houses are there with 1 person, 2 people,..., 10 people?  If a census reports that there are 100 houses in an area of 26-50 square meters, then how many houses are in 26, 27,..., 50 square meters?  The challenge therefore is to infer the people/area ratio when given decoupled and summarized data. The statistical package `revengc` was designed to reverse engineer censored, decoupled census data into a likely hhs x area uncensored contingency table for estimating interior residential occupancy.


## Introduction 

The main function in `revengc` is called `rec()`.  This function can be used on the following scenarios found in any given census
 
 * Case I. Averages for both household size (hhs) and area

 * Case II. Decoupled hhs and area frequency tables

 * Case III. Combination of hhs (area) average and area (hhs) frequency table 

 * Case IV. hhs x area contingency table (censored)
 
In short, `rec()` inputs censored, decoupled census data and returns an uncensored contingency table with household size as the rows and area as the columns. The rows will range from the household size lower bound to the household size upper bound. The columns will range from the area lower bound to the area upper bound.  **More information behind the mathematics of `rec()` can be found in the `vignettes/` directory.**
 
## Getting Started 
 
You can install the latest development version from github with

```
devtools::install_github("GIST-ORNL/revengc")
library(revengc)
```

## Details about `rec()`

 ### Usage
`rec()` has the following format 
```
rec(hhsdata, areadata, hhslowerbound, hhsupperbound, arealowerbound, areaupperbound)
``` 

where a description of each argument is found below 

* **hhsdata**-This household size value can be a univariate frequency table or numeric value that represents an average. This input could also be a contingency table, but only if the areadata = 0.

* **areadata**-This area (size of house) value can be a univariate frequency table or numeric value that represents an average. This input could also be a contingency table, but only if the hhsdata = 0. The areadata can be any unit of measure.
	
* **hhslowerbound**-This is a numeric value to represent the household size lower bound. This lower bound variable needs to be numeric value >=0.
	
* **hhsupperbound**-This is a numeric value to represent the household size upper bound. This upper bound variable cannot be less than the highest category value (e.g. if a table has '>100' then the upper bound cannot be 90).
	
* **arealowerbound**-This is a numeric value to represent the area lower bound. This lower bound variable needs to be numeric values >=0.

* **areaupperbound**-This is a numeric value to represent the area upper bound. This upper bound variable cannot be less than the highest category value (e.g. if a table has '>100' then the upper bound cannot be 90).


#### Bounds

Ideally, the four bounds should be chosen based off prior knowledge and expert elicitation, but they can also be selected intuitively with a brute force method.  If the reverse engineering tool outputs a final contingency table with higher probabilities near the edge(s) of the table, then it would make sense to increase the range of the bound(s).  For both the hhs and area variables, this would just involve making the lower bound less, making the upper bound more, or doing a combination of the two.  The opposite holds true as well.  If the final contingency table has very low probabilities near the edge(s) of the table, then a person should decrease the range of the particular bound(s).


#### Tables

The table(s) for Case II and III has restrictions. The frequency table must be formatted where there are 2 columns with n number of rows.  The categories must be in the first column and the frequencies in the second column.  Row names should never be placed in this table, the default name should always be 1:n where n is number of rows in the table.  Both columns should not have a header (header=FALSE).  No words are allowed for censoring.  The only censoring symbols accepted are < and <= (left censoring), - (interval censoring), > and >= and + (right censoring).  A formatted example is below.


<=6  | 11800
---- | ----
7-12 | 57100
13-19| 14800
 20+ |3900


The table for Case IV also has restrictions.  Again, no words are allowed for censoring. Only the censored values of <, <=, -, >, >=, and +  are permitted.  This table works when there is a column header present or absent.  However, the only column header that is allowed has to be the hhs or area category values.  Row names should never be placed in this table, the default name should always be 1:n where n is number of rows in the table.  The inside of this table is the cross tabulation of hhs x area which are either positive frequency values or percentages. The row and column total marginals have to be in this table. The top left, top right, and bottom left corners of this table have to be NA or blank, but the bottom right corner can be a total sum value, NA, or blank. This code will transpose a contingency table if given a table with area=rows and hhs=columns, but the output will always be hhs=rows and area=columns. This transpose will only occur under the assumption that the sum of area category value is greater than the sum of household size category value.  Below is a formatted example with percentages as the cross-tabulations, the bottom right corner as a total sum, and the column header as the area category values.

  NA | <20 | 20-30 | >30 | NA 
 -----|------|-------|------|-----
  <5 | 0.18 |0.19 | 0.09 | 0.46
  5-9 | 0.13 |0.08 | 0.12 | 0.33
  10+ | 0.06 |0.05 | 0.10 | 0.21
  NA | 0.37 |0.32 | 0.31 | 1.00 


## Sample datasets
Since the format for the tables is strict, we will now show how to format these tables properly using actual census data.  If a user wants to read in a file, the format must look like the following sample datasets: `nepal_hhs`, `hongkong_hhs`, `hongkong_area`, `iran_hhs`, and `indonesia_contingency `.  These datasets are cited in the Legal section below and more details can be found in `man/` directory.  Creating tables with R code is possible too.  The following code shows how these sample datasets can be created in R.

```
hhsdata_nepal<-cbind(as.character(c("1-2", "3-4", "5-6", "7-8", ">=9")), c(16.2, 41.7, 29.0, 9.0, 4.1))

hhsdata_hongkong<-cbind(as.character(c("1", "2", "3", ">3")), c(27600,25600,20900,13500))

areadata_hongkong<-cbind(as.character(c("<7", "7-12", "13-19", ">19")), c(11800,57100,14800,3900))

hhsdata_iran<-cbind(as.character(c("1", "2", "3", "4", ">=5")), c(7.08,18.29,29.64,27.95,17.04)) 

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
  contingencytable<-data.frame(c("1","2","3","4","5","6", "7", "8","9","10+", NA), contingencytable)
  colnames(contingencytable)<-c(NA,"<20","20-29","30-39","40-49","50-69","70-99",
                                "100-149","150-199","200-299","300+", NA)
```

## Examples of Applying `rec()` to Census Data

### Nepal
The Nepal Living Standards Survey [2] provides averages and censored tables for household size and also averages for area of dwelling.  This census data provides an example for Case I and Case III.  To produce a final hhs x area contingency table (rows ranging from 1 to 20 people and columns ranging from 520 to 620 square feet) for urban Nepal you would run 

```
#Case I
rec(4.4,571.3,1,20,520,620)
#Case II
rec(nepal_hhs,571.3,1,20,520,620)

```

### Hong Kong
The Census and Statistics Department of Hong Kong [1] provides censored frequency tables for hhs and area as well as medians for both variables.  This census data provides an example for Case I, Case II, and Case III.  To produce a final hhs x area contingency table (rows ranging from 1 to 15 people and columns ranging from 1 to 30 square meters) for sub-divided units in Hong Kong you would run 

```
#Case I
rec(2.0,10.3,1,15,1,30)
#Case II
rec(hongkong_hhs,hongkong_area,1,15,1,30)
#Case III
rec(2.0,hongkong_area,1,15,1,30)
rec(hongkong_hhs,10.3,1,15,1,30)
```

### Iran
For different provinces, The Statistical Centre of Iran [4] reports averages and censored tables for household size as well as averages for floor area.  This census data provides an example for Case I and Case III.  To produce a final hhs x area contingency table (rows ranging from 1 to 10 people and columns ranging from 80 to 130 square meters) for East Azerbayejan (Azerbaijan), Iran you would run 

```
#Case I
rec(3.4,100.5,1,10,80,130)
#Case III
rec(iran_hhs,100.5,1,10,80,130)
```

### Indonesia
The 2010 Population Census Data - Statistics Indonesia [3] provides over 60 censored contingency tables of Floor Area of Dwelling Unit (m2) x Household Member Size separated by province, urban, and rural.  This census data provides a Case IV example.   To produce a final hhs x area contingency table (rows ranging from 1 to 15 people and columns ranging from 10 to 310 square meters) for Indonesia's Rural Aceh Province you would run 

```
#Case IV
rec(indonesia_contingency,0,1,15,10,310)
rec(0,indonesia_contingency,1,15,10,310)
```

## Legal

[1] Census and Statistics Department of Hong Kong Special Administrative Region . (2016). *Thematic Household Survey Report - Report No. 60 - Housing conditions of sub-divided units in Hong Kong*.  Retrieved from: <http://www.censtatd.gov.hk/hkstat/sub/sp100.jsp?productCode=C0000091>

[2] National Planning Commissions Secretariat, Government of Nepal. (2011). *Nepal Living Standards Survey*. Retrieved from: <http://siteresources.worldbank.org/INTLSMS/Resources/3358986-1181743055198/3877319-1329489437402/Statistical_Report_Vol1.pdf>

[3] Population Census Data - Statistics Indonesia. (2010). *Household by Floor Area of Dwelling Unit and Households Member Size*. Retrieved from: <http://sp2010.bps.go.id/index.php/site/tabel?wid=1100000000&tid=334&fi1=586&fi2=>

[4] The Statistical Centre of Iran. (2011). *Selected Findings of National Population and Housing Census*.  Retrieved from: <https://www.amar.org.ir/Portals/1/Iran/90.pdf>
