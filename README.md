# revengc: Reverse Engineering Censored, Decoupled Residential Data for Population Density Estimation

A wealth of open source information is available that points to building usage including floor area of building size and likely occupancy. In the case of residential structures, census data provides a number of attributes including two values important to interior density estimation: household size (hhs) and area.  If a national census revealed the raw data or provided a full uncensored contingency table (hhs x area), computing interior density as people/area would be straightforward. However, agencies rarely report this contingency table. Rather hhs and area are often decoupled and reported as separate univariate frequency tables, average values, or a combination of the two.  In addition, the decoupled or contingency tables provided are typically left (<), right (>), and interval (-) censored.  This type of information becomes problematic in estimating interior residential occupancy for numerous reasons.  How can the people/area ratio be calculated when no affiliation between the variables exist?  If a census reports a hhs average of 5.3, then how many houses are there with 1 person, 2 people,..., 10 people?  If a census reports that there are 100 houses in an area of 26-50 square meters, then how many houses are in 26, 27,..., 50 square meters?  The challenge therefore is to infer the people/area ratio when given decoupled and summarized data. The statistical package revengc was designed to reverse engineer censored, decoupled census data into a likely hhs x area uncensored contingency table for estimating interior residential occupancy.


## Getting Started

You can install the latest development version from github with


```
devtools::install_github("GIST-ORNL/revengc")
```

## Details 


The main function in the revengc package is called 'rec'.  The 'rec' function will handle 4 different types of input:

 -Case 1. hhs average, area average, hhs lower bound, hhs upper bound, area lower bound, and area upper bound

 -Case 2. hhs frequency table, area frequency table, hhs lower bound, hhs upper bound, area lower bound, and area upper bound

 -Case 3. hhs average or frequency table, area average or frequency table, hhs lower bound, hhs upper bound, area lower bound and area upper bound

 -Case 4. contingency table (hhs, area) or (area, hhs), hhs lower bound, hhs upper bound, area lower bound, and area upper bound


### Bounds

Ideally, the four bounds should be chosen based off prior knowledge and expert elicitation, but they can also be selected intuitively with a brute force method.  If the reverse engineering tool outputs a final contingency table with higher probabilities near the edge(s) of the table, then it would make sense to increase the range of the bound(s).  For both the hhs and area variables, this would just involve making the lower bound less, making the upper bound more, or doing a combination of the two.  The opposite holds true as well.  If the final contingency table has very low probabilities near the edge(s) of the table, then a person should decrease the range of the particular bound(s).

#### Tables

The table(s) for Case 2-3 has restrictions. The frequency table must be formatted where there are 2 columns with n number of rows.  The categories must be in first column and the frequencies in the second column.  Row names should never be placed in this table, the default name should always be 1:n where n is number of rows in the table.  Both columns should not have a header (header=FALSE).  No words are allowed for censoring.  The only censoring symbols accepted are < and <= (left censoring), - (interval censoring), > and >= and + (right censoring).  A formatted example is below.

\tabular{cc}{
    <=6\tab 11800 \cr
    7-12\tab 57100 \cr
    13-19\tab 14800 \cr
    20+\tab 3900 \cr
  }

The table for Case 4 also has restrictions.  Again, no words are allowed for censoring. Only the censored values of <, <=, -, >, >=, and +  are allowed.  This table works when there is a column header present or absent.  However, the only column header that is allowed has to be the hhs or area category values.  Row names should never be placed in this table, the default name should always be 1:n where n is number of rows in the table.  The inside of this table is the cross tabulation of hhsxarea which are either positive frequency values or percentages. The row and column total marginals have to placed in this table. The top left, top right, and bottom left corners of this table have to be NA or blank, but the bottom right corner can be a total sum value, NA, or blank. This code will transpose a contingency table if given a table with area=rows and hhs=columns, but the output will always be hhs=rows and area=columns. This transpose will only occur under the assuption that the sum of area category value is greater than the sum of household size category value.  Below is a formatted example with percentages as the cross-tabulations, the bottom right corner as a total sum, and the column header as the area category values.

  \tabular{ccccc}{
    NA \tab <20 \tab 20-30 \tab >30\tab NA\cr
    <5 \tab 0.18 \tab 0.19 \tab 0.08\tab 0.45\cr
    5-9 \tab 0.13 \tab 0.08 \tab 0.12\tab 0.33\cr
    >=10 \tab 0.06 \tab 0.05 \tab 0.10\tab 0.21\cr
    NA \tab 0.38 \tab 0.32 \tab 0.31\tab 1.00\cr
  }

}





## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone who's code was used
* Inspiration
* etc
