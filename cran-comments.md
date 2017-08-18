## Resubmission
This is a resubmission. In this version I have:

* added single quote software names such as 'revengc' in the Description field

* added quickly running examples < 5 sec outside of \dontrun{} 

## Test environments
* local OS X install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Note: I have checked that the submission actually comes from the maintainer: 'Samantha Duchscherer <sam.duchscherer@gmail.com>'

## Comment on possibly mis-spelled words in DESCRIPTION

I use 'hhs' as an acronym for 'household size'.  'revengc' is my package name that I have now put in quotes.  'univariate' is not misspelled. 

## Comment on examples inside \dontrun{}

Added \dontrun{} on rec(indonesia_contingency,0,1,15,10,310) because this gives same results as rec(contingencytable,0,1,15,10,310).

