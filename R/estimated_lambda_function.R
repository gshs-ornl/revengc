library(stringr)
source("R/format_data_for_functions.R")

# estimated_lambda is a function that outputs lambda from univariate censored table

estimated_lambda<-function (censoredtable, upperbound, quasipoisson_phi){
  
  ## functions that help format data first
  #The findtypeofcensoring_univariatetable Function list the type of censoring in a provided censored frequency table
  #Type_of_Censoring (L1 (<), L2 (<=), I (-), G1 (>), G2 (>= or +), or U (no censoring)), and Censor_Number (category value corresponding to censoring value)
  
  findtypeofcensoring_univariatetable<-function (contingencytable){
    
    #making a dataframe from the category case (column one)
    cases<-data.frame(contingencytable)
    cases<-cases[1]
    #relabeled column with censoring as Censoring_Symbol
    names(cases) <- c("Censoring_Symbol")
    
    # changed lowercase up uppercase if users input "l, le, i, g, ge"
    cases$Censoring_Symbol<-toupper(as.matrix(cases$Censoring_Symbol))
    
    #will list censoring type in column called Type_of_Censoring
    cases$Type_of_Censoring <- ""
    
    #find what numbers go with censoring and put it in censor_number column
    cases$Censor_Number<-str_replace_all(cases$Censoring_Symbol, "[-,<,<=,>,>=,+,\\,]", " ")
    cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[L, LE, I, G, GE]", " ")
    cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[l, le, i, g, ge]", " ")
    
    # remove numbers from censoring_symbol column
    cases$Censoring_Symbol<-gsub('[0-9]+', '', cases$Censoring_Symbol)
    
    #named the symbols allowed in model
    L1 <- c("<")
    L1a <- c("L")
    L2 <- c("<=")
    L2a <- c("LE")
    I <- c("-")
    Ia <- c("I")
    G1 <- c(">")
    G1a <- c("G")
    G2 <- c(">=")
    G2a<-c("[+]")
    G2b<-c("GE")
    
    # remove any spaces found in symbol column 
    cases$Censoring_Symbol<-gsub('\\s+', '', cases$Censoring_Symbol)
    
    #look to see if these symbols are in the provided table
    leftcensorcase1<-grep("^<$", cases$Censoring_Symbol)
    leftcensorcase1a<-grep("^L$", cases$Censoring_Symbol)
    
    leftcensorcase2<-grep("^<=$", cases$Censoring_Symbol)
    leftcensorcase2a<-grep("^LE$", cases$Censoring_Symbol)
    
    intervalcensor<-grep("^[-]$", cases$Censoring_Symbol)
    intervalcensora<-grep("^I$", cases$Censoring_Symbol)
    
    rightcensorcase1<-grep("^>$", cases$Censoring_Symbol)
    rightcensorcase1a<-grep("^G$", cases$Censoring_Symbol)
    
    rightcensorcase2<-grep("^>=$", cases$Censoring_Symbol)
    rightcensorcase2a<-grep("^[+]$", cases$Censoring_Symbol)
    rightcensorcase2b<-grep("^GE$", cases$Censoring_Symbol)
    
    
    #if the cases were there then this puts an L1, L2, I, G1, G2, or U in column called Type_of_Censoring
    if (length(leftcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, L1)),"Type_of_Censoring"] <- "L1"}
    if (length(leftcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, L1a)),"Type_of_Censoring"] <- "L1a"}
    
    if (length(leftcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, L2)),"Type_of_Censoring"] <- "L2"}
    if (length(leftcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, L2a)),"Type_of_Censoring"] <- "L2a"}
    
    if (length(intervalcensor)>0){cases[which (str_detect(cases$Censoring_Symbol, I)),"Type_of_Censoring"] <- "I"}
    if (length(intervalcensora)>0){cases[which (str_detect(cases$Censoring_Symbol, Ia)),"Type_of_Censoring"] <- "Ia"}
    
    if (length(rightcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, G1)),"Type_of_Censoring"] <- "G1"}
    if (length(rightcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, G1a)),"Type_of_Censoring"] <- "G1a"}
    
    if (length(rightcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, G2)),"Type_of_Censoring"] <- "G2"}
    if (length(rightcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, G2a)),"Type_of_Censoring"] <- "G2a"}
    if (length(rightcensorcase2b)>0){cases[which (str_detect(cases$Censoring_Symbol, G2b)),"Type_of_Censoring"] <- "G2b"}
    
    
    #make all the different labels of censoring consistent
    cases$Type_of_Censoring[cases$Type_of_Censoring =="L1a"]<-c("L1")
    cases$Type_of_Censoring[cases$Type_of_Censoring =="L2a"]<-c("L2")
    cases$Type_of_Censoring[cases$Type_of_Censoring =="Ia"]<-c("I")
    cases$Type_of_Censoring[cases$Type_of_Censoring =="G1a"]<-c("G1")
    cases$Type_of_Censoring[cases$Type_of_Censoring =="G2a"]<-c("G2")
    cases$Type_of_Censoring[cases$Type_of_Censoring =="G2b"]<-c("G2")
    
    #listing the no censoring cases as "U"
    cases$Type_of_Censoring[cases$Type_of_Censoring==""]<-"U"
    
    #put in errors to let users know that there can't be duplicates of Greater than or Less than 
    if(length(grep("G1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than (> or G) category')
    if(length(grep("G2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than or equal to (>= or GE) category')
    if(length(grep("L2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than or equal to (<= or LE) category')
    if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
    if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
    if(length(grep("L1", cases$Type_of_Censoring))==1 && length(grep("L2", cases$Type_of_Censoring))==1) stop ('Censored table can not  have both a less than (< or L) category and a less than or equal to category (<= or LE)')
    if(length(grep("G1", cases$Type_of_Censoring))==1 && length(grep("G2", cases$Type_of_Censoring))==1) stop ('Censored table can not have both a greater than (< or G) category and a greater than or equal to category (<= or GE)')
    
    
    
    return (cases)} #end findtypeofcensoring_univariatetable function
  
  #####
  #####
  
  #The fixdata_univariatecase function regonizes the symbols and divides a frequency table up into the 6 censored cases
  #This is a formatting data function 
  #This uses the findtypeofcensoring_univariatetable function
  #Requirement of lower and upper bounds
  #The lower bound provided will be used in calculating a right shift and upper bound for truncation
  
  
  fixdata_univariatecase<-function(contingencytable, upperbound, quasipoisson_phi) {
    
    # conduct findtypeofcensoring_univariatetable function on data 
    cases<-findtypeofcensoring_univariatetable(data.frame(contingencytable))
    
    # stop if upperbound is smaller than largest number in table
    largestnumintable<-max(as.numeric(unlist(regmatches(cases$Censor_Number, gregexpr("[[:digit:]]+", cases$Censor_Number)))))
    if(largestnumintable>upperbound) stop ('Upperbound needs to be larger than or equal to its largest corresponding category number')
    
    
    #gives a row number for each of the 6 censoring types
    lo1<-as.numeric(which(cases$Type_of_Censoring=='L1'))
    lo2<-as.numeric(which(cases$Type_of_Censoring=='L2'))
    inte<-as.numeric(which(cases$Type_of_Censoring=='I'))
    gre1<-as.numeric(which(cases$Type_of_Censoring=='G1'))
    gre2<-as.numeric(which(cases$Type_of_Censoring=='G2'))
    ex<-as.numeric(which(cases$Type_of_Censoring=='U'))
    
    #finding what number corresponds to the freqency value based off type of censoring
    #put as.numeric and as.character to help with format
    #R was changing decimals to whole numbers without putting as.numeric and as.character
    countl1<-as.numeric(as.character(contingencytable[lo1,2]))
    countl2<-as.numeric(as.character(contingencytable[lo2,2]))
    counti<-as.numeric(as.character(contingencytable[inte,2]))
    countg1<-as.numeric(as.character(contingencytable[gre1,2]))
    countg2<-as.numeric(as.character(contingencytable[gre2,2]))
    counte<-as.numeric(as.character(contingencytable[ex,2]))
    
    #finding what number (in the Censor_Number) corresponds to the censor type
    #values listed in cases$Censor_Number[] are factors
    #intervalnumber will be as.vector because will have to run strsplit function on this value later
    lowernumber1<-as.numeric(as.character(cases$Censor_Number[lo1]))
    lowernumber2<-as.numeric(as.character(cases$Censor_Number[lo2]))
    intervalnumber<-as.vector(cases$Censor_Number[inte])
    greaternumber1<-as.numeric(as.character(cases$Censor_Number[gre1]))
    greaternumber2<-as.numeric(as.character(cases$Censor_Number[gre2]))
    exactnumber<-as.numeric(as.character(cases$Censor_Number[ex]))
    
    #combining the category number (without its symbol) and the freqency number that corresponds to that category number
    lower1<-t(c(lowernumber1,countl1))
    lower2<-t(c(lowernumber2,countl2))
    greater1<-t(c(greaternumber1,countg1))
    greater2<-t(c(greaternumber2,countg2))
    #unlike the left and right censor categories there could be multiple of the no censored category
    exact<-unname(rbind(exactnumber,counte))
    
    #spliting the interval(s)
    if (length(intervalnumber)>0){
      spl<-na.omit(as.numeric(unlist(strsplit(intervalnumber,' ', fixed=FALSE))))
      interval<-matrix(spl,length(intervalnumber),2,byrow=TRUE)
      interval<-unname(cbind(interval,counti))
    } else {interval=NULL}
    
    
    #return values for later use
    final<-list(leftcensored1=lower1,leftcensored2=lower2, nocensored=exact,
                rightcensored1=greater1,
                rightcensored2=greater2, intervalcensored=interval,
                upperbound=upperbound,
                quasipoisson_phi = quasipoisson_phi)
    
    # report error.. 
    if(length(final$leftcensored2)!=0) {leftcensored2value<-final$leftcensored2[1,1]} else {leftcensored2value=-1}
    if(length(final$intervalcensored)!=0) {intervalvalues<-final$intervalcensored[,c(1,2)]} else {intervalvalues=-2}
    if(length(final$rightcensored2)!=0) {rightcensored2value<-final$rightcensored2[1,1]} else {rightcensored2value=-3}
    if(length(final$nocensored)!=0) {nocensored<-final$nocensored[1,]} else {nocensored = -4}
    
    # add error if user does not do interval censoring correctly 
    if(any(intervalvalues==leftcensored2value) || 
       any(intervalvalues==rightcensored2value) ||
       any(intervalvalues==nocensored)) stop ('Check interval censoring.')
    
    
    #replacing any negative numbers with 0
    #there might be negative numbers, but these negative values will cause error in the likelihood function
    final<-rapply(final,function(x) ifelse(x<0,0,x), how = "replace")
    return(final)
  } #end fixdata_univariatecase function
  
  #####
  #####
  
  dqpois = function(n, lambda, quasipoisson_phi) {
    mu = lambda
    k = mu/(quasipoisson_phi - 1)
    r = dnbinom(n, mu = mu, size = k)
    return(r)
  }
  
  pqpois = function(n, lambda, quasipoisson_phi, lower.tail) {
    mu = lambda
    k = mu/(quasipoisson_phi - 1)
    r = pnbinom(n, mu = mu, size = k, lower.tail = lower.tail)
    return(r)
  }
  
  
  #A dpois_trunc gives right-truncation Poisson probabilities with quasipoisson_phi value
  dqpois_trunc<-function(n,lambda,quasipoisson_phi, upperbound){
    density.output = dqpois(n,lambda, quasipoisson_phi)*(1/(1-(pqpois((upperbound-1),lambda,quasipoisson_phi, lower.tail=FALSE))))
    return(density.output)} #end dpois_trunc_quasi
  
  #"ppois_truc" is a right truncated Poisson distribution function
  #This will be used in loglikelihood_univariatecase function
  ppois_trunc_quasi<-function(n,lambda,quasipoisson_phi, upperbound,lower.tail){
    if (lower.tail==FALSE) {
      x_to_up<-data.frame((n+1):upperbound) #greater than (>). NOT greater than AND equal to (>= or +)
      dtsum=NULL
      for (i in 1:nrow(x_to_up)){
        dtsum[i]=dqpois_trunc(x_to_up[i,],lambda,quasipoisson_phi, upperbound)}
      return(sum(dtsum))
    }
    
    if (lower.tail==TRUE) {
      zero_to_n<-data.frame(0:n) #less than AND equal to (<=). NOT less than (<)
      dtsum=NULL
      for (i in 1:nrow(zero_to_n)){
        dtsum[i]=dqpois_trunc(zero_to_n[i,],lambda,quasipoisson_phi, upperbound)}
      return(sum(dtsum))
    }
    
  } #end ppois_truc_quasi function
  
  ##### end function that help format data first ###################
  
  
  #The "loglikelihood_univariatecase" gives the Log Likelihood for a simple frequency table
  #This function needed to be provided first in order to find the lambda that maximizes the univariate table
  #The censoredtable provided in this function will be from the fixdata_univariatecase function
  #This function will be ran on both hhs and soh

  loglikelihood_univariatecase<-function (censoredtable,lam){

    #the if statement checks to see if the particular case even exist
    #if a cased doesn't exist then that final value for that case equals 0
    #these if statements take the output from the fixdata_univariatecase function and reads in the category number into the ppois/dpois functions
    #then the function multiplies the ppois/dpois output by the corresponding count number
    #the sum is taken because there could be more than 1 of a particular category (usually more multiple cases of interval and exact)
    #notice Log is taken after the ppois is computed (log.p=TRUE will NOT be the same mathematical value)
    #also sometimes the ppois_trunc function produces -INF, this function replaces that with a 0

    # add quasipoisson_phi value 
    quasipoisson_phi<-censoredtable$quasipoisson_phi
    
    #< censor case
    if (length(censoredtable$leftcensored1)==0) {finall1=0} else {
      finall1=NULL
      for (i in 1:nrow(censoredtable$leftcensored1)){
        #have a -1 because how the ppois_trunc_quasi function works
        #calculates less than AND equal to (<=) NOT less than (<)
        finall1[i]=sum(log(ppois_trunc_quasi(censoredtable$leftcensored1[i,1]-1,lam,quasipoisson_phi, censoredtable$upperbound,lower.tail=TRUE))*censoredtable$leftcensored1[i,2])}
        finall1<-replace(finall1, is.infinite(finall1),0)
      finall1=sum(finall1)}

    #<= censor case
    if (length(censoredtable$leftcensored2)==0) {finall2=0} else {
      finall2=NULL
      for (i in 1:nrow(censoredtable$leftcensored2)){
        finall2[i]=sum(log(ppois_trunc_quasi(censoredtable$leftcensored2[i,1],lam,quasipoisson_phi, censoredtable$upperbound,lower.tail=TRUE))*censoredtable$leftcensored2[i,2])}
      finall2<-replace(finall2, is.infinite(finall2),0)
      finall2=sum(finall2)}

    #0 censor case
    #different format in for loop
    #here using [1,i], [2,i], and ncol NOT [i,1], [i,2], and nrow because how fixdata_univariatecase outputs this case
    if (length(censoredtable$nocensored)==0) {finale=0} else {
      finale=NULL
      for (i in 1:ncol(censoredtable$nocensored)){
        finale[i]=sum(log(dqpois_trunc(as.numeric(censoredtable$nocensored[1,i]),lam,quasipoisson_phi, censoredtable$upperbound))*as.numeric(censoredtable$nocensored[2, i]))}
      finale<-replace(finale, is.infinite(finale),0)
      finale=sum(finale)}

    #> censor case
    if (length(censoredtable$rightcensored1)==0) {finalg1=0} else {
      finalg1=NULL
      for (i in 1:nrow(censoredtable$rightcensored1)){
        finalg1[i]=sum(log(ppois_trunc_quasi(censoredtable$rightcensored1[i,1],lam,quasipoisson_phi, censoredtable$upperbound,lower.tail=FALSE))*censoredtable$rightcensored1[i,2])}
      finalg1<-replace(finalg1, is.infinite(finalg1),0)
      finalg1=sum(finalg1)}

    #>= or + censor case
    if (length(censoredtable$rightcensored2)==0) {finalg2=0} else {
      finalg2=NULL
      for (i in 1:nrow(censoredtable$rightcensored2)){
        #have a -1 because how the ppois_trunc_quasi function works
        #greater than (>) NOT greater than AND equal to(>= or +)
        finalg2[i]=sum(log(ppois_trunc_quasi(censoredtable$rightcensored2[i,1]-1,lam,quasipoisson_phi,censoredtable$upperbound,lower.tail=FALSE))*censoredtable$rightcensored2[i,2])}
      finalg2<-replace(finalg2, is.infinite(finalg2),0)
      finalg2=sum(finalg2)}

    # - censor case
    if (sum(censoredtable$intervalcensored)==0) {finali=0} else {
      finali=NULL
      for (i in 1:nrow(censoredtable$intervalcensored)){
        #have a -1 because how the ppois_trunc_quasi function works
        finali[i]=sum(log(ppois_trunc_quasi(censoredtable$intervalcensored[i,2],lam,quasipoisson_phi,censoredtable$upperbound,lower.tail=TRUE)-ppois_trunc_quasi((censoredtable$intervalcensored[i,1])-1,lam,quasipoisson_phi, censoredtable$upperbound,lower.tail=TRUE))*censoredtable$intervalcensored[i,3])}
        finali<-replace(finali, is.infinite(finali),0)
      finali=sum(finali)}

    #sums all the functions' final result
    #multiplied by -1 because the optim function (used below) minimizes so this is easiest way to minimize
    final<-(finall1+finall2+finale+finalg1+finalg2+finali)*-1
    return(final)
  } #end of loglikelihood_univariatecase function

  #####
  #####

  #The "findlambda_univariatecase" function finds the lambda that mimizes the loglikelihood_univariate function
  #Note that par is the intial guess, now it is random between 0 and hhs/soh upper bound
  #findlambda_univariatecase has to be done for hhs and area (soh) seperately
  #Remember this function will produce a right shifted lambda because the input 'censoredtable' will be the output from fixdata_univariatecase function 

  findlambda_univariatecase<- function(censoredtable){
    op<- optim(par=runif(1,0,censoredtable$upperbound),loglikelihood_univariatecase, censoredtable=censoredtable, method="Brent", lower=1, upper=censoredtable$upperbound )
    options( warn = -1 )
    return(op$par)
  } #end of findlambda_univariatecase function

  #####
  #####
  
  #start by categorizing the censoring
  newtabledata<-fixdata_univariatecase(censoredtable,upperbound, quasipoisson_phi)
  #now minimize the loglikelihood_univariatecase function to find lambda from censored table
  final=findlambda_univariatecase(newtabledata)
  return(final)
}