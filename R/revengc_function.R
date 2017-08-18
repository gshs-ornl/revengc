rec<-function (hhsdata,areadata,hhslowerbound,hhsupperbound,arealowerbound,areaupperbound){

  #There are many sub-functions that have to be developed before getting to the main rec function

  #The layout of this code will be the following:
  #1. create sub-functions for the decoupled cases 1,2, and 3: findtypeofcensoring_univariatecase, fixdata_univariatecase, dpois_trunc, ppois_trunc, loglikelihood_univariatecase, findlambda_univariatecase, IPF
  #2. create main function called "Average_andor_Table_Give_Contingency_Table"
  #3. create sub-functions for case 4: parsingrowx, parsingcoly, findtypeofcensoring_bivariatecase, fixdata_bivariatecase, lambdaxy, pbivpois, loglikelihood_lambda3, lambda123, bivpois.table
  #4. create main function called "Bivariate_GivesContingencyTable"
  #5. complete the rec function by first deciding if
      #hhsdata = average or table,
      #areadata = average or table,
      #OR hhsdata=bivariate table while areadata=0 and visa versa
      #AND THEN this code will either run the Average_andor_Table_Give_Contingency_Table function or Bivariate_GivesContingencyTable function

  #####
  #####

  #STARTING THE SUB-FUNCTIONS FOR CASE 1,2, AND 3


  #The findtypeofcensoring_univariatecase Function list the type of censoring in a provided frequency table
  #The output of this function is a dataframe that has 3 columns:
  #Censoring_Symbol (orginal data fed in), Type_of_Censoring (L1 (<), L2 (<=), I (-), G1 (>), G2 (>= or +), or U (no censoring)), and Censor_Number (category value corresponding to censoring value)
  #This function will be used often

      findtypeofcensoring_univariatecase<-function (data2){

        #making a dataframe from the category case (column one)
        cases<-data.frame(data2)
        cases<-cases[1]
        #relabeled column with censoring as Censoring_Symbol
        names(cases) <- c("Censoring_Symbol")

        #will list censoring type in column called Type_of_Censoring
        cases$Type_of_Censoring <- ""

        #named the symbols allowed in model
        L1 <- c("<")
        L2 <- c("<=")
        I <- c("-")
        G1 <- c(">")
        #note G2=G3
        G2 <- c(">=")
        G3<-c("[+]")

        #look to see if these symbols are in the provided table
        leftcensorcase1<-factor(str_extract(cases$Censoring_Symbol,"<"))
        leftcensorcase2<-factor(str_extract(cases$Censoring_Symbol,"<="))
        intervalcensor<-factor(str_extract(cases$Censoring_Symbol,"-"))
        rightcensorcase1<-factor(str_extract(cases$Censoring_Symbol,">"))
        rightcensorcase2<-factor(str_extract(cases$Censoring_Symbol,">="))
        rightcensorcase3<-factor(str_extract(cases$Censoring_Symbol,"[+]"))

        #if the cases were there then this puts an L1, L2, I, G1, G2, or U in column called Type_of_Censoring
        if (length(leftcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, L1)),"Type_of_Censoring"] <- "L1"}
        if (length(leftcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, L2)),"Type_of_Censoring"] <- "L2"}
        if (length(intervalcensor)>0){cases[which (str_detect(cases$Censoring_Symbol, I)),"Type_of_Censoring"] <- "I"}
        if (length(rightcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, G1)),"Type_of_Censoring"] <- "G1"}
        if (length(rightcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, G2)),"Type_of_Censoring"] <- "G2"}
        if (length(rightcensorcase3)>0){cases[which (str_detect(cases$Censoring_Symbol, G3)),"Type_of_Censoring"] <- "G3"}
        #G2==G3
        cases$Type_of_Censoring[cases$Type_of_Censoring=="G3"]<-"G2"
        #listing the no censoring cases as "U"
        cases$Type_of_Censoring[cases$Type_of_Censoring==""]<-"U"

        #now have 2 new columns in the cases dataframe
        #one column (Censor_Number) has no symbols (<,>,-, including no commas too), just the numeric value of the original censored column
        #the second column (Type_of_Censoring) just has the labels of L1, L2, I, G1, G2, or blank
        cases$Censor_Number<-str_replace_all(cases$Censoring_Symbol, "[-,<,<=,>,>=,+,\\,]", " ")

        return (cases)} #end findtypeofcensoring_univariatecase function

  #####
  #####

  #The fixdata_univariatecase function regonizes the symbols and divides a frequency table up into the 6 censored cases
  #This uses the findtypeofcensoring_univariatecase function
  #This function will be ran on both hhs and soh
  #Also there is a requirement of lower and upper bounds
  #The lower bound provided will be used in calculating a right shift

  fixdata_univariatecase<-function(data2,lowerbound,upperbound) {

    data2<-data.frame(data2)
    cases<-findtypeofcensoring_univariatecase(data2[1])

    #gives a row number for each of the 6 censoring types
    lo1<-which(cases$Type_of_Censoring=='L1')
    lo2<-which(cases$Type_of_Censoring=='L2')
    inte<-which(cases$Type_of_Censoring=='I')
    gre1<-which(cases$Type_of_Censoring=='G1')
    gre2<-which(cases$Type_of_Censoring=='G2')
    ex<-which(cases$Type_of_Censoring=='U')

    #finding what number (in the Censor_Number) corresponds to the censor type
    #values listed in cases$Censor_Number[] are factors
    #need to make these a numeric value instead because will be subtracting the lowerbound later
    #intervalnumber will be as.vector because will have to run strsplit function on this value later
    lowernumber1<-as.numeric(as.character(cases$Censor_Number[lo1]))
    lowernumber2<-as.numeric(as.character(cases$Censor_Number[lo2]))
    intervalnumber<-as.vector(cases$Censor_Number[inte])
    greaternumber1<-as.numeric(as.character(cases$Censor_Number[gre1]))
    greaternumber2<-as.numeric(as.character(cases$Censor_Number[gre2]))
    exactnumber<-as.numeric(as.character(cases$Censor_Number[ex]))

    #finding what number corresponds to the freqency value based off type of censoring
    #put as.numeric and as.character to help with format
    #R was changing decimals to whole numbers without putting as.numeric and as.character
    countl1<-as.numeric(as.character(data2[lo1,2]))
    countl2<-as.numeric(as.character(data2[lo2,2]))
    counti<-as.numeric(as.character(data2[inte,2]))
    countg1<-as.numeric(as.character(data2[gre1,2]))
    countg2<-as.numeric(as.character(data2[gre2,2]))
    counte<-as.numeric(as.character(data2[ex,2]))

    #combining the category number (without its symbol) and the freqency number that corresponds to that category number
    #also subtracting lowerbound from the category number to indicate right shift
    lower1<-t(c(lowernumber1-lowerbound,countl1))
    lower2<-t(c(lowernumber2-lowerbound,countl2))
    greater1<-t(c(greaternumber1-lowerbound,countg1))
    greater2<-t(c(greaternumber2-lowerbound,countg2))
    #unlike the left and right censor categories there could be multiple of the no censored category
    exact<-unname(rbind(exactnumber-lowerbound,counte))

    #spliting the interval(s) and subtracting lowerbound
    if (length(intervalnumber)>0){
      spl<-as.numeric(unlist(strsplit(intervalnumber,' ', fixed=FALSE)))-lowerbound
      interval<-matrix(spl,length(intervalnumber),2,byrow=TRUE)
      interval<-unname(cbind(interval,counti))
    }
    else {
      interval=0
    }

    #applying right shift to upper bound
    upperbound=upperbound-lowerbound

    #return values for later use
    final<-list(leftcensored1=lower1,leftcensored2=lower2, nocensored=exact,
                rightcensored1=greater1,
                rightcensored2=greater2, intervalcensored=interval,
                lowerbound=lowerbound,upperbound=upperbound)

    #replacing any negative numbers with 0
    #there might be negative numbers from applying lowerbound, but these negative values will cause error in the likelihood function
    final<-rapply(final,function(x) ifelse(x<0,0,x), how = "replace")
    return(final)
  } #end fixdata_univariatecase function

  #####
  #####

  #A dpois_trunc gives right-truncation Poisson probabilities

  dpois_trunc<-function(n,lam,upperbound){
    final = dpois(n,lam)*(1/(1-(ppois((upperbound-1),lam,lower.tail=FALSE))))
    return(final)} #end dpois_trunc

  #"ppois_truc" is a right truncated Poisson distribution function
  #This will be used in loglikelihood_univariatecase function

  ppois_trunc<-function(n,lam,upperbound,lower.tail){
    if (lower.tail==FALSE) {
      x_to_up<-data.frame((n+1):upperbound) #greater than (>). NOT greater than AND equal to (>= or +)
      dtsum=NULL
      for (i in 1:nrow(x_to_up)){
        dtsum[i]=dpois_trunc(x_to_up[i,],lam,upperbound)}
      return(sum(dtsum))
    }

    if (lower.tail==TRUE) {
      zero_to_n<-data.frame(0:n) #less than AND equal to (<=). NOT less than (<)
      dtsum=NULL
      for (i in 1:nrow(zero_to_n)){
        dtsum[i]=dpois_trunc(zero_to_n[i,],lam,upperbound)}
      return(sum(dtsum))
    }

  } #end ppois_truc function

  #####
  #####

  #The "loglikelihood_univariatecase" gives the Log Likelihood for a simple frequency table
  #This function needed to be provided first in order to find the lambda that maximizes the univariate table
  #The data2 provided in this function will be from the fixdata_univariatecase function
  #This function will be ran on both hhs and soh

  loglikelihood_univariatecase<-function (data2,lam){

    #the if statement checks to see if the particular case even exist
    #if a cased doesn't exist then that final value for that case equals 0
    #these if statements take the output from the fixdata_univariatecase function and reads in the category number into the ppois/dpois functions
    #then the function multiplies the ppois/dpois output by the corresponding count number
    #the sum is taken because there could be more than 1 of a particular category (usually more multiple cases of interval and exact)
    #notice Log is taken after the ppois is computed (log.p=TRUE will NOT be the same mathematical value)
    #also sometimes the ppois_trunc function produces -INF, this function replaces that with a 0

    #< censor case
    if (length(data2$leftcensored1)==0) {finall1=0} else {
      finall1=NULL
      for (i in 1:nrow(data2$leftcensored1)){
        #have a -1 because how the ppois_trunc function works
        #calculates less than AND equal to (<=) NOT less than (<)
        finall1[i]=sum(log(ppois_trunc(data2$leftcensored1[i,1]-1,lam,data2$upperbound,lower.tail=TRUE))*data2$leftcensored1[i,2])}
        finall1<-replace(finall1, is.infinite(finall1),0)
      finall1=sum(finall1)}

    #<= censor case
    if (length(data2$leftcensored2)==0) {finall2=0} else {
      finall2=NULL
      for (i in 1:nrow(data2$leftcensored2)){
        finall2[i]=sum(log(ppois_trunc(data2$leftcensored2[i,1],lam,data2$upperbound,lower.tail=TRUE))*data2$leftcensored2[i,2])}
      finall2<-replace(finall2, is.infinite(finall2),0)
      finall2=sum(finall2)}

    #0 censor case
    #different format in for loop
    #here using [1,i], [2,i], and ncol NOT [i,1], [i,2], and nrow because how fixdata_univariatecase outputs this case
    if (length(data2$nocensored)==0) {finale=0} else {
      finale=NULL
      for (i in 1:ncol(data2$nocensored)){
        finale[i]=sum(log(dpois_trunc(as.numeric(data2$nocensored[1,i]),lam,data2$upperbound))*as.numeric(data2$nocensored[2,i]))}
      finale<-replace(finale, is.infinite(finale),0)
      finale=sum(finale)}

    #> censor case
    if (length(data2$rightcensored1)==0) {finalg1=0} else {
      finalg1=NULL
      for (i in 1:nrow(data2$rightcensored1)){
        finalg1[i]=sum(log(ppois_trunc(data2$rightcensored1[i,1],lam,data2$upperbound,lower.tail=FALSE))*data2$rightcensored1[i,2])}
      finalg1<-replace(finalg1, is.infinite(finalg1),0)
      finalg1=sum(finalg1)}

    #>= or + censor case
    if (length(data2$rightcensored2)==0) {finalg2=0} else {
      finalg2=NULL
      for (i in 1:nrow(data2$rightcensored2)){
        #have a -1 because how the ppois_trunc function works
        #greater than (>) NOT greater than AND equal to(>= or +)
        finalg2[i]=sum(log(ppois_trunc(data2$rightcensored2[i,1]-1,lam,data2$upperbound,lower.tail=FALSE))*data2$rightcensored2[i,2])}
      finalg2<-replace(finalg2, is.infinite(finalg2),0)
      finalg2=sum(finalg2)}

    # - censor case
    if (sum(data2$intervalcensored)==0) {finali=0} else {
      finali=NULL
      for (i in 1:nrow(data2$intervalcensored)){
        #have a -1 because how the ppois_trunc function works
        finali[i]=sum(log(ppois_trunc(data2$intervalcensored[i,2],lam,data2$upperbound,lower.tail=TRUE)-ppois_trunc((data2$intervalcensored[i,1])-1,lam,data2$upperbound,lower.tail=TRUE))*data2$intervalcensored[i,3])}
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
  #Remember this function will produce a right shifted lambda because the input 'data2' will be the output from fixdata_univariatecase function which includes the subtraction from lowerbound (right shift)

  findlambda_univariatecase<- function(data2){
    op<- optim(par=runif(1,0,data2$upperbound),loglikelihood_univariatecase, data2=data2, method="Brent", lower=1, upper=data2$upperbound )
    options( warn = -1 )
    return(op$par)
  } #end of findlambda_univariatecase function

  #####
  #####

  #This IPF function is only needed in cases where there is no existing cross tabulation
  #The "IPF" function is the actual function that calculates the Iterative proportioanl fitting
  #"IPF" can be found online: http://www.demog.berkeley.edu/~eddieh/datafitting.html
  #Alaska Department of Labor and Workforce Development, 2009 (Updated April 2011)

  IPF <- function(rowcontrol, colcontrol, seed, maxiter=100, closure=0.0001, debugger=FALSE){

    #made sure that rowcontrol and colcontrol == 1
    #commented this part out because R's round off error

    #   #input data checks:
    #   if(debugger)print("checking inputs")
    #   #sum of marginal totals equal and no zeros in marginal totals
    #   if(debugger){print("checking rowsum=colsum")}
    #   if(sum(rowcontrol) != sum(colcontrol))
    #     stop("sum of rowcontrol must equal sum of colcontrol")

    #added new errors BECAUSE
    #IPF might not converge if bounds are set too high
    #so this new error stops the IPF function and returns if the user needs to fix hhs and/or soh bounds
    isit.finite<- function(obj){
      sapply(obj,FUN = function(x) all(is.finite(x)))}
    if (any(is.na(rowcontrol)==TRUE)) return ("rowfault")
    if (any(isit.finite(data.frame(rowcontrol))==FALSE)) return ("rowfault")
    if (any(is.na(colcontrol)==TRUE)) return ("colfault")
    if (any(isit.finite(data.frame(colcontrol))==FALSE)) return ("colfault")

    if(debugger){print("checking rowsums for zeros")}

    if(any(rowcontrol==0)){
      numzero <- sum(rowcontrol==0)
      rowcontrol[rowcontrol==0] <- 0.001
      #warning(paste(numzero, "zeros in rowcontrol argument replaced with 0.001", sep=" "))
    }
    if(debugger){print("Checking colsums for zeros")}

    if(any(colcontrol==0)){
      numzero <- sum(colcontrol==0)
      colcontrol[colcontrol==0] <- 0.001
      #warning(paste(numzero, "zeros in colcontrol argument replaced with 0.001", sep=" "))
    }
    if(debugger){print("Checking seed for zeros")}
    if(any(seed==0)){
      numzero <- sum(seed==0)
      seed[seed==0] <- 0.001
      #warning(paste(numzero, "zeros in seed argument replaced with 0.001", sep=" "))
    }
    #set initial values
    result <- seed
    rowcheck <- 1
    colcheck <- 1
    checksum <- 1
    iter <- 0
    #successively proportion rows and columns until closure or iteration criteria are met

    if(debugger){print(checksum > closure);print(iter < maxiter)}

    while((checksum > closure) && (iter < maxiter))
    {

      if(debugger){print(paste("(re)starting the while loop, iteration=",iter)) }

      coltotal <- colSums(result)
      colfactor <- colcontrol/coltotal

      result <- sweep(result, 2, colfactor, "*")
      if (any(is.na(result)==TRUE) | any(isit.finite(data.frame(result))==FALSE)) return ("colfault")
      if(debugger){
        print(paste("column factor = ",colfactor))
        print(result)
      }

      rowtotal <- rowSums(result)
      rowfactor <- rowcontrol/rowtotal

      result <- sweep(result, 1, rowfactor, "*")
      if (any(is.na(result)==TRUE) | any(isit.finite(data.frame(result))==FALSE)) return ("rowfault")
      if(debugger){
        print(paste("row factor = ",rowfactor))
        print(result)}

      rowcheck <- sum(abs(1-rowfactor))
      colcheck <- sum(abs(1-colfactor))
      checksum <- max(rowcheck,colcheck)
      iter <- iter + 1


    }
    result <- list(fitted.table=result, number.iterations=iter, tolerance=checksum)
    result
  } #end of IPF function

  #####
  #####

  #START THE OFFICIAL FUNCTION FOR CASE 1,2, and 3

  #Case where there is an average/table for household size and an average/table for size of house

  Average_andor_Table_Give_Contingency_Table  <-function (HHS, SOH, hhslowerbound, hhsupperbound, arealowerbound, areaupperbound) {

    #first have to check if hhs and soh are tables or averages because this effects how lambda is found

    #case where 2 averages
    if (length(HHS)==1 && length(SOH)==1) {
      #averages represent lambdas
      rowlambda=HHS-hhslowerbound
      collambda=SOH-arealowerbound
    } #end of case where 2 averages

    #case where 2 tables
    if (length(HHS)>1 && length(SOH)>1) {
      #start by categorizing the censoring
      newhhs<-fixdata_univariatecase(HHS,hhslowerbound,hhsupperbound)
      newsoh<-fixdata_univariatecase(SOH,arealowerbound,areaupperbound)
      #now minimize the loglikelihood_univariatecase function to find lambda from censored table
      rowlambda=findlambda_univariatecase(newhhs)
      collambda=findlambda_univariatecase(newsoh)
    } #end of case where 2 tables

    #case where HHS=table and SOH=average
    if (length(HHS)>1 && length(SOH)==1) {
      #find lambda from censored table
      newhhs<-fixdata_univariatecase(HHS,hhslowerbound,hhsupperbound)
      rowlambda=findlambda_univariatecase(newhhs)
      #average represent lambda for SOH
      collambda=SOH-arealowerbound
    } #end of case where HHS=table and SOH=average

    #case where HHS=average and SOH=table
    if (length(HHS)==1 && length(SOH)>1) {
      #averages represent lambda for HHS
      rowlambda=HHS-hhslowerbound
      #find lambda from censored table
      newsoh<-fixdata_univariatecase(SOH,arealowerbound,areaupperbound)
      collambda=findlambda_univariatecase(newsoh)
    } #end of case where HHS=average and SOH=table

    #error if averages/bounds are incorrect
    if (rowlambda<0) stop ('Check household size inputs. Right now this right shifted lambda value is negative')
    if (collambda<0) stop ('Check area inputs. Right now this right shifted lambda value is negative')

    #error if rowlambda/collambda > bounds
    if (rowlambda > hhsupperbound) stop ('Check household size inputs. Right now this right shifted lambda value is bigger than the household size upper bound')
    if (collambda > areaupperbound) stop ('Check area inputs. Right now this right shifted lambda value is bigger than the area upper bound')

    #let user know what lambdas are
    #DOES NOT INCLUDE RIGHT SHIFT
    # print(c(hhs_lambda = rowlambda + hhslowerbound, area_lambda = collambda + arealowerbound))

    #need seperate ranges for both HHS and SOH
    rowrange<-0:(hhsupperbound-hhslowerbound)
    colrange<-0:(areaupperbound-arealowerbound)

    #now calculate right shifted, right truncated probabilities
    rowprob<-dpois_trunc(rowrange,rowlambda,hhsupperbound)
    colprob<-dpois_trunc(colrange,collambda,areaupperbound)

    #IPF total row marginals has to equal the total column marginal
    #so making sure that both marginals == 1
    rsum<-sum(rowprob)
    rowc_prob<-(rowprob/rsum)
    csum<-sum(colprob)
    colc_prob<-(colprob/csum)

    #The IFP function only accepts certain formats for marginals and a seed
    #rc=new row marginal format
    #cc=new column marginal format
    rc<-data.frame(rowc_prob)
    names(rc)<-c("V1")
    rc<-as.matrix(rc)
    cc<-data.frame(colc_prob)
    names(cc)<-c("V1")
    cc<-as.matrix(cc)

    #IPF Setup:
    #Randomize a seed using dpois function in R then run IPF
    seed<-matrix(dpois(nrow(cc)*nrow(rc),mean(c(rowlambda,collambda))), nrow(rc),nrow(cc))
    ipfmatrix<-IPF(rc,cc,seed)

    #check to see if IPF could converge
    #give error if not
    if (length(ipfmatrix)<=1) {
      if (ipfmatrix=="rowfault") stop ('The Iterative Proportional Fitting (IPF) algorithm cannot complete convergence. Fix the household size lower and/or upper bound(s)')
      if (ipfmatrix=="colfault") stop ('The Iterative Proportional Fitting (IPF) algorithm cannot complete convergence. Fix the area lower and/or upper bound(s)')
    }

    #IPF function outputs more then the needed matrix, so only grab the matrix
    final<-data.frame(ipfmatrix$fitted.table)
    #want names of final output to correspond to the provided bounds
    names(final)<-arealowerbound:areaupperbound
    row.names(final)<-hhslowerbound:hhsupperbound
    return(final)
  } #end of Average_andor_Table_Give_Contingency_Table function

  #####
  #####
  #####
  #####

  #We have now ended code for cases 1,2, and 3
  #NOW STARTING ON SUB-FUNCTIONS FOR CASE 4

  #####
  #####

  #The "parsingrowx" function makes a dataframe of the row titles and row marginals
  #This function is used when given a bivariate poisson table to find lambdax

  parsingrowx<-function (data2){
    #get row censored names
    rnames<-data2[,1]
    rnames[rnames==""] <- NA
    #remove commas from row names
    rnames<-str_replace_all(rnames,",","")
    #remove na values
    rnames<-na.omit(rnames)
    #get row marginals
    rsum<-data2[,ncol(data2)]
    #remove commas from row marginals
    rsum<-str_replace_all(rsum,",","")
    rsum[rsum==""] <- NA
    rsum<-na.omit(rsum)
    #removing total of row total if present
    if (length(rsum)!=length(rnames)) {rsum<-rsum[-length(rsum)]}
    rowtable<-data.frame(rnames,rsum)
    return(rowtable)
  } #end of parsingrowx function

  #####
  #####

  #The "parsingcoly" function makes a dataframe of the column titles and column marginals
  #This function is used when given a bivariate poisson table to find lambday

  parsingcoly<-function(data2){
    #user can read in csv based with header = TRUE or header = FALSE
    #this function will regonize that and act accordingly to get the correct column names
    if (any(na.omit(colnames(data2))=="V1")) {cnames=data2[1,]} else {cnames=colnames(data2)}
    cnames[cnames==""] <- NA
    #remove commas from column name
    cnames<-str_replace_all(as.matrix(cnames),",","")
    #remove na values
    cnames<-na.omit(cnames)
    #get column marginals
    csum<-data2[nrow(data2),]
    #remove comma from column marginals
    csum<-str_replace_all(as.matrix(csum),",","")
    csum[csum==""] <- NA
    csum<-na.omit(csum)
    #removing total of column total if present
    if (length(csum)!=length(cnames)) {csum<-csum[-length(csum)]}
    coltable<-data.frame(cnames,csum)
    return(coltable)
  } #end of parsingcoly function

  #####
  #####

  #The "findtypeofcensoring_bivariatecase" function finds the censoring type for a contingency table
  #Note: there are potentially 36 different types of censoring for a bivariate that could be listed in this function
  #This function considers the row titles and columns titles as a pair
  #The inside of the matrix is considered the count
  #The output of this function has 7 columns:
  #1. row1 = category number for row
  #2. row2 = only present if row intervals, this would be second part of row interval
  #3. col1 = category number for column
  #4. col2 = only present if col intervals, this would be second part of col interval
  #5. count = frequency value for a row and column pair
  #6. censoredtype = type of censoring that the row and column pair gives
  #7. product = filled in later by loglikelihood_lambda3 function

  findtypeofcensoring_bivariatecase<-function(data2){

      #grabbing row and column names of table and making cross tabulations with expand.grid
      rnames<-parsingrowx(data2)$rnames
      cnames<-parsingcoly(data2)$cnames
      cases<-data.frame(expand.grid(rnames,cnames))
      names(cases)<-c("Rows", "Cols")
      #spliting intervals
      #the second part of the interval goes into another column
      row1row2<-str_split_fixed(cases$Rows, "-", 2)
      col1col2<-str_split_fixed(cases$Cols, "-", 2)
      #finding censoring type of all cross tabulations
      rowcensoring<-findtypeofcensoring_univariatecase(cases$Rows)$Type_of_Censoring
      colcensoring<-findtypeofcensoring_univariatecase(cases$Cols)$Type_of_Censoring


      #removing marginals to get to inside of table
      #br = bottom row, tr = top row, rc = right column, and lc = left column
      brgone<-data2[-nrow(data2),]
      #for removing the top row we have to also accounte for if user read in csv with header=TRUE or header=FALSE
      if (any(na.omit(colnames(data2))=="V1")) {trgone=brgone[-1,]} else {trgone=unname(brgone)}
      rcgone<-trgone[, -ncol(trgone)]
      lcgone<-rcgone[,-1]
      Inside<-matrix(as.matrix(lcgone), nrow(cases), 1)
      #removing commas from inside of table
      inside<-str_replace_all(Inside,",","")

      #making dataframe of everything found above
      cases<-data.frame(row1=str_replace_all(row1row2[,1], "[<,<=,>,>=,+]", ""),
                        row2=row1row2[,2],
                        col1=str_replace_all(col1col2[,1], "[<,<=,>,>=,+]", ""),
                        col2=col1col2[,2],
                        count = inside,
                        censoredtype = paste(rowcensoring, colcensoring, sep =""),
                        product="")

      #replace negative numbers with 0's because bivariate poisson distribution codes does not calculate negative numbers
      cases<-replace(cases, cases < 0, 0)
      #replace NA with blanks
      cases[is.na(cases)] <- c("")
      return(cases)
    } #end of findtypeofcensoring_bivariatecase function

  #####
  #####

  #lambdaxy function uses previous functions to find marginal lambdax and then lambday
  #first grabs the row and column marginal tables from the bivariate table then formats the marginal tables to find their lambda

  lambdaxy<-function (data2,hhslowerbound, hhsupperbound, arealowerbound, areaupperbound){
      rowmarginal<-parsingrowx(data2)
      newrowmarg<-fixdata_univariatecase(rowmarginal,hhslowerbound,hhsupperbound)
      rowxlambda<-findlambda_univariatecase(newrowmarg)
      colmarginal<-parsingcoly(data2)
      newcolmarg<-fixdata_univariatecase(colmarginal,arealowerbound,areaupperbound)
      colylambda<-findlambda_univariatecase(newcolmarg)
      final<-data.frame(rowxlambda,colylambda)
      #these values have a right shift (subtraction of lower bounds) because the fixdata_univariatecase and findlambda_univariatecase functions
      names(final)<-c("Rowxmarginal", "Colymarginal")
      return(final)
    } #end of lambdaxy function

  #####
  #####

  #Now grouping all the bivariate information together (just like in univariate case)
  #The output from the "fixdata_bivariatecase" function will be fed into the loglikelihood_lambda3 function

  fixdata_bivariatecase<-function(bivariatetable,hhslowerbound, hhsupperbound, arealowerbound, areaupperbound){
     ij<-lambdaxy(bivariatetable,hhslowerbound, hhsupperbound, arealowerbound, areaupperbound)
     lambdax=ij$Rowxmarginal
     lambday=ij$Colymarginal
     tableforbiv<-findtypeofcensoring_bivariatecase(bivariatetable)
     #need to indicate a right shift in the row and column marginal pairs
     #tableforbiv$row1... are factors so need as.numeric and as.character so lower bounds can be subtracted
     tableforbiv$row1<-as.numeric(as.character(tableforbiv$row1)) -hhslowerbound
     tableforbiv$row2<-as.numeric(as.character(tableforbiv$row2)) -hhslowerbound
     tableforbiv$col1<-as.numeric(as.character(tableforbiv$col1)) -arealowerbound
     tableforbiv$col2<-as.numeric(as.character(tableforbiv$col2)) -arealowerbound
     return(list(lambdax=lambdax,
                 lambday=lambday,
                 tableforbiv=tableforbiv,
                 hhsupperbound = hhsupperbound,
                 areaupperbound = areaupperbound))} #end of fixbivdata_bivariatecase function

  #####
  #####

  #Before developing 36 cases used for Bivariate Likelihood
  #the following pbivpois function to needed to calculate the bivariate poisson distribution

  "pbivpois" <-
      function(x, y=NULL, lambda = c(1, 1, 1), log=FALSE) {
        # ------------------------------------------------------------------------------
        # Karlis and Ntzoufras (2003, 2004)
        # EM algorithms for Bivariate Poisson Models
        # ------------------------------------------------------------------------------
        # x      : matrix or vector of length n
        # y      : vector of length n. If x is matrix then it is not used
        # lambda : parameters of the bivariate poisson distribution
        # log    : argument controlling the calculation of the log-probability or the
        #          probability function.
        # ------------------------------------------------------------------------------

        if ( is.matrix(x) ) {
          var1<-x[,1]
          var2<-x[,2]
        }
        else if (is.vector(x)&is.vector(y)){
          if (length(x)==length(y)){
            var1<-x
            var2<-y
          }
          else{
            stop('lengths of x and y are not equal')
          }
        }
        else{
          stop('x is not a matrix or x and y are not vectors')
        }
        n <- length(var1)
        logbp<-vector(length=n)
        #
        for (k in 1:n){
          x0<-var1[k]
          y0<-var2[k]
          xymin<-min( x0,y0 )
          lambdaratio<-lambda[3]/(lambda[1]*lambda[2])
          #
          i<-0:xymin
          sums<- -lgamma(var1[k]-i+1)-lgamma(i+1)-lgamma(var2[k]-i+1)+i*log(lambdaratio)
          maxsums <- max(sums)
          sums<- sums - maxsums
          logsummation<- log( sum(exp(sums)) ) + maxsums
          logbp[k]<- -sum(lambda) + var1[k] * log( lambda[1] ) + var2[k] * log( lambda[2] ) + logsummation
        }
        if (log) { result<-logbp }
        else     { result<-exp(logbp)  }
        result
      } #end of function pbivpois

  #####
  #####

  #Likelihood for lambda3
  #Give this function the output of the fixdata_bivariatecase function

  loglikelihood_lambda3<-function (formatted_bivariatetable, lambda3){

       lambda1<-formatted_bivariatetable$lambdax-lambda3
       lambda2<-formatted_bivariatetable$lambday-lambda3
       k_hhs<-formatted_bivariatetable$hhsupperbound
       k_soh<-formatted_bivariatetable$areaupperbound

      #now going through the 36 possible censoring types
      #will calculate bivarite poisson distributions based off these censored type
      #the calculation will be put in an empty product column
      #the bivcensoring function only returns the product column

      bivcensoring<-function (x){

        #x = output from fixdata_bivariatecase$tableforbiv
        row1<-as.numeric(as.character(x["row1"]))
        row2<-as.numeric(as.character(x["row2"]))
        col1<-as.numeric(as.character(x["col1"]))
        col2<-as.numeric(as.character(x["col2"]))
        count<-as.numeric(as.character(x["count"]))
        censoredtype<-as.character(x["censoredtype"])
        product <-x["product"]

        if (censoredtype=='II') {
          #row is interval, column is interval
          in.in<-function (x,x1,y,y1,z,lambda1,lambda2,lambda3){
            xx<-x:x1
            yy<-y:y1
            x.y<-expand.grid(xx,yy)
            ii<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ii))}
          product<-in.in(row1,row2,col1,col2,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='UU') {
          #both values are uncensored
          un.un<-function (x,y,z,lambda1,lambda2,lambda3){
            uu<-pbivpois(x, y, lambda = c(lambda1,lambda2,lambda3),log=TRUE)*z
            return(sum(uu))}
          product<- un.un(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='UG1') {
          #row is uncensored, column is >
          un.greater1<-function (x,y,z,k,lambda1,lambda2,lambda3){
            y<-y+1
            yy<-y:k
            xx<-rep(x,length(yy))
            ug<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ug))}
          product<- un.greater1(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='UL1') {
          #row is uncensored, column is <
          un.less1<-function (x,y,z,lambda1,lambda2,lambda3){
            y<-y-1
            yy<-0:y
            xx<-rep(x,length(yy))
            ul<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ul))}
          product<- un.less1(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1U') {
          #row is <, column uncensored
          less1.un<-function (x,y,z,lambda1,lambda2,lambda3){
            x<-x-1
            xx<-0:x
            yy<-rep(y,length(xx))
            lu<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lu))}
          product<- less1.un(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1L1') {
          #row is <, column <
          less1.less1<-function(x,y,z,lambda1,lambda2,lambda3){
            x<-x-1
            y<-y-1
            xx<-0:x
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            ll<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ll))}
          product<- less1.less1(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1G1') {
          #row is <, column is >
          less1.greater1<-function(x,y,z,k,lambda1,lambda2,lambda3){
            x<-x-1
            y<-y+1
            xx<-0:x
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            lg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lg))}
          product<- less1.greater1(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1U') {
          #row is >, column is uncensored
          greater1.un<-function (x,y,z,k,lambda1,lambda2,lambda3){
            x<-x+1
            xx<-x:k
            yy<-rep(y,length(xx))
            gu<-pbivpois(xx, yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gu))}
          product<- greater1.un(row1,col1,count,k_hhs,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1L1') {
          #row is >, column <
          greater1.less1<-function (x,y,z,k,lambda1,lambda2,lambda3){
            x<-x+1
            y<-y-1
            xx<-x:k
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            gl<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gl))}
          product<- greater1.less1(row1,col1,count,k_hhs,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1G1') {
          #row is >, column >
          greater1.greater1<-function (x,y,z,k,k2,lambda1,lambda2,lambda3){
            x<-x+1
            y<-y+1
            xx<-x:k
            yy<-y:k2
            x.y<-expand.grid(xx,yy)
            gg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gg))}
          product<- greater1.greater1(row1,col1,count,k_hhs,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='UI') {
          #row is uncensored, column is interval
          un.in<-function (x,y,y1,z,lambda1,lambda2,lambda3){
            yy<-y:y1
            xx<-rep(x,length(yy))
            ui<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ui))}
          product<- un.in(row1,col1,col2,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1I') {
          #row is <, column is interval
          less1.in<-function(x,y,y1,z,lambda1,lambda2,lambda3){
            x<-x-1
            x<-0:x
            yy<-y:y1
            x.y<-expand.grid(x,yy)
            ui<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ui))}
          product<- less1.in(row1,col1,col2,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='IU') {
          #row is interval, column is uncensored
          in.un<-function (x,x1,y,z,lambda1,lambda2,lambda3){
            xx<-x:x1
            yy<-rep(y,length(xx))
            iu<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(iu))}
          product<- in.un(row1,row2,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='IL1') {
          #row is interval, column is <
          in.less1<-function (x,x1,y,z,lambda1,lambda2,lambda3){
            xx<-x:x1
            y<-y-1
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            il<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(il))}
          product<- in.less1(row1,row2,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='IG1') {
          #row is interval, column is >
          in.greater1<-function (x,x1,y,z,k,lambda1,lambda2,lambda3){
            y<-y+1
            xx<-x:x1
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            ig<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ig))}
          product<- in.greater1(row1,row2,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1I') {
          #row is >, column is interval
          greater1.in<-function(x,y,y1,z,k,lambda1,lambda2,lambda3){
            x<-x+1
            xx<-x:k
            yy<-y:y1
            x.y<-expand.grid(xx,yy)
            gi<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gi))}
          product<- greater1.in(row1,col1,col2,count, k_hhs, lambda1,lambda2,lambda3)
        }


        if (censoredtype=='UG2') {
          #row is uncensored, column is >= or +
          un.greater2<-function (x,y,z,k,lambda1,lambda2,lambda3){
            yy<-y:k
            xx<-rep(x,length(yy))
            ug<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ug))}
          product<- un.greater2(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='UL2') {
          #row is uncensored, column is <= or +
          un.less2<-function (x,y,z,lambda1,lambda2,lambda3){
            yy<-0:y
            xx<-rep(x,length(yy))
            ul<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ul))}
          product<- un.less2(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2U') {
          #row is <= or +, column uncensored
          less2.un<-function (x,y,z,lambda1,lambda2,lambda3){
            xx<-0:x
            yy<-rep(y,length(xx))
            lu<-pbivpois(xx,yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lu))}
          product<- less2.un(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2L2') {
          #row is <= or +, column <= or +
          less2.less2<-function(x,y,z,lambda1,lambda2,lambda3){
            xx<-0:x
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            ll<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ll))}
          product<- less2.less2(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2L1') {
          #row is <= or +, column is <
          less2.less1<-function(x,y,z,lambda1,lambda2,lambda3){
            y<-y-1
            xx<-0:x
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            ll<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ll))}
          product<- less2.less1(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1L2') {
          #row is <, column <= or +
          less1.less2<-function(x,y,z,lambda1,lambda2,lambda3){
            x<-x-1
            xx<-0:x
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            ll<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ll))}
          product<- less1.less2(row1,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2G2') {
          #row is <= or +, column is >= or +
          less2.greater2<-function(x,y,z,k,lambda1,lambda2,lambda3){
            xx<-0:x
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            lg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lg))}
          product<- less2.greater2(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2G1') {
          #row is <= or +, column is >
          less2.greater1<-function(x,y,z,k,lambda1,lambda2,lambda3){
            y<-y+1
            xx<-0:x
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            lg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lg))}
          product<- less2.greater1(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L1G2') {
          #row is <, column is >=
          less1.greater2<-function(x,y,z,k,lambda1,lambda2,lambda3){
            x<-x-1
            xx<-0:x
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            lg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(lg))}
          product<- less1.greater2(row1,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2U') {
          #row is >=, column is uncensored
          greater2.un<-function (x,y,z,k,lambda1,lambda2,lambda3){
            xx<-x:k
            yy<-rep(y,length(xx))
            gu<-pbivpois(xx, yy, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gu))}
          product<- greater2.un(row1,col1,count,k_hhs,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2G2') {
          #row is >= or +, column >= or +
          greater2.greater2<-function (x,y,z,k,k2,lambda1,lambda2,lambda3){
            xx<-x:k
            yy<-y:k2
            x.y<-expand.grid(xx,yy)
            gg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gg))}
          product<- greater2.greater2(row1,col1,count,k_hhs,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2G1') {
          #row is >= or +, column >
          greater2.greater1<-function (x,y,z,k,k2,lambda1,lambda2,lambda3){
            y<-y+1
            xx<-x:k
            yy<-y:k2
            x.y<-expand.grid(xx,yy)
            gg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gg))}
          product<- greater2.greater1(row1,col1,count,k_hhs,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1G2') {
          #row is >, column >= or +
          greater1.greater2<-function (x,y,z,k,k2,lambda1,lambda2,lambda3){
            x<-x+1
            xx<-x:k
            yy<-y:k2
            x.y<-expand.grid(xx,yy)
            gg<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gg))}
          product<- greater1.greater2(row1,col1,count,k_hhs,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='L2I') {
          #row is <= or +, column is interval
          less2.in<-function(x,y,y1,z,lambda1,lambda2,lambda3){
            x<-0:x
            yy<-y:y1
            x.y<-expand.grid(x,yy)
            ui<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ui))}
          product<- less2.in(row1,col1,col2,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='IL2') {
          #row is interval, column is <= or +
          in.less2<-function (x,x1,y,z,lambda1,lambda2,lambda3){
            xx<-x:x1
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            il<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(il))}
          product<- in.less2(row1,row2,col1,count,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='IG2') {
          #row is interval, column is >= or +
          in.greater2<-function (x,x1,y,z,k,lambda1,lambda2,lambda3){
            xx<-x:x1
            yy<-y:k
            x.y<-expand.grid(xx,yy)
            ig<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(ig))}
          product<- in.greater2(row1,row2,col1,count,k_soh,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2I') {
          greater2.in<-function(x,y,y1,z,k,lambda1,lambda2,lambda3){
            xx<-x:k
            yy<-y:y1
            x.y<-expand.grid(xx,yy)
            gi<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gi))}
          product<- greater2.in(row1,col1,col2,count, k_hhs, lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2L1') {
          greater2.less1<-function (x,y,z,k,lambda1,lambda2,lambda3){
            y<-y-1
            xx<-x:k
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            gl<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gl))}
          product<- greater2.less1(row1,col1,count, k_hhs, lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G2L2') {
          greater2.less2<-function (x,y,z,k,lambda1,lambda2,lambda3){
            xx<-x:k
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            gl<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gl))}
          product<- greater2.less2(row1,col1,count, k_hhs,lambda1,lambda2,lambda3)
        }

        if (censoredtype=='G1L2') {
          #row is >, column <=
          greater1.less2<-function (x,y,z,k,lambda1,lambda2,lambda3){
            x<-x+1
            xx<-x:k
            yy<-0:y
            x.y<-expand.grid(xx,yy)
            gl<-pbivpois(x.y$Var1,x.y$Var2, lambda = c(lambda1,lambda2,lambda3), log=TRUE)*z
            return(sum(gl))}
          product<- greater1.less2(row1,col1,count, k_hhs, lambda1,lambda2,lambda3)
        }
       return (product)
        } #end of bivcensoring function

      almost<-apply(data.frame(formatted_bivariatetable$tableforbiv),1,bivcensoring)
      #We use a min function to find lambda 3 so cannot forget multiplying by -1
      final<-sum(almost)*-1
      return(final)

    } #end of loglikelihood_lambda3 function

  #####
  #####

  #lambda123 function finds lambda1, lambda2, and lambda3
  #This functioninputs of  the censored bivariate table along with bounds for both hhs and soh

  lambda123<-function (bivariatetable,hhslowerbound,hhsupperbound, arealowerbound, areaupperbound){
      formatted_bivariatetable<-fixdata_bivariatecase(bivariatetable,hhslowerbound,hhsupperbound, arealowerbound, areaupperbound)
      lambdax=formatted_bivariatetable$lambdax
      lambday=formatted_bivariatetable$lambday
      #lambda3 has limits
      minxy<-min(lambdax,lambday)
      #par is the intial guess, it is random between 0 and minxy
      #lambda 3 also has upper (minxy) and lower (cannot be 0) bound restrictions
      op<- optim(par=runif(1,0,minxy),loglikelihood_lambda3, method = "Brent", formatted_bivariatetable=formatted_bivariatetable,lower = 0, upper = minxy)
      options( warn = -1 )
      lambda3<-op$par
      lambda1<-lambdax-lambda3
      lambda2<-lambday-lambda3
      final<-data.frame(lambda1,lambda2,lambda3)
      return(final)
    } #end lambda123 function

  #####
  #####

  #Now with all 3 lambdas we need the bivpois.table function to calculate the bivariate poission distribtions in table format
  #This functions contingency table starts from 0 and ends at selected x (rows) and y (cols)

  "bivpois.table" <-
    function(x, y, lambda = c(1, 1, 1))
    {
      # ------------------------------------------------------------------------------
      # Karlis and Ntzoufras (2003, 2004)
      # EM algorithms for Bivariate Poisson Models
      # ------------------------------------------------------------------------------
      # x     : 1st count variable
      # y     : 2nd count variable
      # lambda: parameters of the bivariate poisson distribution.
      # ------------------------------------------------------------------------------

      j<-0
      n <- length(x)
      maxy <- c(max(x), max(y))      #Set initial values for parameters
      lambda1 <- lambda[1]
      lambda2 <- lambda[2]
      lambda3 <- lambda[3]
      if((x == 0) | (y == 0)) {
        prob <- matrix(NA, nrow = maxy[1] + 1, ncol = maxy[2]+1, byrow = T)
        prob[maxy[1] + 1, maxy[2] + 1] <- exp( - lambda3) *
          dpois(x[j], lambda1[j]) * dpois(y[j], lambda2[j])
      }
      else {
        prob <- matrix(NA, nrow = maxy[1] + 1, ncol = maxy[2]+1, byrow = T)
        k <- 1
        m <- 1
        prob[k, m] <- exp( - lambda1 - lambda2 - lambda3)
        for(i in 2:(maxy[1] + 1)) {
          prob[i, 1] <- (prob[i - 1, 1] * lambda1)/(i - 1)
        }
        for(j in 2:(maxy[2] + 1)) {
          prob[1, j] <- (prob[1, j - 1] * lambda2)/(j - 1)
        }
        for(j in 2:(maxy[2] + 1)) {
          for(i in 2:(maxy[1] + 1)) {
            prob[i, j] <- (lambda1 * prob[i - 1, j] +
                             lambda3 * prob[i - 1, j - 1])/(i - 1)
          }
        }
      }
      result <- prob
      result
    } #end bivpois.table function

  #####
  #####

  #START THE OFFICIAL FUNCTION FOR CASE 4

  #This is a function used for censored contingency table

  Bivariate_GivesContingencyTable<-function (bivtable,hhslowerbound, hhsupperbound, arealowerbound, areaupperbound) {

    #deciding whether table provided is hhs,soh or soh,hhs
    #This is going off assumption that soh category numbers with allways be > hhs category numbers
    #if the soh and hhs caegory numbers sum together and are equal then this function keeps rows as hhs and columns as soh
    cnamess<-parsingcoly(bivtable)$cnames
    rnamess<-parsingrowx(bivtable)$rnames
    #removing symbols found in column and row category names
    numbers_col<-as.numeric(str_replace_all(cnamess, "[-,<,<=,>,>=,+,\\,]", ""))
    numbers_row<-as.numeric(str_replace_all(rnamess, "[-,<,<=,>,>=,+,\\,]", ""))
    #summing up both category names
    colsum<-sum(na.omit(numbers_col))
    rowsum<-sum(na.omit(numbers_row))
    #transpose table if soh are rows rather than columns
    if (colsum>=rowsum) {bivtable} else {bivtable<-data.frame(t(bivtable))}

    #finding lambdas to calculate bivariate Poisson distribution table
    lambdaonetwothree<-lambda123(bivtable,hhslowerbound, hhsupperbound, arealowerbound, areaupperbound)
    l1<-lambdaonetwothree$lambda1
    l2<-lambdaonetwothree$lambda2
    l3<-lambdaonetwothree$lambda3

    #let user know what lambdas are
    # print(c(lambda1=l1, lambda2=l2, lambda3=l3))

    #making uncensored contingency table with found lambda values
    bivtable<-data.frame(bivpois.table((hhsupperbound-hhslowerbound),(areaupperbound-arealowerbound),c(l1,l2,l3)))

    #making the inside of the table equals 1
    sumbiv<-sum(bivtable)
    bivtable<-data.frame(bivtable/sumbiv)

    #making column and row names correspond to the bounds provided
    names(bivtable)<-arealowerbound:areaupperbound
    row.names(bivtable)<-hhslowerbound:hhsupperbound
    return(bivtable)
   } #end Bivariate_GivesContingencyTable function

  #####
  #####

  #COMPLETING THE MAIN "rec" function

  #make it impossible for user to enter in a negative value for bound
  bounds<-as.numeric(c(hhslowerbound, hhsupperbound, arealowerbound, areaupperbound))
  if(any(bounds <0)) stop ('There cannot be any negative bound values')
  #upper can not be less than lower bound
  if(hhsupperbound<hhslowerbound) stop ('Upper bound can not be less than lower bound')
  if(areaupperbound<arealowerbound) stop ('Upper bound can not be less than lower bound')
  #bounds can not be equal
  if(areaupperbound==arealowerbound) stop ('Upper bound can not be equal to lower bound')
  if(hhsupperbound==hhslowerbound) stop ('Upper bound can not be equal to lower bound')

  #Average_andor_Table_Give_Contingency_Table only runs if hhsdata and areadata is an average (>0) or univariate frequency table (2 columns)
  if (length(hhsdata)>=1 &&
      hhsdata!="0" &&
      length(areadata)>=1 &&
      areadata!="0")
     {final<-Average_andor_Table_Give_Contingency_Table(hhsdata,areadata,hhslowerbound,hhsupperbound,arealowerbound,areaupperbound)}

  #Bivariate_GivesContingencyTable function only runs if hhsdata or areadata = 0 and the other variable is a contingency table
  if (hhsdata=="0" && length(areadata)>2) {final<-Bivariate_GivesContingencyTable(areadata,hhslowerbound,hhsupperbound,arealowerbound,areaupperbound)}
  if (areadata=="0" && length(hhsdata)>2) {final<-Bivariate_GivesContingencyTable(hhsdata,hhslowerbound,hhsupperbound,arealowerbound,areaupperbound)}

  #return error if user has wrong input
  if (exists("final")==FALSE) stop('ERROR: Check input!')
  return(final)
} #end rec function
