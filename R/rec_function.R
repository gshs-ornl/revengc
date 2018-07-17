library(mipfp)
library(stringr)
library(dplyr)
source("R/format_data_for_functions.R")
source("R/estimated_lambda_function.R")
source("R/seedmatrix_function.R")


# rec produces an uncensored X*Y contingency table 
 rec<-function (X,Y,Xlowerbound,Xupperbound,Ylowerbound,Yupperbound, quasipoisson_phiX,quasipoisson_phiY , seed_estimation_method, lambda_range ){
  
   
   
################################################################################## functions that help format data first
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
   
   
################################################################################### 
   #Case where there is an average/table for X and an average/table for Y

    Averages_andor_Tables  <-function (X, Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                                                          seed_estimation_method, lambda_range) {

    #first have to check if X and Y are tables or averages because this effects how lambda is found
    #case where 2 averages
    if (length(X)==1 && length(Y)==1) {
      #averages represent lambdas
      rowlambda=X
      collambda=Y
    } #end of case where 2 averages

    #case where 2 tables
    if (length(X)>1 && length(Y)>1) {
      rowlambda=estimated_lambda(X, Xupperbound, quasipoisson_phiX)
      collambda=estimated_lambda(Y, Yupperbound, quasipoisson_phiY)
    } #end of case where 2 tables

    #case where X=table and Y=average
    if (length(X)>1 && length(Y)==1) {
      #find lambda from censored table
      rowlambda=estimated_lambda(X, Xupperbound, quasipoisson_phiX)
      #average represent lambda for Y
      collambda=Y
    } #end of case where X=table and Y=average

    #case where X=average and Y=table
    if (length(X)==1 && length(Y)>1) {
      #averages represent lambda for X
      rowlambda=X
      #find lambda from censored table
      collambda=estimated_lambda(Y, Yupperbound, quasipoisson_phiY)
    } #end of case where X=average and Y=table

    #error if averages/bounds are incorrect
    if (rowlambda<0) stop ('Check X inputs. Lower bound might be too high.')
    if (collambda<0) stop ('Check Y inputs. Lower bound might be too high.')

    #error if rowlambda/collambda > bounds
    if (rowlambda > Xupperbound) stop ('Check X inputs. Upper bound might be too low.')
    if (collambda > Yupperbound) stop ('Check Y inputs. Upper bound might be too low.')
      
    #need seperate ranges for both X and Y - incorporate right shift of lower bound.. 
    rowrange<-0:(Xupperbound - Xlowerbound)
    rowrange<-rowrange + Xlowerbound
    colrange<-0:(Yupperbound - Ylowerbound)
    colrange<-colrange + Ylowerbound
    
    # list different lambdas row
    rowlambdaorginal<-rowlambda
    rowlambdaorginalpluslambdarange<-rowlambda+lambda_range
    rowlambdaorginalminuslambdarange<-rowlambda-lambda_range
    lambdarowvalues<-list(rowlambdaorginalminuslambdarange, rowlambdaorginal, rowlambdaorginalpluslambdarange)
    
    # list different lambdas col
    collambdaorginal<-collambda
    collambdaorginalpluslambdarange<-collambda+lambda_range
    collambdaorginalminuslambdarange<-collambda-lambda_range
    lambdacolvalues<-list(collambdaorginalminuslambdarange, collambdaorginal, collambdaorginalpluslambdarange)
    
    # function to output estimated cross tabulations 
    finaltable<-function (rowrange,rowlambda, Xupperbound, colrange,collambda, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                          Xlowerbound, Ylowerbound, seed_estimation_method){
      
    #now calculate right shifted, right truncated probabilities
    rowprob<-dqpois_trunc(rowrange,rowlambda,quasipoisson_phiX, Xupperbound)
    colprob<-dqpois_trunc(colrange,collambda,quasipoisson_phiY, Yupperbound)

    #total row marginals has to equal the total column marginal
    #so making sure that both marginals == 1
    rsum<-sum(rowprob)
    rowc_prob<-(rowprob/rsum)
    csum<-sum(colprob)
    colc_prob<-(colprob/csum)
    
    # generating an intial a table to be updated
    seed <- array(1,dim=c(length(rowc_prob),c(length(colc_prob))))
    # store the margins in a list
    tgt.data <- list(rowc_prob, colc_prob)
    # list of dimensions of each marginal constrain
    tgt.list <- list(1,2)
    # calling the estimated function (seed_estimation_method = ipfp, ml, chi2, lsq)
    seed_estimation_method<-as.character(seed_estimation_method)
    final<- Estimate(seed, tgt.list, tgt.data, method = seed_estimation_method)
    
    #check to see if seed method could converge and give error if not
    if (exists("final")==FALSE) {stop ('Selected matrix estimation method cannot complete convergence.')}

    #function outputs more then the needed matrix, so only grab the matrix
    final<-data.frame(final$x.hat)
    #want names of final output to correspond to the provided bounds
    names(final)<-Ylowerbound:Yupperbound
    row.names(final)<-Xlowerbound:Xupperbound
    return(final)
    } # end finaltable function 
    
    
    #if user does not want a range on lambda then only one table will be produced 
    if (lambda_range==0){
      final<-finaltable(rowrange,lambdarowvalues[[2]], Xupperbound, colrange,lambdacolvalues[[2]], Yupperbound, quasipoisson_phiX, quasipoisson_phiY, 
                        Xlowerbound, Ylowerbound, seed_estimation_method)
      return(final)
    }
    
    #user wanted a lambda range so will produce 3 tables 
    if (lambda_range!=0){
    # for loop going though the lambdas
    final = NULL
    for (i in 1:3){
    final[[i]]<-finaltable(rowrange,lambdarowvalues[[i]], Xupperbound, colrange,lambdacolvalues[[i]], Yupperbound, quasipoisson_phiX, quasipoisson_phiY, 
                           Xlowerbound, Ylowerbound, seed_estimation_method)
    }
    #tell user which tables output
    names(final)<-c("lambda.minus", "lambda.original", "lambda.plus")
    return(final)
  }
    
  } #end of Averages_andor_Tables

################################################################################### end average/table case for both vars
###################################################################################     
  # case where there is a censored contingency table   
  Censored_Contingency_Table<-function (censoredtable, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                                        seed_estimation_method, lambda_range) {
    

    # getting lambda values from margins 
    X = row_marginal(censoredtable)
    rowlambda=estimated_lambda(X, Xupperbound, quasipoisson_phiX)
    Y = column_marginal(censoredtable)
    collambda=estimated_lambda(Y, Yupperbound, quasipoisson_phiY)
    
    #error if averages/bounds are incorrect
    if (rowlambda<0) stop ('Check X inputs. Lower bound might be too high.')
    if (collambda<0) stop ('Check Y inputs. Lower bound might be too high.')
    
    #error if rowlambda/collambda > bounds
    if (rowlambda > Xupperbound) stop ('Check X inputs. Upper bound might be too low.')
    if (collambda > Yupperbound) stop ('Check Y inputs. Upper bound might be too low.')
    
    #need seperate ranges for both X and Y - incorporate right shift of lower bound.. 
    rowrange<-0:(Xupperbound - Xlowerbound)
    rowrange<-rowrange + Xlowerbound
    colrange<-0:(Yupperbound - Ylowerbound)
    colrange<-colrange + Ylowerbound
    
    # list different lambdas row
    rowlambdaorginal<-rowlambda
    rowlambdaorginalpluslambdarange<-rowlambda+lambda_range
    rowlambdaorginalminuslambdarange<-rowlambda-lambda_range
    lambdarowvalues<-list(rowlambdaorginalminuslambdarange, rowlambdaorginal, rowlambdaorginalpluslambdarange)
    
    # list different lambdas col
    collambdaorginal<-collambda
    collambdaorginalpluslambdarange<-collambda+lambda_range
    collambdaorginalminuslambdarange<-collambda-lambda_range
    lambdacolvalues<-list(collambdaorginalminuslambdarange, collambdaorginal, collambdaorginalpluslambdarange)
    
    # get seed from contingency table
    seed<- seedmatrix(censoredtable, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound)

    # function to output estimated cross tabulations 
  finaltable<-function (censoredtable, rowrange,rowlambda, Xupperbound, colrange,collambda, Yupperbound, quasipoisson_phiX, 
                        quasipoisson_phiY, Xlowerbound, Ylowerbound, seed_estimation_method){
      
      # now calculate right shifted, right truncated probabilities
      rowprob<-dqpois_trunc(rowrange,rowlambda,quasipoisson_phiX, Xupperbound)
      colprob<-dqpois_trunc(colrange,collambda,quasipoisson_phiY, Yupperbound)
      
      # total row marginals has to equal the total column marginal
      # so making sure that both marginals == 1
      rsum<-sum(rowprob)
      rowc_prob<-(rowprob/rsum)
      csum<-sum(colprob)
      colc_prob<-(colprob/csum)
      
      # store the margins in a list
      tgt.data <- list(rowc_prob, colc_prob)
      # list of dimensions of each marginal constrain
      tgt.list <- list(1,2)
      # calling the estimated function (seed_estimation_method = ipfp, ml, chi2, lsq)
      final<- Estimate(array(seed,dim=c(length(rowrange),length(colrange))), 
                       tgt.list, tgt.data, method = as.character(seed_estimation_method))
      
      #check to see if seed method could converge and give error if not
      if (exists("final")==FALSE) {stop ('Selected matrix estimation method cannot complete convergence.')}
      
      #function outputs more then the needed matrix, so only grab the matrix
      final<-data.frame(final$x.hat)
      #want names of final output to correspond to the provided bounds
      colnames(final)<-Ylowerbound:Yupperbound
      rownames(final)<-Xlowerbound:Xupperbound
      return(final)
    } # end finaltable function 
    
    
    #if user does not want a range on lambda then only one table will be produced 
    if (lambda_range==0){
      final<-finaltable(censoredtable, rowrange,lambdarowvalues[[2]], Xupperbound, colrange,lambdacolvalues[[2]], Yupperbound, 
                        quasipoisson_phiX, quasipoisson_phiY, Xlowerbound, Ylowerbound, seed_estimation_method)
      return(final)
    }
    
    #user wanted a lambda range so will produce 3 tables 
    if (lambda_range!=0){
      # for loop going though the lambdas
      final = NULL
      for (i in 1:3){
        final[[i]]<-finaltable(censoredtable, rowrange,lambdarowvalues[[i]], Xupperbound, colrange,lambdacolvalues[[i]], Yupperbound, 
                               quasipoisson_phiX, quasipoisson_phiY, Xlowerbound, Ylowerbound, seed_estimation_method)
      }
      #tell user which tables output
      names(final)<-c("lambda.minus", "lambda.original", "lambda.plus")
      return(final)
    }
    
  } #end of Censored_Contingency_Table
  
  ############################################################################################## end censored contingency case
  
 # finish rec function ...   

 #Averages_andor_Tables only runs if X and Y is an average (>0) or univariate frequency table (2 columns)
 if (length(X)>=1 &&
     X!="0" &&
     length(Y)>=1 &&
     Y!="0")
   {final<-Averages_andor_Tables(X, Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                                 seed_estimation_method, lambda_range)}
 
 #Censored_Contingency_Table function only runs if X or Y = 0 and the other variable is a contingency table
 if (X=="0" && length(Y)>2) {final<-Censored_Contingency_Table(Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                                                               seed_estimation_method, lambda_range)}
 if (Y=="0" && length(X)>2) {final<-Censored_Contingency_Table(X, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, quasipoisson_phiX, quasipoisson_phiY,
                                                               seed_estimation_method, lambda_range)}
 
 
 return(final)} # end rec function 
    