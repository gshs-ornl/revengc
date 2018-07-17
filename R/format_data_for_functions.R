library(stringr)

# #The findtypeofcensoring_univariatetable Function list the type of censoring in a provided censored frequency table
# #Type_of_Censoring (L1 (<), L2 (<=), I (-), G1 (>), G2 (>= or +), or U (no censoring)), and Censor_Number (category value corresponding to censoring value)
# 
# findtypeofcensoring_univariatetable<-function (contingencytable){
#   
#   #making a dataframe from the category case (column one)
#   cases<-data.frame(contingencytable)
#   cases<-cases[1]
#   #relabeled column with censoring as Censoring_Symbol
#   names(cases) <- c("Censoring_Symbol")
#   
#   # changed lowercase up uppercase if users input "l, le, i, g, ge"
#   cases$Censoring_Symbol<-toupper(as.matrix(cases$Censoring_Symbol))
#   
#   #will list censoring type in column called Type_of_Censoring
#   cases$Type_of_Censoring <- ""
#   
#   #find what numbers go with censoring and put it in censor_number column
#   cases$Censor_Number<-str_replace_all(cases$Censoring_Symbol, "[-,<,<=,>,>=,+,\\,]", " ")
#   cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[L, LE, I, G, GE]", " ")
#   cases$Censor_Number<-str_replace_all(cases$Censor_Number, "[l, le, i, g, ge]", " ")
#   
#   # remove numbers from censoring_symbol column
#   cases$Censoring_Symbol<-gsub('[0-9]+', '', cases$Censoring_Symbol)
#   
#   #named the symbols allowed in model
#   L1 <- c("<")
#   L1a <- c("L")
#   L2 <- c("<=")
#   L2a <- c("LE")
#   I <- c("-")
#   Ia <- c("I")
#   G1 <- c(">")
#   G1a <- c("G")
#   G2 <- c(">=")
#   G2a<-c("[+]")
#   G2b<-c("GE")
#   
#   # remove any spaces found in symbol column 
#   cases$Censoring_Symbol<-gsub('\\s+', '', cases$Censoring_Symbol)
#   
#   #look to see if these symbols are in the provided table
#   leftcensorcase1<-grep("^<$", cases$Censoring_Symbol)
#   leftcensorcase1a<-grep("^L$", cases$Censoring_Symbol)
#   
#   leftcensorcase2<-grep("^<=$", cases$Censoring_Symbol)
#   leftcensorcase2a<-grep("^LE$", cases$Censoring_Symbol)
#   
#   intervalcensor<-grep("^[-]$", cases$Censoring_Symbol)
#   intervalcensora<-grep("^I$", cases$Censoring_Symbol)
#   
#   rightcensorcase1<-grep("^>$", cases$Censoring_Symbol)
#   rightcensorcase1a<-grep("^G$", cases$Censoring_Symbol)
#   
#   rightcensorcase2<-grep("^>=$", cases$Censoring_Symbol)
#   rightcensorcase2a<-grep("^[+]$", cases$Censoring_Symbol)
#   rightcensorcase2b<-grep("^GE$", cases$Censoring_Symbol)
#   
#   
#   #if the cases were there then this puts an L1, L2, I, G1, G2, or U in column called Type_of_Censoring
#   if (length(leftcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, L1)),"Type_of_Censoring"] <- "L1"}
#   if (length(leftcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, L1a)),"Type_of_Censoring"] <- "L1a"}
#   
#   if (length(leftcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, L2)),"Type_of_Censoring"] <- "L2"}
#   if (length(leftcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, L2a)),"Type_of_Censoring"] <- "L2a"}
#   
#   if (length(intervalcensor)>0){cases[which (str_detect(cases$Censoring_Symbol, I)),"Type_of_Censoring"] <- "I"}
#   if (length(intervalcensora)>0){cases[which (str_detect(cases$Censoring_Symbol, Ia)),"Type_of_Censoring"] <- "Ia"}
#   
#   if (length(rightcensorcase1)>0){cases[which (str_detect(cases$Censoring_Symbol, G1)),"Type_of_Censoring"] <- "G1"}
#   if (length(rightcensorcase1a)>0){cases[which (str_detect(cases$Censoring_Symbol, G1a)),"Type_of_Censoring"] <- "G1a"}
#   
#   if (length(rightcensorcase2)>0){cases[which (str_detect(cases$Censoring_Symbol, G2)),"Type_of_Censoring"] <- "G2"}
#   if (length(rightcensorcase2a)>0){cases[which (str_detect(cases$Censoring_Symbol, G2a)),"Type_of_Censoring"] <- "G2a"}
#   if (length(rightcensorcase2b)>0){cases[which (str_detect(cases$Censoring_Symbol, G2b)),"Type_of_Censoring"] <- "G2b"}
#   
#   
#   #make all the different labels of censoring consistent
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="L1a"]<-c("L1")
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="L2a"]<-c("L2")
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="Ia"]<-c("I")
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="G1a"]<-c("G1")
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="G2a"]<-c("G2")
#   cases$Type_of_Censoring[cases$Type_of_Censoring =="G2b"]<-c("G2")
#   
#   #listing the no censoring cases as "U"
#   cases$Type_of_Censoring[cases$Type_of_Censoring==""]<-"U"
#   
#   #put in errors to let users know that there can't be duplicates of Greater than or Less than 
#   if(length(grep("G1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than (> or G) category')
#   if(length(grep("G2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 greater than or equal to (>= or GE) category')
#   if(length(grep("L2", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than or equal to (<= or LE) category')
#   if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
#   if(length(grep("L1", cases$Type_of_Censoring))>1) stop ('Censored table can only have 1 less than (< or L) category')
#   if(length(grep("L1", cases$Type_of_Censoring))==1 && length(grep("L2", cases$Type_of_Censoring))==1) stop ('Censored table can not  have both a less than (< or L) category and a less than or equal to category (<= or LE)')
#   if(length(grep("G1", cases$Type_of_Censoring))==1 && length(grep("G2", cases$Type_of_Censoring))==1) stop ('Censored table can not have both a greater than (< or G) category and a greater than or equal to category (<= or GE)')
#   
#   
#   
#   return (cases)} #end findtypeofcensoring_univariatetable function
# 
# #####
# #####
# 
# #The fixdata_univariatecase function regonizes the symbols and divides a frequency table up into the 6 censored cases
# #This is a formatting data function 
# #This uses the findtypeofcensoring_univariatetable function
# #Requirement of lower and upper bounds
# #The lower bound provided will be used in calculating a right shift and upper bound for truncation
# 
# fixdata_univariatecase<-function(contingencytable, upperbound, quasipoisson_phi) {
#   
#   # conduct findtypeofcensoring_univariatetable function on data 
#   cases<-findtypeofcensoring_univariatetable(data.frame(contingencytable))
#   
#   # stop if upperbound is smaller than largest number in table
#   largestnumintable<-max(as.numeric(unlist(regmatches(cases$Censor_Number, gregexpr("[[:digit:]]+", cases$Censor_Number)))))
#   if(largestnumintable>upperbound) stop ('Upperbound needs to be larger than or equal to its largest corresponding category number')
#  
#   
#   #gives a row number for each of the 6 censoring types
#   lo1<-as.numeric(which(cases$Type_of_Censoring=='L1'))
#   lo2<-as.numeric(which(cases$Type_of_Censoring=='L2'))
#   inte<-as.numeric(which(cases$Type_of_Censoring=='I'))
#   gre1<-as.numeric(which(cases$Type_of_Censoring=='G1'))
#   gre2<-as.numeric(which(cases$Type_of_Censoring=='G2'))
#   ex<-as.numeric(which(cases$Type_of_Censoring=='U'))
#   
#   #finding what number corresponds to the freqency value based off type of censoring
#   #put as.numeric and as.character to help with format
#   #R was changing decimals to whole numbers without putting as.numeric and as.character
#   countl1<-as.numeric(as.character(contingencytable[lo1,2]))
#   countl2<-as.numeric(as.character(contingencytable[lo2,2]))
#   counti<-as.numeric(as.character(contingencytable[inte,2]))
#   countg1<-as.numeric(as.character(contingencytable[gre1,2]))
#   countg2<-as.numeric(as.character(contingencytable[gre2,2]))
#   counte<-as.numeric(as.character(contingencytable[ex,2]))
#   
#   #finding what number (in the Censor_Number) corresponds to the censor type
#   #values listed in cases$Censor_Number[] are factors
#   #intervalnumber will be as.vector because will have to run strsplit function on this value later
#   lowernumber1<-as.numeric(as.character(cases$Censor_Number[lo1]))
#   lowernumber2<-as.numeric(as.character(cases$Censor_Number[lo2]))
#   intervalnumber<-as.vector(cases$Censor_Number[inte])
#   greaternumber1<-as.numeric(as.character(cases$Censor_Number[gre1]))
#   greaternumber2<-as.numeric(as.character(cases$Censor_Number[gre2]))
#   exactnumber<-as.numeric(as.character(cases$Censor_Number[ex]))
#   
#   #combining the category number (without its symbol) and the freqency number that corresponds to that category number
#   lower1<-t(c(lowernumber1,countl1))
#   lower2<-t(c(lowernumber2,countl2))
#   greater1<-t(c(greaternumber1,countg1))
#   greater2<-t(c(greaternumber2,countg2))
#   #unlike the left and right censor categories there could be multiple of the no censored category
#   exact<-unname(rbind(exactnumber,counte))
#   
#   #spliting the interval(s)
#   if (length(intervalnumber)>0){
#     spl<-na.omit(as.numeric(unlist(strsplit(intervalnumber,' ', fixed=FALSE))))
#     interval<-matrix(spl,length(intervalnumber),2,byrow=TRUE)
#     interval<-unname(cbind(interval,counti))
#   }
#   else {
#     interval=NULL
#   }
#   
#   #applying right shift to upper bound
#   upperbound=upperbound
#   
#   #return values for later use
#   final<-list(leftcensored1=lower1,leftcensored2=lower2, nocensored=exact,
#               rightcensored1=greater1,
#               rightcensored2=greater2, intervalcensored=interval,
#               upperbound=upperbound,
#               quasipoisson_phi = quasipoisson_phi)
#   
#   #replacing any negative numbers with 0
#   #there might be negative numbers, but these negative values will cause error in the likelihood function
#   final<-rapply(final,function(x) ifelse(x<0,0,x), how = "replace")
#   return(final)
# } #end fixdata_univariatecase function
# 
# #####
# #####


#The "column_marginal" function makes a dataframe of the column titles and column marginals
column_marginal<-function(contingencytable){
  # user can read in csv based with header = TRUE or header = FALSE
  # this function will regonize that and act accordingly to get the correct column names
  if (any(na.omit(colnames(contingencytable))=="V1")) {cnames=contingencytable[1,]} else {cnames=colnames(contingencytable)}
  # remove commas from column name
  cnames<-str_replace_all(as.matrix(cnames),",","")
  cnames[cnames==" NA"] <- NA
  cnames[cnames=="NA "] <- NA
  cnames[cnames==""] <- NA
  # remove na values
  cnames<-na.omit(cnames)
  # get column marginals
  csum<-contingencytable[nrow(contingencytable),]
  # remove na values
  csum[which(csum=="NA")]<-NA
  csum<-na.omit(as.numeric(csum))
  # remove comma from column marginals
  csum<-str_replace_all(as.matrix(csum),",","")
  csum[csum==""] <- NA
  csum<-na.omit(as.numeric(csum))
  if (length(csum)!=length(cnames)) {csum<-csum[-length(csum)]}
  coltable<-data.frame(cnames,csum)
  names(coltable)<-c("Column Category", "Marginal Frequencies")
  return(coltable)
} # end of column_marginal function



#The "row_marginal" function makes a dataframe of the row titles and row marginals
row_marginal<-function (contingencytable){
  #get row censored names
  rnames<-contingencytable[,1]
  rnames[rnames==""] <- NA
  #remove commas from row names
  rnames<-str_replace_all(rnames,",","")
  rnames[rnames==" NA"] <- NA
  rnames[rnames=="NA "] <- NA
  rnames[rnames==""] <- NA
  #remove na values
  rnames<-na.omit(rnames)
  #get row marginals
  rsum<-contingencytable[,ncol(contingencytable)]
  #remove commas from row marginals
  rsum<-str_replace_all(rsum,",","")
  rsum<-str_replace_all(rsum," ","")
  rsum[which(rsum=="NA")]<-NA
  rsum<-na.omit(as.numeric(rsum))
  #removing total of row total if present
  if (length(rsum)!=length(rnames)) {rsum<-rsum[-length(rsum)]}
  rowtable<-data.frame(rnames,rsum)
  names(rowtable)<-c("Row Category", "Marginal Frequencies")
  return(rowtable)
} #end of row_marginal function