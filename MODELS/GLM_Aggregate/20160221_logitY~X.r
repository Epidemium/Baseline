NA0 = function(A) {B=A;for(i in 1:length(A)) {if(is.na(A[i])) {B[i]=0;} else {B[i]=A[i];}}; return(B)}  #replaces NA by zero in a column

mydata=read.csv("..\\..\\MATRIX\\matrix.csv",header=TRUE,sep=";",dec=",",stringsAsFactors=FALSE); #reads the matrix of Y and X

myResult=NULL;
mydataG=mydata[mydata$Age=="0_100",]; #let us here work at an age-standardized level: age is not a parameter
mydataG=mydataG[!is.na(mydataG$population),]; #we restrict to lines where population is defined
for( myCancerNum in 8:40) #for each cancer type :40
  {
  mydataGC=mydataG[!is.na(mydataG[,myCancerNum]),]; #consider the lines with Y filled
  assign("Y", (as.numeric(mydataGC[,myCancerNum])+10)*1e-7 );
  myFormula="Y ~ mydataGC$Gender";
  for(myXNum in c(4,7,78:ncol(mydata))) #for each X
     {
     myXName = names(mydata)[myXNum];
     a = as.numeric(mydataGC[,myXNum]); b = !is.na(a);
     assign(myXName, NA0(a));
     if(!anyNA(a)) myFormula=paste(myFormula," + ",myXName, sep=""); #add X if present in all lines
     #if the variable is sufficiently present add it to the regression 
     if( anyNA(a) && sum(b) >49 && myXName!="hib_vacc" && myXName!="diphteria_vacc"  && myXName!="tetanus_vacc")  
        {
        assign(paste("i_", myXName, sep=""), b);
              myFormula=paste(myFormula," + i_",myXName," + ",myXName, sep="");
              }
    } #X have been added
   # run the formula and append the new results to the existing ones
   cat(myFormula, "\n");
   tryCatch({   
          mycoefs=glm( as.formula(myFormula),family="binomial", weights=mydataGC$population/2)$coefficients},
       error=function(e){cat("ERROR :",conditionMessage(e), "\n")});
  if(myCancerNum==8) coefstruct=NA0(mycoefs);
  myNewResult = coefstruct; # initialize with 0
  for(k in 1:length(mycoefs)) myNewResult[names(mycoefs)[k]]=mycoefs[k];
  myResult = cbind(myResult, myNewResult);
  } #cancers have been listed
print(myResult);
