Libname h "G:\biostat\class";

/* Identifying factors that influence patient remission through use of logistic regression*/

/* Exploring the outcome variable remission*/

Proc Freq data=h.remission;
tables remiss;
run;
Proc Freq data=h.remission;
tables remiss*cell;
run;

/* Running a correlation matrix to check dependencies among continuous idependent varibales*/

Proc corr data=h.remission;
var cell smear infil li blast temp;
run;

/* Make a scatter plot to assess the trend of the data*/

Proc plot data=h.remission;
plot remiss*cell;
run;

/* Checking for multicollinearity among different subsets of the independent varibale*/
/* Subset 1*/
 Proc Logistic data=H.remission;
 model remiss (event="1")= cell smear infil li blast temp;
 run;

 /* subset 2*/
  Proc Logistic data=h.remission;
  model remiss (event="1")= cell smear li blast temp/expb;
  run;

/* Run logistic regression model with backward selection*/
Proc logistic data=h.remission;
model remiss (event='1')= cell smear infil li blast temp/selection=backward expb;
run;

/*Running model diagonistics to assess the goodness of fit*/

/* Roc curves*/
ods graphics on;
proc
logistic
 data=h.Remission plots(only)=roc(id=obs);
model remiss(event='1') = li / scale=none
                         clparm=wald
                         clodds=pl
                         rsquare;
   effectplot;
run
;
ods graphics off;

/* Examining pearson and deviance residuals*/
proc
logistic
 data=h.Remission;
   model remiss (event='1')= li/ influence iplots; 
run
;
