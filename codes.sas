/* Multilinear regression project to make a prediction model of body fat based on a set of other independent variables,
   including density age weight height neck chest abdomen hip thigh knee ankle biceps forearm wrist*/
   

/* USing backward selection method to reduce the set of predictor variables to those that are necessary and account
to nearly as much variance in the outcome variable as is accounted for in the total set*/
 
ods html body='G:\biostat\hmw\backwards.htm';
proc reg data= hmw;
model bodyfat= age weight height neck chest abdomen hip thigh knee ankle biceps forearm wrist/selection=backward;
run;
ods html close;

/*Running a correlation matrix to assess multicolinnearity effect among the three narrowed-down predictor variables*/

Ods html body='G:\biostat\hmw\corr.htm';
Proc corr data=hmw;
var age weight neck abdomen thigh forearm wrist;
run;
ods html close;

/* Assessing the effect of multicolinearity in the model by removing the varibles that are high correlated*/

ods pdf body='G:\biostat\hmw\reg.pdf';
proc reg data= hmw;
model bodyfat= chest age;
run;
ods pdf close;

ods pdf body='G:\biostat\hmw\reg_chest.pdf';
proc reg data= hmw;
model bodyfat=wrist age;
run;
ods pdf close;

/* Computing Variance Inflation Factor (VIF) for full model to identify varibales with high VIF*/

ods html body='G:\biostat\hmw\vif.htm';
proc reg data=hmw;
model bodyfat= age weight neck abdomen thigh forearm wrist/ tol vif collin;
run;
ods html close;

