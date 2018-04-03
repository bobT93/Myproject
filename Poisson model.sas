/* Comparative analysis using different models to capture deman of care with other patients variables*/
libname HW "G:\biostat\Homework3";

data HW.hmw;
 set homework;
run;

data Hw.hmw1 (keep= ofp hosp health numchron gender school privins);
 set hw.hmw;
run;
*Dataset exploration;
Proc means
 data=hw.hmw1;
 var ofp numchron school;
 run;
Proc Univariate data=Hw.hmw1;
var ofp hosp;
histogram;
run;
proc freq
 data=hw.hmw1;
   table ofp/ plots(only)=freqplot(scale=freq);
run;

/*Fitting Poisson Regression*/
Proc genmod data=hw.hmw1;
 class health gender privins;
 model ofp=hosp health numchron gender school privins/dist=poisson link=log;
run;
   *Residual analysis for Possion Regression;

proc genmod data=hw.hmw1;
class health gender privins;
model ofp = hosp health numchron gender school privins/dist = poisson link= log;
output out       = Residuals
          pred      = Pred
          resraw    = Resraw
          reschi    = Reschi
          resdev    = Resdev;
run;
proc print
 data=Residuals ;
run;
proc plot
 data = Residuals;
plot Resdev*Pred;
run;
proc univariate
 data = Residuals;
var Resdev;
histogram;
run
;


/* Fitting Negative Binomial*/
Proc genmod data=hw.hmw1;
 class health gender privins;
 model ofp=hosp health numchron gender school privins/dist=negbin link=log;
run;
/*Residual analysis for Negative Binomial*/
proc genmod data=hw.hmw1;
class health gender privins;
model ofp = hosp health numchron gender school privins/dist = negbin link= log;
output out       = Residuals1
          pred      = Pred
          resraw    = Resraw
          reschi    = Reschi
          resdev    = Resdev;
run;

proc print
 data=Residuals1 ;
run;
proc plot
 data = Residuals1;
plot Resdev*Pred;
run;
proc univariate
 data = Residuals1;
var Resdev;
histogram;
run;


/* Fitting Zero Inflated model*/
Proc genmod data=hw.hmw1;
 class health gender privins;
 model ofp=hosp health numchron gender school privins/dist=zip link=log;
 zeromodel hosp health numchron gender school privins/link=logit;
run;

/*Residuals analysis for ZIP model*/

proc genmod data= hw.hmw1;
  class health gender privins;
model ofp = hosp health numchron gender school privins /dist=zip;
  zeromodel hosp health numchron gender school privins /link = logit;
   output out=zip predicted=pred pzero=pzero;
   ods output Modelfit=fit;
run;
proc means data=hw.hmw1
 noprint;
 var ofp;
output out=maxcount max=max N=N;
run;
data _null_;
  set maxcount;
  call symput('N',N);
  call symput('max',max);
run;
%let max=%sysfunc(strip(&max));

ods html body='diagnostics1.htm' 
style=HTMLBlue;
proc sgplot data=zipprob;
scatter x=ofp y=p /
           markerattrs=(symbol=CircleFilled size=5px 
color=blue);
   scatter x=ofp y=zip /markerattrs=(symbol=TriangleFilled 
size=5px color=red);
   xaxis type=discrete;
run;
ods html close;
data zip(drop= i);
   set zip;
   lambda=pred/(1-pzero);
   array ep{0:&max} ep0-ep&max;
   array c{0:&max} c0-c&max;
   do i = 0 to &max;
      if i=0 then ep{i}= pzero + (1-pzero)*pdf('POISSON',i,lambda);
      else        ep{i}=         (1-pzero)*pdf('POISSON',i,lambda);
      c{i}=ifn(art=i,1,0);
   end;
run;
proc 
means data=zip noprint;
   var ep0 - ep&max c0-c&max;
   output out=ep(drop=_TYPE_ _FREQ_) mean(ep0-ep&max)=ep0-ep&max;
   output out=p(drop=_TYPE_ _FREQ_) mean(c0-c&max)=p0-p&max;
run;
proc 
transpose data=ep out=ep(rename=(col1=zip) drop=_NAME_);
run;
proc 
transpose data=p out=p(rename=(col1=p) drop=_NAME_);
run;
data zipprob;
   merge ep p;
   zipdiff=p-zip;
ofp
=_N_ -1;
   label zip='ZIP Probabilities'
         p='Relative Frequencies'
         zipdiff='Observed minus Predicted';
run;

ods html body='diagnostics2.htm' 
style=HTMLBlue;
proc sgplot data=zipprob;
   series x=
ofp
 y=zipdiff / lineattrs=(pattern=ShortDash  color=blue)
           markers markerattrs=(symbol=CircleFilled 
size=5px color=blue);
   refline 0/ axis=y;
   xaxis type=discrete;
run;
ods html close;


/* Fitting Zero inflated negative binomial*/

Proc genmod data=hw.hmw1;
 class health gender privins;
 model ofp=hosp health numchron gender school privins/dist=zinb link=log;
 zeromodel hosp health numchron gender school privins/link=logit;
run;

/*Residual analysis for ZINB model*/
proc genmod data= hw.hmw1;
  class health gender privins;
model ofp = hosp health numchron gender school privins /dist=zinb;
  zeromodel hosp health numchron gender school privins /link = logit;
   output out=zip predicted=pred pzero=pzero;
   ods output Modelfit=fit;
run;
proc means data=hw.hmw1
 noprint;
 var ofp;
output out=maxcount max=max N=N;
run;
data _null_;
  set maxcount;
  call symput('N',N);
  call symput('max',max);
run;
%let max=%sysfunc(strip(&max));

ods html body='diagnostics1.htm' 
style=HTMLBlue;
proc sgplot data=zipprob;
scatter x=ofp y=p /
           markerattrs=(symbol=CircleFilled size=5px 
color=blue);
   scatter x=ofp y=zip /markerattrs=(symbol=TriangleFilled 
size=5px color=red);
   xaxis type=discrete;
run;
ods html close;
data zip(drop= i);
   set zip;
   lambda=pred/(1-pzero);
   array ep{0:&max} ep0-ep&max;
   array c{0:&max} c0-c&max;
   do i = 0 to &max;
      if i=0 then ep{i}= pzero + (1-pzero)*pdf('POISSON',i,lambda);
      else        ep{i}=         (1-pzero)*pdf('POISSON',i,lambda);
      c{i}=ifn(art=i,1,0);
   end;
run;
proc 
means data=zip noprint;
   var ep0 - ep&max c0-c&max;
   output out=ep(drop=_TYPE_ _FREQ_) mean(ep0-ep&max)=ep0-ep&max;
   output out=p(drop=_TYPE_ _FREQ_) mean(c0-c&max)=p0-p&max;
run;
proc 
transpose data=ep out=ep(rename=(col1=zip) drop=_NAME_);
run;
proc 
transpose data=p out=p(rename=(col1=p) drop=_NAME_);
run;
data zipprob;
   merge ep p;
   zipdiff=p-zip;
ofp
=_N_ -1;
   label zip='ZIP Probabilities'
         p='Relative Frequencies'
         zipdiff='Observed minus Predicted';
run;

ods html body='diagnostics2.htm' 
style=HTMLBlue;
proc sgplot data=zipprob;
   series x=
ofp
 y=zipdiff / lineattrs=(pattern=ShortDash  color=blue)
           markers markerattrs=(symbol=CircleFilled 
size=5px color=blue);
   refline 0/ axis=y;
   xaxis type=discrete;
run;
ods html close;
