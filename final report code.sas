/*Input data after cleaned*/
LIBNAME project 'C:\Users\qxz171530\Desktop\SAS\google-play-store-apps';

PROC IMPORT OUT=project.google 
            DATAFILE= "C:\Users\qxz171530\Desktop\SAS\google-play-store-apps\apps.csv" 
            DBMS=CSV REPLACE;
		GETNAMES=YES;
		DATAROW=2;
RUN;
data google; 
 set project.google;
run; 

/*proc corr data=google;
 var Rating Reviews Size Installs Recency;
 title 'correlation of numeric variables';
 run;
this method does not include correlation matrix*/
proc corr data=google plots(maxpoints=none)=matrix;
 var Rating Reviews Size Installs Price Recency Android_Ver;
 title 'correlation matrix';
run;

/*looks good*/
proc sgplot data=google;
	hbox Rating / category=Category_new;
	title 'report of rating';
run;

proc sgplot data= google;
 histogram Rating / binstart = 1 binwidth = 1; 
 density Rating / type = kernel; 
 density Rating / type = normal;
 title 'distribution report of rating';
run;

/* looks bad, need do something*/
proc sgplot data=google;
	hbox Reviews / category=Category_new;
	title 'Report of reviews';
run;

/*Create the log installs, looks much better*/
data google;
set google;
if Reviews = 0 then Reviews = 1;
log_Reviews = log(Reviews);
run;

proc sgplot data=google;
	hbox log_Reviews / category=Category_new;
	title 'Report of log reviews';
run;

proc sgplot data= google;
 histogram log_Reviews / binstart = 1 binwidth = 1 ; 
 density log_Reviews / type = kernel; 
 density log_Reviews / type = normal;
 title 'distribution report of log reviews';
run;

/*Size looks not bad, but can easily see that size is related with category*/
proc sgplot data=google;
	hbox Size / category=Category_new;
	title 'report of size';
run;

proc sgplot data= google;
 histogram Size / binstart = 1 binwidth = 1 ; 
 density Size / type = kernel; 
 density Size / type = normal;
 title 'distribution report of size';
run;

/*looks bad*/
proc sgplot data=google;
	hbox Installs / category=Category_new;
	title 'report of installs';
run;

/*use log installs, with log installs looks better*/
data google;
set google;
if Installs = 0 then Installs = 1;
log_Installs = log(Installs);
run;

proc sgplot data=google;
	hbox log_Installs / category=Category_new;
	title 'report of log installs';
run;

proc sgplot data= google;
 histogram log_Installs / binstart = 1 binwidth = 1 ; 
 density log_Installs / type = kernel; 
 density log_Installs / type = normal;
 title 'distribution report of installs';
run;

/*looks fine*/
proc sgplot data=google;
	hbox Recency / category=Category_new;
	title 'report of recency';
run;

proc sgplot data= google;
 histogram Recency / binstart = 10 binwidth = 1 ; 
 density Recency / type = kernel; 
 density Recency / type = normal;
 title 'distribution report of recency';
run;

data google;
set google;
if Android_Ver < 5 then Android_Ver = 0;
if Android_Ver > 4 then Android_Ver = 1;
run;

/*plot of price*/
proc sgplot data=google;
	hbox Price / category = Category_new;
	where Price > 0;
	title 'Report of Price';
run;

/*price have outlier so remove it*/
proc glm data = google;
 model Price =Rating Size Installs_new Reviews Recency/ solution;
 output out = regdata cookd =cookd student =sresiduals;
run;

/*influtial points*/
proc print data = regdata;
 var _ALL_;
 where Cookd > 4 / 9660;
run;

data app_new;
set regdata;
if Cookd > 4 / 9660 then delete;
run;

data app_new(drop = cookd sresiduals);
set app_new;
run;

/*plot after outlier*/
proc sgplot data=app_new;
	hbox Price / category = Category_new;
	where Price > 0;
	title 'Report of Price After Drop Outlier';
run;

/*sort data then check correlation by category*/
proc sort data = google;
by Category_new;
run;

proc corr data=google plots(maxpoints=none)=matrix;
 var Rating log_Reviews Size log_Installs Price Recency;
 by Category_new;
 run;

 /*create a new variable*/
data app_new;
 set app_new;
 Price_2 = Price*Price;
run;

/*Model 2-2 Non-linear Regression - SalesUnit over AdvertisingSpending and its power 2*/
proc reg data = app_new;
 model Rating = Price Price_2;
 where Type = 'Paid';
 title 'Non-linear Regression - Rating over Price and its power 2';
run;
/*price_2 not significant so won't add it, drop it*/
data app_new;
set app_new;
drop Price_2;
run;

/*split data into train and tesy*/
proc surveyselect data=app_new out=app_new_sampled outall samprate=0.8 seed=2;
run;

data app_new_train app_new_test;
 set app_new_sampled;
 if selected then output app_new_train;
 else output app_new_test;
run;

/*seperate with type*/

data app_free;
set app_new;
where Type = 'Free';
drop Type Price;
run;

data app_paid;
set app_new;
where Type = 'Paid';
drop Type;
run;




/*Rating as dependent variable*/

/*linear regression*/
proc glm data = app_new;
 class Category_new(ref ='GAME') Android_Ver(ref = '0') Content_Rating(ref = 'Everyone') Type(ref = 'Free') update(ref = '0');
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type/solution;
 title'linear regression';
run;

/*Forward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_new testdata=app_new_test  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
  /selection=forward(select=cp) hierarchy=single showpvalues;
 performance buildsscp=incremental;
 title 'Forward Selection based on Mallows Cp selection Criteria';
run;

/*Backward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_new testdata=app_new_test  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=backward(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Backward Selection based on Mallows Cp selection Criteria';
run;

/* stepwise selection with Mallows' cp as criteria */
proc glmselect data=app_new testdata=app_new_test  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=stepwise(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Stepwise Selection based on Mallows Cp selection Criteria';
run;

/* forward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=forward(select=cv) hierarchy=single cvmethod=random(10)showpvalues ; /*cvmethod with 10 folds*/
 performance buildsscp=incremental;
title'forward selection with cross validation as criteria with 10-folds';
run;

/* backward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=backward(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'backward selection with cross validation as criteria with 10-folds';
run;

/* stepwise selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Category_new Content_Rating Recency Android_Ver update|Type @2
 /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'stepwise selection with cross validation as criteria with 10-folds ';
run;

/*LASSO regression with Cross-Validation as criteria */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=lasso(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title 'LASSO regression with Cross_Validation as criteria';
run;

/* Elasticnet regression with Cross_Validation as criteria */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
 model Rating = Category_new|Size Category_new|log_installs log_Reviews Content_Rating Recency Android_Ver update|Type @2
 /selection=elasticnet(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues ; /*stop = none, don't stop when find the best lambda*/
 performance buildsscp=incremental;
 title 'Elasticnet regression with Cross_Validation as criteria';
run;




/*log_installs as dependent variable*/

/*linear regression*/
proc glm data = app_new;
 class Category_new(ref ='GAME') Android_Ver(ref = '0') Content_Rating(ref = 'Everyone') Type(ref = 'Free') update(ref = '0');
 model log_Installs = Category_new|Size Category_new|Rating Content_Rating Recency Android_Ver update|Type Price/solution;
 title'linear regression';
run;

/*Forward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_new testdata=app_new_test plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Content_Rating Recency Android_Ver update|Type Price @2
  /selection=forward(select=cp) hierarchy=single showpvalues;
 performance buildsscp=incremental;
 title 'Forward Selection based on Mallows Cp selection Criteria';
run;

/*Backward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_new testdata=app_new_test  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=backward(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Backward Selection based on Mallows Cp selection Criteria';
run;

/* stepwise selection with Mallows' cp as criteria */
proc glmselect data=app_new testdata=app_new_test  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=stepwise(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Stepwise Selection based on Mallows Cp selection Criteria';
run;

/* forward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=forward(select=cv) hierarchy=single cvmethod=random(10)showpvalues ; /*cvmethod with 10 folds*/
 performance buildsscp=incremental;
title'forward selection with cross validation as criteria with 10-folds';
run;

/* backward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=backward(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'backward selection with cross validation as criteria with 10-folds';
run;

/* stepwise selection with cross validation as criteria with 10-folds */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'stepwise selection with cross validation as criteria with 10-folds ';
run;

/*LASSO regression with Cross-Validation as criteria */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=lasso(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title 'LASSO regression with Cross_Validation as criteria';
run;

/* Elasticnet regression with Cross_Validation as criteria */
proc glmselect data=app_new testdata=app_new_test seed = 2  plots=all;
 class Category_new(split) Installs_new(split) Android_Ver(split) Content_Rating(split) Type(split) update(split);
  model log_Installs = Category_new|Size Category_new|Rating Category_new Content_Rating Recency Android_Ver update|Type Price @2
 /selection=elasticnet(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues ; /*stop = none, don't stop when find the best lambda*/
 performance buildsscp=incremental;
 title 'Elasticnet regression with Cross_Validation as criteria';
run;



/*Price as dependent variable*/

/*linear regression*/
proc glm data = app_paid;
 class Category_new(ref ='GAME') Android_Ver(ref = '0') Content_Rating(ref = 'Everyone') update(ref = '0');
 model Price = Category_new|Size Category_new|Rating Category_new|log_Reviews log_Installs|Category_new Content_Rating Recency Android_Ver update/solution;
 title'linear regression';
run;

/*Forward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
  /selection=forward(select=cp) hierarchy=single showpvalues;
 performance buildsscp=incremental;
 title 'Forward Selection based on Mallows Cp selection Criteria';
run;

/*Backward Selection based on Mallows Cp selection Criteria*/
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=backward(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Backward Selection based on Mallows Cp selection Criteria';
run;

/* stepwise selection with Mallows' cp as criteria */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=stepwise(select=cp) hierarchy=single showpvalues ;
 performance buildsscp=incremental;
 title 'Stepwise Selection based on Mallows Cp selection Criteria';
run;

/* forward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=forward(select=cv) hierarchy=single cvmethod=random(10)showpvalues ; /*cvmethod with 10 folds*/
 performance buildsscp=incremental;
title'forward selection with cross validation as criteria with 10-folds';
run;

/* backward selection with cross validation as criteria with 10-folds */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=backward(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'backward selection with cross validation as criteria with 10-folds';
run;

/* stepwise selection with cross validation as criteria with 10-folds */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=stepwise(select=cv) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title'stepwise selection with cross validation as criteria with 10-folds ';
run;

/*LASSO regression with Cross-Validation as criteria */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=lasso(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues;
 performance buildsscp=incremental;
 title 'LASSO regression with Cross_Validation as criteria';
run;

/* Elasticnet regression with Cross_Validation as criteria */
proc glmselect data=app_paid plots=all;
 class Category_new Installs_new Android_Ver Content_Rating update;
  model Price = Category_new|Size Category_new|Rating log_Reviews|Category_new log_Installs|Category_new Content_Rating Recency Android_Ver update @2
 /selection=elasticnet(choose=cv stop=none) hierarchy=single cvmethod=random(10) showpvalues ; /*stop = none, don't stop when find the best lambda*/
 performance buildsscp=incremental;
 title 'Elasticnet regression with Cross_Validation as criteria';
run;








/*reform rating as category to run the model*/

/*change rating into 1-5*/
data app_cate;
set google;
rate=int(Rating/(10**(length(input(Rating,$1.0))-1)));
run;

/* OLS for Y > 0: Here we run a regression  where only paid apps have been considered. We truncated data to remove all free apps */
proc glm data=app_cate;
 class Category_new(ref = 'GAME') Rate(ref = '1') Content_Rating(ref = 'Everyone') Android_Ver(ref = '1') update(ref = '1');
 model Rate = Category_new Size Installs_new Price Android_ver log_Reviews Recency Content_Rating category_new|Size category_new|Price category_new|log_Installs update/ solution;
 where Price > 0;
 run;

/*free VS paid*/
data app_free;
set app_cate;
where Type = 'Free';
drop Type Price;
run;

data app_paid;
set app_cate;
where Type = 'Paid';
drop Type;
run;

/*ordered probit and logit on app_paid*/
proc logistic data=app_paid;
 class Category_new(ref = 'GAME') Rate(ref = '1') Content_Rating(ref = 'Everyone') Android_Ver(ref = '1') update(ref = '1') Installs_new(ref = '0');
  model Rate = Category_new Size Installs_new Android_ver log_Reviews Recency Content_Rating category_new|Size category_new|Price category_new|log_Installs update
/link = probit; 
run;

proc logistic data=app_paid;
 class Category_new(ref = 'GAME') Rate(ref = '1') Content_Rating(ref = 'Everyone') Android_Ver(ref = '1') update(ref = '1') Installs_new(ref = '0');
  model Rate = Category_new Size Installs_new Android_ver log_Reviews Recency Content_Rating category_new|Size category_new|Price category_new|log_Installs update
/link = logit; 
run;

/*ordered probit and logit on app_free*/
proc logistic data=app_free;
 class Category_new(ref = 'GAME') Rate(ref = '1') Content_Rating(ref = 'Everyone') Android_Ver(ref = '1') update(ref = '1') Installs_new(ref = '0');
  model Rate = Category_new Size Installs_new Android_ver log_Reviews Recency Content_Rating category_new|Size category_new|log_Installs update
/link = probit; 
run;

proc logistic data=app_free;
 class Category_new(ref = 'GAME') Rate(ref = '1') Content_Rating(ref = 'Everyone') Android_Ver(ref = '1') update(ref = '1') Installs_new(ref = '0');
  model Rate = Category_new Size Installs_new Android_ver log_Reviews Recency Content_Rating category_new|Size category_new|log_Installs update
/link = logit; 
run;
