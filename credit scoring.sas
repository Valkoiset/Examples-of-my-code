 
/* Deep analysis of customers' data as part of major course task of my uni programme.
   The aim of the project was to identify customers' creditworthiness based on the certain data about their marital
   status, income, propensity to consume etc. As a main tool used for modelling credit risk - the vintage analysis which
   determines the probability an account will default over the next 12 months */

%let Type = css;
%let Type = ins;

/*libname inlib 'D:\DOCUMENTOS\SGH\Sem1-Basic and Advanced Programming - SAS\pd2_en\project\data\';*/
libname inlib 'C:\Users\Oleksandr\Desktop\SAS\pd2_en\project\data';
/*libname inlib 'd:\SGH\1 semester\Sas\pd2_en\project\data\';*/

/*--------------------------------------------------------------*/
/*                            Step 1                            */
/*                      Transactions data                       */
/*          Finding observations that have vintage 3            */
/*--------------------------------------------------------------*/

/* creating vintage3 */
data vin;
	set inlib.Transactions;
		/* intck provides intervals between dates */
        /* calculate the difference between the current period and the initial one*/
		seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.)); 
		/* find those accounts, that have missed more than 3 installments*/
		vin3=(due_installments>=3);
	output;
		/* clarify what b and c means */
		if status in ('B','C') and period<='200812' then do;
			/* calculate the difference between the current period and the last possible one  */
			n_steps=intck('month',input(period,yymmn6.),input('200812',yymmn6.));
				do i=1 to n_steps;
					period=put(intnx('month',input(period,yymmn6.),1,'end'),yymmn6.);
					seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
					output;
				end;
		end;
	/*select only installments*/
	where product="&Type";
	/* drop these variables because they were just used for a loop */
	drop n_steps i; 
run;

/* presort */
proc sort data=vin;
	by cid aid;
run;

/* obtaining max vin3 per application. Meaning that customer has at least 1 occurence of vin3 */
proc means data=vin noprint;
	by cid aid;
	var vin3;
	output out=vin3_dict(drop= _FREQ_ _TYPE_) max()=vin3;
run;

/*--------------------------------------------------------------*/
/*                            Step 2                            */
/*                       Production data                        */
/*                    Binding observations                      */
/*--------------------------------------------------------------*/
/* copying production table */
data product 																																																																																(drop=act12_n_arrears_days act3_n_arrears_days act9_n_arrears_days act_CIns_Acp5y act_CIns_All5y act_CMaxA_Due act_CMaxC_Days act_CMaxC_Due act_CMaxI_Due act_call_n_loan act_ccss_dueutl act_ccss_maxdue act_ccss_min_lninst act_ccss_min_pninst act_ccss_min_seniority act_ccss_n_loan act_ccss_utl act_cins_dueutl act_cins_maxdue act_cins_min_lninst act_cins_min_pninst act_cins_n_loan act_cins_n_loans_act act_cins_n_statB act_cus_active agr12_Max_CMaxA_Days agr12_Max_CMaxA_Due agr12_Max_CMaxC_Days agr12_Max_CMaxC_Due agr12_Max_CMaxI_Days agr12_Max_CMaxI_Due agr12_Mean_CMaxC_Days agr12_Mean_CMaxI_Due agr12_Min_CMaxA_Due agr12_Min_CMaxC_Due agr12_Min_CMaxI_Due agr3_Max_CMaxA_Days agr3_Max_CMaxA_Due agr3_Max_CMaxC_Days agr3_Max_CMaxI_Due agr3_Mean_CMaxA_Due agr3_Mean_CMaxI_Due agr3_Min_CMaxA_Due agr3_Min_CMaxC_Due agr3_Min_CMaxI_Due agr6_Max_CMaxA_Days agr6_Max_CMaxC_Days agr6_Max_CMaxI_Days agr6_Max_CMaxI_Due agr6_Mean_CMaxI_Due agr6_Min_CMaxA_Due agr6_Min_CMaxC_Due agr6_Min_CMaxI_Due agr9_Max_CMaxA_Days agr9_Max_CMaxC_Days agr9_Max_CMaxI_Days agr9_Max_CMaxI_Due agr9_Mean_CMaxI_Due agr9_Min_CMaxA_Due agr9_Min_CMaxC_Due agr9_Min_CMaxI_Due ags12_Max_CMaxA_Days ags12_Max_CMaxC_Days ags12_Max_CMaxI_Days ags12_Max_CMaxI_Due ags12_Mean_CMaxI_Due ags12_Min_CMaxA_Due ags12_Min_CMaxC_Due ags12_Min_CMaxI_Due ags3_Max_CMaxA_Days ags3_Max_CMaxC_Days ags3_Max_CMaxI_Due ags3_Mean_CMaxA_Due ags3_Mean_CMaxI_Due ags3_Min_CMaxA_Due ags3_Min_CMaxC_Due ags3_Min_CMaxI_Due ags6_Max_CMaxA_Days ags6_Max_CMaxC_Days ags6_Max_CMaxI_Days ags6_Max_CMaxI_Due ags6_Mean_CMaxI_Due ags6_Min_CMaxA_Due ags6_Min_CMaxC_Due ags6_Min_CMaxI_Due ags9_Max_CMaxA_Days ags9_Max_CMaxC_Days ags9_Max_CMaxI_Days ags9_Max_CMaxI_Due ags9_Min_CMaxA_Due ags9_Min_CMaxI_Due app_installment app_loan_amount app_number_of_children);
	set inlib.production;
run;

/* bining step */
PROC HPBIN DATA = product
			output = binned
				/*pseudo_quantile*/ WINSOR WINSORRATE=0.05
			/*create 2 bins for each variable*/
			numbin=2;
			input _ALL_;
			/* transferring cid and iad from input data to output */
			id cid aid; 
RUN;

/*--------------------------------------------------------------*/
/*                            Step 3                            */
/*                        Joining tables                        */
/*--------------------------------------------------------------*/

/* merging binned production and vin3 per customer */
proc sort data=binned;
	by cid aid;
run;
proc sort data=vin3_dict;
	by cid aid;
run;
data vin3_predict;
	merge binned (in=l) vin3_dict(in=r);
	/* the same as where in sql for joining tables */
	by cid aid; 
	if l and r;
	drop  aid cid;
run;

/*--------------------------------------------------------------*/
/*                            Step 4                            */
/*                         Joint table                          */
/*               Calculating Chi squared statistics             */
/*--------------------------------------------------------------*/
/* calculating chi-square statistics for all variables in vin3_predict */
/*can be used only for interval variables*/
proc freq data=vin3_predict ;
	tables vin3* _ALL_ /chisq ;
	ods output ChiSq=screen ;
run;

/*Look into why we use Cramer`s*/
/*screening only Cramer's*/
data screen_num_V;
	set screen;
	where statistic="Cramer's V";
run;


/* We include class variables*/
/*Sort tables*/
proc sort data=product(keep= cid aid app_char_:) out=prod_char;
	by cid aid;
run;
proc sort data=vin3_dict;
	by cid aid;
run;
/*joint tables keeping only common rows*/
data screen_char;
	merge vin3_dict (in=l) prod_char (in=r);
	by cid aid;
	if l and r then output;
run;

proc freq data=screen_char;
	tables vin3*app_char_: / chisq ;
	ods output ChiSq=screen_char_V(where= (statistic="Cramer's V")) ;
run;

/* Gathering everything together*/
data screen_all_V;
	set screen_num_V screen_char_V;
	drop statistic df prob;
	value=abs(value);
	if Value<1;
	Table=substr(table,find(table,"*")+1);
	/*putlog table;*/
	/*index= find(table,"BIN");*/
	/*putlog index;*/
	/*Table=substr(table,find(table,"BIN")+1);*/
	rename Table= Variable;
run;

/*--------------------------------------------------------------*/
/*                            Step 5                            */
/*                      Creating categories                     */
/*--------------------------------------------------------------*/

/*Assign categories*/
data screen_all_V ;
	set screen_all_V;
	if index(Variable,"app")>0 then cat="app";
		else if index(Variable,"act")>0 then cat="act";
		else if index(Variable,"ags")>0 then cat="ags";
		else if index(Variable,"agr")>0 then cat="agr";
run;

proc sort data=screen_all_V;
	by cat descending value;
run;

/* Select the top 5 variables*/
data screen_all_V_top5;
	set screen_all_V;
	by cat;
	retain i;
	if FIRST.cat then i=1;
		else  i=i+1;
	if i<6 then output;
	drop i ;
run;

/*Show variables*/
proc print data=screen_all_V_top5 noobs;
	by cat;
	var Value;
	id Variable;
run;

/*--------------------------------------------------------------*/
/*                            Step 6                            */
/*                       Separating graphs                      */
/*--------------------------------------------------------------*/


/*Steps:*/
/*1. Sort Production table in accordance with binned variables*/
/*2. Join Transaction table with Production table based on common accounts*/
/*3. Obtain sorted Transaction table for binned variables */
/*4. Build graphs*/

/*Preparing Transaction table*/
data vin_short;
	set vin;
	keep cid aid fin_period vin3 seniority;
run;
proc sort data=vin_short;
	by cid aid ;
run;


/*-----------------------------------------------------------------------------------------------------------------------------------*/
/***********************************************Insert a macro operator for next section**********************************************/                                     
/*-----------------------------------------------------------------------------------------------------------------------------------*/
%MACRO sortandprint(data=, output=, condition=);

	/* Dividing Production table by binned categories*/

	data prod;
		set &data;
		where &condition;
	run;
	proc sort data=prod;
		by cid aid;
	run;

	/*Joining Production and Transaction tables*/;
	data prod_sorted;
		merge vin_short (in=l) prod(in=r);
		by cid aid;
		if l and r then output;	
		keep fin_period vin3 seniority;
	run;

	/*--------------------------------------------------------------*/
	/*                            Step 7                            */
	/*                            Graphs                            */
	/*--------------------------------------------------------------*/
	proc means data=prod_sorted noprint nway;
		class fin_period seniority;
		var vin3;
		output out=vintagr(drop=_freq_ _type_) n()=production mean()=vin3;
		format vin3 nlpct12.2;
	run;

	proc means data=prod_sorted noprint nway;
		class fin_period;
		var vin3;
	output out=production(drop=_freq_ _type_) n()=production;
	where seniority=0;
	run;

	proc transpose data=vintagr out=vintage prefix=months_after_;
		by fin_period;
		var vin3;
		id seniority;
	run;

	data prod;
		set Vintagr;
		where seniority=0;
		keep production fin_period;
	run;

	data prod_vintage;
		merge prod vintage(drop=_NAME_);
		by fin_period;
	run;

	Data quarters;
		Set prod_vintage;
		Rownum=ceil(_n_/3);
	Run;

		/*	Output directory for graphs*/
	
/*	%let dir=d:\SGH\1 semester\Sas\Project\Output\;*/
 %let dir=C:\Users\Oleksandr\Desktop\SAS\myproject\graphs;

	proc sql;
		create table forecast as
		select Rownum as Quarters, avg(production) as Production, avg(months_after_3) as months_after_3, avg(months_after_4) as months_after_4, avg(months_after_5) as months_after_5, avg(months_after_6) as months_after_6, avg(months_after_7) as months_after_7, avg(months_after_8) as months_after_8, avg(months_after_9) as months_after_9, avg(months_after_10) as months_after_10, avg(months_after_11) as months_after_11, avg(months_after_12) as months_after_12
		from quarters
		group by Rownum;
	quit;

/*	proc forecast data=forecast lead=4 out=pred;*/
/*	   var months_after_12;*/
/*	run;*/

/*data predict(keep=Forecast);*/
/*	set pred(rename=(months_after_12=Forecast));*/
/*	run;*/



/*First graph data*/
	data asymptotic(RENAME = (months_after_3=months_3 months_after_4=months_4 months_after_5=months_5 
							months_after_6=months_6 months_after_7=months_7 months_after_8=months_8 
							months_after_9=months_9 months_after_10=months_10 months_after_11=months_11 
							months_after_12=months_12));
		set forecast;
		drop Production Forecast Quarters;
		format _all_ nlpct12.2;
	run;

	proc transpose data=asymptotic
	               out=asymptotic_transpose;
	run;

	




/*Second graph data*/
/*======================================================================*/
	/* FORECAST PROCEDURE*/

data forecast_sum;
	set forecast;
	if Quarters<=10 then
	v_6_3=months_after_6-months_after_3;
	if Quarters<=9 then
	v_9_6=months_after_9-months_after_6;
	if Quarters<=8 then
	v_12_9=months_after_12-months_after_9;
run;

	proc sql;
		create table forecast_mean  as
		select Quarters, Production, months_after_3, months_after_6,months_after_9,months_after_12, avg(v_6_3) as mean_6_3, avg(v_9_6)as mean_9_6, avg(v_12_9) as mean_12_9
		from forecast_sum;
	quit;


data forecast_final;
	set forecast_mean;
	if Quarters>=11 then
	months_after_6=months_after_3+mean_6_3;
	if Quarters>=10 then
	months_after_9=months_after_6+mean_9_6;
	if Quarters>=9 then
	months_after_12=months_after_9+mean_12_9;
	drop mean_6_3 mean_9_6 mean_12_9;
run;

data &output;
	set forecast_final;
	if Quarters>=10 then do;
	Forecast_6=months_after_6;
	end;
	if Quarters>=9 then do;
	Forecast_9=months_after_9;
	end;
	if Quarters>=8 then do;
	Forecast_12=months_after_12;
	end;
run;


/*======================================================================*

/*======================================================================*/
/*	/* FORECAST PROCEDURE WITH ARIMA*/*/
/*	/*Generating more data to allow arima to work and eliminating missing values */*/
/*data forecast2;*/
/*  set forecast forecast forecast forecast;*/
/*  run;*/
/*data forecast3;*/
/*  set forecast2 forecast2 forecast2 forecast2;*/
/*  run;*/
/*data forecast4;*/
/*  set forecast3;*/
/*  if months_after_12=. then delete;*/
/*run;*/
/*	*/
/**/
/*   proc arima data=forecast4;*/
/*      identify var=months_after_12(1,2) scan;   /* nlag=8;*/*/
/*      run;*/
/*      estimate p=1 q=(1)(8);*/
/*      run;*/
/*      /*outlier;*/
/*      run;*/*/
/*      forecast lead=4 out=forK;*/
/*      run;*/
/*   quit;*/
/**/
/*      data predict (keep = forecast);*/
/*	set forK;*/
/*	if _N_ > 128 then*/
/*	output;*/
/*run;*/
/*/*======================================================================*/*/
/**/
/*data &output;*/
/*	set forecast;*/
/*	if Quarters<8 then*/
/*		Forecast=.;*/
/*	if Quarters=8 then*/
/*	Forecast=months_after_12;*/
/*	if Quarters>=9 then*/
/*	set predict;*/
/*run;*/;





	/*--------------------------------------------------------------*/
	/*                            Graph                             */
	/*--------------------------------------------------------------*/

	ods listing close;
	goptions reset=all device=activex;
	ods powerPoint file='d:\SGH\1 semester\Sas\Project\Output\vintage.ppt' dpi=300;
	ods powerpoint layout=_null_;
	goptions reset=all device=activex;
	ods html path="&dir" body='asymptotic.html' style=statistical;

	/*Graph 1*/

	proc sgplot data=asymptotic_final;
		  title "Vintage 3+ when &condition and &Type - asymptotic";
		  series x=_NAME_ y=quarter_1/ LEGENDLABEL = '1 quarter' MARKERS LINEATTRS = (THICKNESS = 2);
		  series x=_NAME_ y=quarter_2/ LEGENDLABEL = '2 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_3/ LEGENDLABEL = '3 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_4/ LEGENDLABEL = '4 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_5/ LEGENDLABEL = '5 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_6/ LEGENDLABEL = '6 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;  
		  series x=_NAME_ y=quarter_7/ LEGENDLABEL = '7 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_8/ LEGENDLABEL = '8 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_9/ LEGENDLABEL = '9 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_10/ LEGENDLABEL = '10 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_11/ LEGENDLABEL = '11 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  series x=_NAME_ y=quarter_12/ LEGENDLABEL = '12 quarter' MARKERS LINEATTRS = (THICKNESS = 2);;
		  YAXIS LABEL = 'Percentage';
		  XAXIS LABEL = 'Period';
	run;




	ods html path="&dir" body='Vintage.html' style=statistical;

	/*Graph 2*/

	symbol1 i=join c=red line=1 v=dot h=0.5 w=2;
	symbol2 i=join c=green line=1 v=dot h=0.5 w=2;
	symbol3 i=join c=blue line=1  v=dot h=0.5 w=2;
	symbol4 i=join c=black line=1 v=dot h=0.5 w=2;
	symbol5 i=join c=brown line=1 v=dot h=0.5 w=2;
	symbol6 i=join c=cyan line=1 v=dot h=0.5 w=2;
	symbol7 i=join c=yellow line=1 v=dot h=0.5 w=2;
	symbol8 i=join c=blue line=2 v=dot h=0.5 w=2;
	symbol9 i=join c=green line=2 v=dot h=0.5 w=2;
	symbol10 i=join c=red line=2 v=dot h=0.5 w=2;

	LEGEND1
	FRAME
	LABEL=(FONT='Calibri' HEIGHT=10pt )
	VALUE=(FONT='Calibri' HEIGHT=10pt )
	;
	LEGEND2
	FRAME
	LABEL=(FONT='Calibri' HEIGHT=10pt )
	VALUE=(FONT='Calibri' HEIGHT=10pt )
	;

	PROC GBARLINE DATA=&output;
		title "Vintage 3+ when &condition and &Type";
		BAR	 Quarters / discrete
	    SUMVAR=production
		COUTLINE=LTGRAY
		LEGEND=LEGEND;
		PLOT / SUMVAR=months_after_3 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=months_after_6 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=months_after_9 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=months_after_12 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=Forecast_6 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=Forecast_9 TYPE=mean LEGEND=LEGEND2;
		PLOT / SUMVAR=Forecast_12 TYPE=mean LEGEND=LEGEND2;
		label Quarters='Quarters' Production='Production' 
		months_after_3='3'
		months_after_6='6'
		months_after_9='9'
		months_after_12='12'
		Forecast_6='Forecast_6'
		Forecast_9='Forecast_9'
		Forecast_12='Forecast_12'
		;
		format production comma14.1 months: percent12.1;
	RUN;
	quit;

	data asymptotic_final; 
		set asymptotic_transpose(RENAME = (COL1=quarter_1 COL2=quarter_2 COL3=quarter_3 COL4=quarter_4 	
											COL5=quarter_5 COL6=quarter_6 COL7=quarter_7 COL8=quarter_8 
											COL9=quarter_9 COL10=quarter_10 COL11=quarter_11 COL12=quarter_12));
	run;

	ods html close;
	ods powerpoint close;
	ods listing;
	goptions reset=all device=win;

%MEND sortandprint;

/*-------------------------------------------------------------------------------------------------------------------------------*/
/******************************************************End of macro operator******************************************************/
/*-------------------------------------------------------------------------------------------------------------------------------*/





/*-----------------------------------------------------------------------------------------------------------------*/
/*                                                 INS                                                             */
/*-----------------------------------------------------------------------------------------------------------------*/

/*                                            ACT caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*1) BIN_act_call_cc*/
%sortandprint(output=act_1_1, data=Binned, condition=BIN_act_call_cc=1); /*report*/
%sortandprint(output=act_1_2, data=Binned, condition=BIN_act_call_cc=2);
/*2) BIN_act_loaninc*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_act_loaninc=1); /*report +*/
%sortandprint(output=act_2_2, data=Binned, condition=BIN_act_loaninc=2);
/*3) BIN_act_cins_cc*/
%sortandprint(output=act_4_1, data=Binned, condition=BIN_act_cins_cc=1);
%sortandprint(output=act_4_2, data=Binned, condition=BIN_act_cins_cc=2);
%sortandprint(output=act_3_3, data=Binned, condition=BIN_act_cins_cc=0);
/*4) BIN_act_cins_utl*/
%sortandprint(output=act_4_1, data=Binned, condition=BIN_act_cins_utl=1);
%sortandprint(output=act_4_2, data=Binned, condition=BIN_act_cins_utl=2);
%sortandprint(output=act_4_3, data=Binned, condition=BIN_act_cins_utl=0);
/*5) BIN_act_CMaxI_Days*/
%sortandprint(output=act_4_1, data=Binned, condition=BIN_act_CMaxI_Days=1);
%sortandprint(output=act_4_2, data=Binned, condition=BIN_act_CMaxI_Days=2);
%sortandprint(output=act_4_3, data=Binned, condition=BIN_act_CMaxI_Days=0);

/*proc univariate data=product;*/
/*var act_cins_dueutl;*/
/*histogram act_cins_dueutl / normal;*/
/*run;*/

/*                                            AGR caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*1) BIN_agr3_Mean_CMaxI_Due*/
%sortandprint(output=act_1_1, data=Binned, condition=BIN_agr3_Mean_CMaxI_Due=1);
%sortandprint(output=act_1_2, data=Binned, condition=BIN_agr3_Mean_CMaxI_Due=2);
%sortandprint(output=act_1_3, data=Binned, condition=BIN_agr3_Mean_CMaxI_Due=0);
/*2) BIN_agr3_Max_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_agr3_Max_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_agr3_Max_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_agr3_Max_CMaxI_Due=0);
/*3) BIN_agr3_Min_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_agr3_Min_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_agr3_Min_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_agr3_Min_CMaxI_Due=0);
/*4) BIN_agr9_Mean_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_agr9_Mean_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_agr9_Mean_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_agr9_Mean_CMaxI_Due=0);
/*5) BIN_agr3_Mean_CMaxI_Days*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_agr3_Mean_CMaxI_Days=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_agr3_Mean_CMaxI_Days=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_agr3_Mean_CMaxI_Days=0); /*report + EXPLAIN:
Mean calculated on last 3 month on unmissing maximum customer days for installment product. Difference between due
date and delayed installments*/

/*                                            AGS caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*1) BIN_ags6_Mean_CMaxI_Due*/
%sortandprint(output=act_1_1, data=Binned, condition=BIN_ags6_Mean_CMaxI_Due=1); /*report +*/
%sortandprint(output=act_1_2, data=Binned, condition=BIN_ags6_Mean_CMaxI_Due=2);
%sortandprint(output=act_1_3, data=Binned, condition=BIN_ags6_Mean_CMaxI_Due=0);
/*2) BIN_ags9_Mean_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_ags9_Mean_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_ags9_Mean_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_ags9_Mean_CMaxI_Due=0);
/*3) BIN_ags3_Mean_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_ags3_Mean_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_ags3_Mean_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_ags3_Mean_CMaxI_Due=0);
/*4) BIN_ags3_Max_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_ags3_Max_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_ags3_Max_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_ags3_Max_CMaxI_Due=0);
/*5) BIN_ags3_Min_CMaxI_Due*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_ags3_Min_CMaxI_Due=1);
%sortandprint(output=act_2_2, data=Binned, condition=BIN_ags3_Min_CMaxI_Due=2);
%sortandprint(output=act_2_3, data=Binned, condition=BIN_ags3_Min_CMaxI_Due=0);

/*                                            APP caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*1) BIN_app_loan_amount*/
%sortandprint(output=app_3_1, data=Binned, condition=BIN_app_loan_amount=1); /*report*/
%sortandprint(output=app_3_2, data=Binned, condition=BIN_app_loan_amount=2);
/*2)  BIN_app_installment*/
%sortandprint(output=app_3_1, data=Binned, condition=BIN_app_installment=1);
%sortandprint(output=app_3_2, data=Binned, condition=BIN_app_installment=2);
/*3) app_char_job_code*/
%sortandprint(output=app_1_1, data=Screen_char, condition=app_char_job_code='Retired');
%sortandprint(output=app_1_2, data=Screen_char, condition=app_char_job_code='Owner company'); /*report ?*/
%sortandprint(output=app_1_3, data=Screen_char, condition=app_char_job_code='Permanent'); /*report*/
%sortandprint(output=app_1_3, data=Screen_char, condition=app_char_job_code='Contract'); 
/*4) app_char_cars*/
%sortandprint(output=app_1_1, data=Screen_char, condition=app_char_cars='Owners');
%sortandprint(output=app_1_2, data=Screen_char, condition=app_char_cars='no');
/*5) app_char_marital_status*/ 
%sortandprint(output=app_2_1, data=Screen_char, condition=app_char_marital_status='Maried'); /*report*/
%sortandprint(output=app_2_2, data=Screen_char, condition=app_char_marital_status='Widowed'); /*report*/
%sortandprint(output=app_2_3, data=Screen_char, condition=app_char_marital_status='Divorced'); /*report*/
%sortandprint(output=app_3_1, data=Screen_char, condition=app_char_gender='Female'); /*report*/
%sortandprint(output=app_3_2, data=Screen_char, condition=app_char_gender='Male'); 


/*-----------------------------------------------------------------------------------------------------------------*/
/*                                                 CSS                                                             */
/*-----------------------------------------------------------------------------------------------------------------*/

/*                                            ACT caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*BIN_act_ccss_n_statC*/
%sortandprint(output=act_1_1, data=Binned, condition=BIN_act_ccss_n_statC=1); /*report +*/
%sortandprint(output=act_1_2, data=Binned, condition=BIN_act_ccss_n_statC=2);
%sortandprint(output=act_1_3, data=Binned, condition=BIN_act_ccss_n_statC=0);
/*BIN_act_ccss_n_loans_hist*/
%sortandprint(output=act_2_1, data=Binned, condition=BIN_act_ccss_n_loans_hist=1); /*report*/
%sortandprint(output=act_2_2, data=Binned, condition=BIN_act_ccss_n_loans_hist=2); 
%sortandprint(output=act_2_3, data=Binned, condition=BIN_act_ccss_n_loans_hist=0);
/*BIN_act_CCss_Acp*/
%sortandprint(output=act_3_1, data=Binned, condition=BIN_act_CCss_Acp=1); /*report*/
%sortandprint(output=act_3_2, data=Binned, condition=BIN_act_CCss_Acp=2);
%sortandprint(output=act_3_3, data=Binned, condition=BIN_act_CCss_Acp=0);
/*BIN_act_CCss_Acp5y*/
%sortandprint(output=act_4_1, data=Binned, condition=BIN_act_CCss_Acp5y=1);
%sortandprint(output=act_4_2, data=Binned, condition=BIN_act_CCss_Acp5y=2);
%sortandprint(output=act_4_3, data=Binned, condition=BIN_act_CCss_Acp5y=0);
/*BIN_act_ccss_n_loans_act*/
%sortandprint(output=act_5_1, data=Binned, condition=BIN_act_ccss_n_loans_act=1);
%sortandprint(output=act_5_2, data=Binned, condition=BIN_act_ccss_n_loans_act=2);
%sortandprint(output=act_5_3, data=Binned, condition=BIN_act_ccss_n_loans_act=0);

/*                                            AGR caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*BIN_agr12_Min_CMaxC_Days*/
%sortandprint(output=agr_1_1, data=Binned, condition=BIN_agr12_Min_CMaxC_Days=1);
%sortandprint(output=agr_1_2, data=Binned, condition=BIN_agr12_Min_CMaxC_Days=2); /*report +*/
%sortandprint(output=agr_1_3, data=Binned, condition=BIN_agr12_Min_CMaxC_Days=0);
/*BIN_agr9_Min_CMaxC_Days*/
%sortandprint(output=agr_2_1, data=Binned, condition=BIN_agr9_Min_CMaxC_Days=1);
%sortandprint(output=agr_2_2, data=Binned, condition=BIN_agr9_Min_CMaxC_Days=2); /*report*/
%sortandprint(output=agr_2_3, data=Binned, condition=BIN_agr9_Min_CMaxC_Days=0);
/*BIN_agr9_Mean_CMaxC_Days*/
%sortandprint(output=agr_3_1, data=Binned, condition=BIN_agr9_Mean_CMaxC_Days=1);
%sortandprint(output=agr_3_2, data=Binned, condition=BIN_agr9_Mean_CMaxC_Days=2); /*report*/
%sortandprint(output=agr_3_3, data=Binned, condition=BIN_agr9_Mean_CMaxC_Days=0);
/*BIN_agr6_Min_CMaxC_Days*/
%sortandprint(output=agr_4_1, data=Binned, condition=BIN_agr6_Min_CMaxC_Days=1);
%sortandprint(output=agr_4_2, data=Binned, condition=BIN_agr6_Min_CMaxC_Days=2);
%sortandprint(output=agr_4_3, data=Binned, condition=BIN_agr6_Min_CMaxC_Days=0);
/*BIN_agr6_Mean_CMaxC_Days*/
%sortandprint(output=agr_5_1, data=Binned, condition=BIN_agr6_Mean_CMaxC_Days=1);
%sortandprint(output=agr_5_2, data=Binned, condition=BIN_agr6_Mean_CMaxC_Days=2);
%sortandprint(output=agr_5_3, data=Binned, condition=BIN_agr6_Mean_CMaxC_Days=0);

/*                                            AGS caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*BIN_ags12_Mean_CMaxC_Days*/
%sortandprint(output=ags_1_1, data=Binned, condition=BIN_ags12_Mean_CMaxC_Days=1);
%sortandprint(output=ags_1_2, data=Binned, condition=BIN_ags12_Mean_CMaxC_Days=2); /*report +*/
%sortandprint(output=ags_1_3, data=Binned, condition=BIN_ags12_Mean_CMaxC_Days=0);
/*BIN_ags9_Min_CMaxC_Days */
%sortandprint(output=ags_2_1, data=Binned, condition=BIN_ags9_Min_CMaxC_Days=1);
%sortandprint(output=ags_2_2, data=Binned, condition=BIN_ags9_Min_CMaxC_Days=2); /*report*/
%sortandprint(output=ags_2_3, data=Binned, condition=BIN_ags9_Min_CMaxC_Days=0);
/*BIN_ags9_Mean_CMaxC_Days */
%sortandprint(output=ags_3_1, data=Binned, condition=BIN_ags9_Mean_CMaxC_Days=1);
%sortandprint(output=ags_3_2, data=Binned, condition=BIN_ags9_Mean_CMaxC_Days=2); /*report*/
%sortandprint(output=ags_3_3, data=Binned, condition=BIN_ags9_Mean_CMaxC_Days=0);
/*BIN_ags6_Mean_CMaxC_Days */
%sortandprint(output=ags_4_1, data=Binned, condition=BIN_ags6_Mean_CMaxC_Days=1);
%sortandprint(output=ags_4_2, data=Binned, condition=BIN_ags6_Mean_CMaxC_Days=2);
%sortandprint(output=ags_4_3, data=Binned, condition=BIN_ags6_Mean_CMaxC_Days=0);
/*BIN_ags6_Min_CMaxC_Days*/
%sortandprint(output=ags_5_1, data=Binned, condition=BIN_ags6_Min_CMaxC_Days=1);
%sortandprint(output=ags_5_2, data=Binned, condition=BIN_ags6_Min_CMaxC_Days=2);
%sortandprint(output=ags_5_3, data=Binned, condition=BIN_ags6_Min_CMaxC_Days=0);

/*                                            APP caterory                                                         */
/*-----------------------------------------------------------------------------------------------------------------*/
/*app_char_job_code*/
%sortandprint(output=app_1_1, data=Screen_char, condition=app_char_job_code='Retired'); /*report*/
%sortandprint(output=app_1_2, data=Screen_char, condition=app_char_job_code='Owner company');
%sortandprint(output=app_1_3, data=Screen_char, condition=app_char_job_code='Permanent');
%sortandprint(output=app_1_3, data=Screen_char, condition=app_char_job_code='Contract');
/*app_char_marital_status */
%sortandprint(output=app_2_1, data=Screen_char, condition=app_char_marital_status='Maried');
%sortandprint(output=app_2_2, data=Screen_char, condition=app_char_marital_status='Widowed'); /*report*/
%sortandprint(output=app_2_3, data=Screen_char, condition=app_char_marital_status='Divorced');
/*app_char_gender*/
%sortandprint(output=app_3_1, data=Screen_char, condition=app_char_gender='Female'); /*report*/
%sortandprint(output=app_3_2, data=Screen_char, condition=app_char_gender='Male'); /*report*/
/*app_char_home_status*/
%sortandprint(output=app_4_1, data=Screen_char, condition=app_char_home_status='Rental');
%sortandprint(output=app_4_2, data=Screen_char, condition=app_char_home_status='Owner'); /*report*/
/*app_char_city */
%sortandprint(output=app_5_1, data=Screen_char, condition=app_char_city='Large');
%sortandprint(output=app_5_3, data=Screen_char, condition=app_char_city='Big'); /*report*/
%sortandprint(output=app_5_2, data=Screen_char, condition=app_char_city='Medium'); /*report*/
%sortandprint(output=app_5_3, data=Screen_char, condition=app_char_city='Small'); /*report*/


/*main vintage/asymptotic ins*/
ods excel file="C:\Users\Oleksandr\Desktop\SAS\myproject\graphs\graphs.xlsx";
%sortandprint(output=act_1_1, data=product, condition=);
ods excel close;

%sortandprint(output=act_1_1, data=Binned, condition=BIN_act_age=1); /*report +*/
%sortandprint(output=act_1_1, data=Binned, condition=BIN_act_age=2); /*report +*/
