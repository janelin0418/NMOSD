/*================================================================================================**
Aim     : NMOSD paper
Analyst : Ryan Chen
**================================================================================================*/
libname CGRD_K "D:\CGRD\ShortForm";
libname imf    "C:\SAS\Analysis data\NMOSD_paper";

%Let ID = IDCODE; 

/*多加一年去算incident*/
%Macro Retri_by_Dx(CD_CODES=, DD_CODES=, DISEASE=, OUT=dx_records);
	data &out (Keep=&ID MED_d Out_d Source DXKD1-DXKD5);
		set CGRD_K.OP_DIAG (keep=&ID IPDAT DSSID rename=(IPDAT=temp_d DSSID=DXKD1) where=( &CD_codes ) in=_a)
			CGRD_K.ER_DIAG (keep=&ID IPDAT DSSID rename=(IPDAT=temp_d DSSID=DXKD1) where=( &CD_codes ) in=_b)
			CGRD_K.IP_ICD  (keep=&ID ADMDAT DGDAT DXKD1-DXKD5 rename=(ADMDAT=temp_d) 
            where=( DXKD1 in: (&DD_codes) or DXKD2 in: (&DD_codes) or DXKD3 in: (&DD_codes) or DXKD4 in: (&DD_codes) or DXKD5 in: (&DD_codes)));
		if _a then Source = "CD";else if _b then Source = "ER"; else Source = "DD";
		MED_d = input(temp_d,yymmdd8.);
		Out_d = input(DGDAT ,yymmdd8.);
		format MED_D Out_d yymmdd10.;
	run;
	Proc sort data=&out         ; by &ID Source MED_D descending Out_d; run;
	Proc sort data=&out nodupkey; by &ID Source MED_D ; run; 
%Mend Retri_by_Dx;
%Retri_by_Dx(
CD_codes = DXKD1 in: ("3410" "G360" "340" "G35"),
DD_codes = "3410" "G360" "340" "G35", 
Disease  = dx,
out      = dx); *2440;

data imf.NMOSD_dx imf.MS_dx;
	set dx (where=( 2005 <= year(MED_d) <= 2021 ));
	if DXKD1 in: ("3410" "G360") or DXKD2 in: ("3410" "G360") or DXKD3 in: ("3410" "G360") or DXKD4 in: ("3410" "G360") or 
	   DXKD5 in: ("3410" "G360") then NMOSD=1;
	if DXKD1 in: ("340" "G35") or DXKD2 in: ("340" "G35") or DXKD3 in: ("340" "G35") or DXKD4 in: ("340" "G35") or 
	   DXKD5 in: ("340" "G35") then MS=1;
	if NMOSD=1 then output imf.NMOSD_dx;
	if MS=1    then output imf.MS_dx;
run;
Proc sort data=imf.NMOSD_dx nodupkey out=x ; by &ID; run;*563;
Proc sort data=imf.MS_dx    nodupkey out=x ; by &ID; run;*1748;

proc sort data=imf.NMOSD_dx; by &ID;
proc sort data=imf.MS_dx(keep=&ID) nodupkey out=tempid; by &ID;
data NMOSD_dx_without_MS NMOSD_dx_with_MS;
    merge imf.NMOSD_dx(in=_a) tempid(in=_b); by &ID;
	if _a;
    if _b = 0 then output NMOSD_dx_without_MS;
		else output NMOSD_dx_with_MS;
run;
Proc sort data=NMOSD_dx_without_MS nodupkey out=x ; by &ID; run;*299;

PROC SQL;
CREATE TABLE MS_dx2 AS 
	SELECT a.*
	FROM imf.MS_dx A
	LEFT JOIN (select distinct &ID from NMOSD_ca_2) B ON A.&ID=B.&ID 
	where b.&ID is null;
QUIT;
Proc sort data=MS_dx2 nodupkey out=x ; by &ID; run;*1490;

/*===   NMOSD 定義為2006-2021住院或門診2年內3次   ===*/
%Macro define_case(IN, OUT);
	****門診2年內3次;
	Proc sort data= &IN (where=(source in ("CD" "ER"))) nodupkey out=cd; by &id med_d; run;
	data cd2 (keep=&ID lag2_d rename=(lag2_d=med_d));
		set cd;
		by &id med_d;
		if first.&id then c = 0 ; c + 1; 
		lag2_d  = ifn(first.&id, (.), lag2(MED_D)) ;
		if lag2_d ne . then dif = yrdif(lag2_d, MED_D);
		if c >= 3 ; 
		if dif <= 2 ;
		FORMAT lag2_d YYMMDD10.;
	run ;
	Proc sort data=cd2 nodupkey ; by &id; run;

	****住院1次;
	data &OUT (drop=source);
		set cd2 &IN(keep=&ID med_d source where=(source="DD"));
	run;

	Proc sort data=&OUT         ; by &ID med_d;
	Proc sort data=&OUT nodupkey; by &ID; 
	run;
%Mend ;
%define_case(imf.NMOSD_dx       , NMOSD_ca)   *492 ;
%define_case(imf.MS_dx          , MS_ca)      *1316;
%define_case(NMOSD_dx_without_MS, NMOSD_ca_1) *234 ;
%define_case(NMOSD_dx_with_MS   , NMOSD_ca_2) *258 ;
%define_case(MS_dx2             , MS_ca_1)    *1072;

/*Drugs*/
%Macro Retri_Rx_NHI(DRUGS_N=, MACRO_DRUGS=, DRUGS=, OUT=);
	DATA &OUT  (drop=temp_d);
		SET CGRD_K.IP_DO (in=_b drop=RCFNO   rename=(FRTM=temp_d)    where=(NHINO in (&MACRO_DRUGS )))
			CGRD_K.OP_OO (drop=CHRDSMK RCFNO rename=(CHRGDAT=temp_d) where=(NHINO in (&MACRO_DRUGS )))            
        ;
		length drug_cat $2;
		if _b=0 and QTY <= 0 then delete;/*刪除批價資料識別碼為D或批價數量為負數(代表減退項目)*/ 
		%DO i=1 %TO &DRUGS_N; 
			%Let Var = %SCAN(&DRUGS, &i);
			if NHINO in ( &&&Var ) then drug_cat="&i";
		%END;
		if _b then source="DD"; else source="CD";
		MED_D = input(temp_d,yymmdd8.);
		format MED_D yymmdd10.;
	run;
%Mend Retri_Rx_NHI;
%Retri_Rx_NHI(
DRUGS_N = 17 , 
MACRO_DRUGS = 
&Azathioprine &Mycophenolate &Rituximab &Mitoxantrone
&inter_beta_1a &inter_beta_1b &Glatiramer &Natalizumab &Alemtuzumab 
&Dimethyl &Teriflunomide &Fingolimod &Cladribine &Siponimod 
&Prednisolone &Cyclosporine &Cyclophosphamide 
, 
DRUGS = 
Azathioprine Mycophenolate Rituximab Mitoxantrone
inter_beta_1a inter_beta_1b Glatiramer Natalizumab Alemtuzumab 
Dimethyl Teriflunomide Fingolimod Cladribine Siponimod 
Prednisolone Cyclosporine Cyclophosphamide, 
OUT = DMT_NMOSD_drug);

PROC SQL;
CREATE TABLE both_drugs AS 
	SELECT distinct a.&ID 
	FROM DMT_NMOSD_drug (where=(drug_cat in ("1" "2" "3" "4"))) /*MS drugs*/ A
	INNER JOIN DMT_NMOSD_drug (where=(drug_cat not in ("1" "2" "3" "4"))) /*NMOSD drugs*/ B 
	ON A.&ID=B.&ID
	WHERE a.MED_D < b.MED_D;
QUIT;

%Macro define_MS(indata, outdata);
	PROC SQL;
	CREATE TABLE temp AS 
		SELECT distinct a.*
		FROM &indata A
		INNER JOIN IMF.NMOSD_dx B 
		ON A.&ID=B.&ID and a.med_d <= b.med_d;
	QUIT;

	PROC SQL; 
	CREATE TABLE &outdata AS 
		SELECT distinct a.*
		FROM temp A
		INNER JOIN both_drugs B 
		ON A.&ID=B.&ID;
	QUIT; 
%Mend define_MS;
%define_MS(MS_ca  , MS_with_NMOdx)   * 234 --> 101;
%define_MS(MS_ca_1, MS_with_NMOdx_1) * 3   --> 1  ;

/* 492 (NMOSD_ca) + 101 (MS_with_NMOdx) = 593 */
/* 234 (NMOSD_ca_1) + 258 (NMOSD_ca_2) + 1 (MS_with_NMOdx_1) = 593 */

data x; set NMOSD_ca MS_with_NMOdx;run;
Proc sort data=x nodupkey; by &ID; run;*493;

data temp x;
	set NMOSD_ca (in=_a) /*492*/
		MS_with_NMOdx (in=_b); /*101*/
	length seq $10;
	if _a then seq = "1.NMOSD";
	   else    seq = "2.MS"; 
	NMOSD_y = year(med_d);
	if NMOSD_y >= 2006 then output temp;/*570*/
		else output x;/*23*/
	rename med_d=NMOSD_d;
run;

Proc sort data=temp     					     ; by &ID NMOSD_d seq; 
Proc sort data=temp nodupkey out= imf.NMOSD_final; by &ID; 
run;*490;

proc sort data=imf.NMOSD_final; by &ID;
proc sort data=NMOSD_ca_1     ; by &ID;
data imf.NMOSD_final_V2;
    merge imf.NMOSD_final(in=_a drop=seq) NMOSD_ca_1(in=_b keep=&ID); by &ID;
    if _a;
	if _b then cohort = "1.NMOSD";
		else cohort = "2.MS";
run;

Proc freq data= imf.NMOSD_final   ; table seq; run;
Proc freq data= imf.NMOSD_final_v2; table cohort; run;

data AQP4;*2204次;
	set CGRD_K.LAB_RESULT (where=(LABSH1IT in ("72-292")));
	if LABRESUVAL="Negative" then Result="Negative";
		else if LABRESUVAL="Positive" then Result="Positive";
		else if .<Value<3 then Result="Negative";
		else if   Value>3 then Result="Positive";
run;
Proc sort data=AQP4                      ; by &ID descending Result;*陽性排第一個;
Proc sort data=AQP4 nodupkey out=AQP4_uni; by &ID; 
run;

PROC SQL;*485;
CREATE TABLE imf.pop AS 
	SELECT a.&ID, a.NMOSD_d, a.NMOSD_y, LAST_VISIT_D, SEX, 
		   yrdif(input(BIRTHDAY,yymmdd10.) , a.NMOSD_d, 'Age') as Age, ifn(calculated Age>=18, 1, 0) as Age_larger18, 
		   yrdif(NMOSD_d, LAST_VISIT_D) as fy, ifn(calculated fy>=5, 1, 0) as fy_larger5,  
		   ifn(c.&ID ne "", 1, 0) as NMOSD_hospitalization, 
		   ifn(d.&ID ne "", 1, 0) as had_AQP4, d.Result as AQP4_Result   
	FROM imf.NMOSD_final A
	INNER JOIN CGRD_K.UNIQUE_IDCODE B ON A.&ID=B.&ID
	LEFT JOIN (select distinct &ID from imf.NMOSD_dx where Source = "DD") C ON A.&ID=C.&ID
	LEFT JOIN AQP4_uni D ON A.&ID=D.&ID;
QUIT;
Proc sort data=imf.pop nodupkey out=x ; by &ID; run;

/*===   (1) 先抓所有人的drug資料   ===*/
data OO;
	set CGRD_K.OP_CGDA (where=(RCFNO_C in (&R_IV_Steroid &R_Oral_Steroid &R_PLEX &R_IVIG &R_AZA &R_MMF)));
run;
data DO;
	set CGRD_K.IP_CGDA (where=(RCFNO_C in (&R_IV_Steroid &R_Oral_Steroid &R_PLEX &R_IVIG &R_AZA &R_MMF)));
run;

Proc SQL;
create table DO2 (drop=FRTM ENDTM) as
select &ID, FRTM, ENDTM, RCFNO_C, Drug_days, MED_D, sum(QTY) as QTY
	from DO (drop= CHRGDPT)
  	group by &ID, FRTM, ENDTM, RCFNO_C, Drug_days, MED_D
	having QTY>0;
quit;

data OODO;*14553031;
	set OO (in=_a keep=&ID RCFNO_C Drug_days MED_D QTY) DO2;
	if _a then Source = "CD"; else Source = "DD";
	if Drug_days = . then Drug_days = 1;
run;

Proc SQL;*48898;
create table imf.OODO_raw as 
select a.* 
	from OODO a
	inner join (select distinct &ID from imf.NMOSD_dx) b on a.&ID=b.&ID ;
quit;

data DX;
	set IMF.MS_DX IMF.NMOSD_DX;
run;

Proc SQL;*46579;
create table imf.OODO_NMO as 
select distinct a.* 
	from OODO a
	inner join DX b on a.&ID=b.&ID
	where a.med_d=b.med_d or b.med_d <= a.med_d <= b.out_d ;
quit;

/*===   (2) clean drug data of study population   ===*/
PROC SQL;
CREATE TABLE OODO2 AS 
	SELECT a.*
	FROM imf.OODO_NMO A
	INNER JOIN IMF.POP B 
	ON A.&ID=B.&ID
	where a.med_d >= b.NMOSD_d;
QUIT;

data Steroid x;
	set OODO2 (where=(RCFNO_C in (&IV_Steroid &Oral_Steroid)));
	length Drug_Name $25;
	if RCFNO_C in ("PMA012M" "PMA032M" "PTA094S" "PMA050M" "PMA026M" "PMA016M") then Drug_cate = "Oral_Steroid";
		else Drug_cate = "IV_Steroid";
	if RCFNO_C in ("PMA020P" "PMA032M") then Dose_mg = 5;
		else if RCFNO_C in ("P6A676P" "PZA261P") then Dose_mg = 0.7;
		else if RCFNO_C in ("PMA024P") then Dose_mg = 100;
		else if RCFNO_C in ("PMA018P") then Dose_mg = 40;
		else if RCFNO_C in ("PMA027P") then Dose_mg = 500;
		else if RCFNO_C in ("PMA012M") then Dose_mg = 25;
		else if RCFNO_C in ("PTA094S") then Dose_mg = 60;
		else if RCFNO_C in ("PMA050M" "PMA026M") then Dose_mg = 4;
		else if RCFNO_C in ("PMA016M") then Dose_mg = 0.5;
	/*Oral_Steroid*/
	if RCFNO_C in ("PMA016M" "PMA026M" "PMA020P" "PZA261P" "P6A676P") then Drug_Name = "Dexamethasone";
		else if RCFNO_C in ("PMA032M" "PTA094S") then Drug_Name = "Prednisolone";
		else if RCFNO_C in ("PMA050M" "PMA027P" "PMA018P") then Drug_Name = "Methylprednisolone";
		else if RCFNO_C in ("PMA012M") then Drug_Name = "Cortisone";
		else if RCFNO_C in ("PMA024P") then Drug_Name = "Hydrocortisone";
	if RCFNO_C in ("PMA016M" "PMA026M" "PMA020P" "PZA261P" "P6A676P") then Equivalent_rate=25/4;*Dexamethasone;
		else if RCFNO_C in ("PMA032M" "PTA094S") then Equivalent_rate=4/4;*Prednisolone;
		else if RCFNO_C in ("PMA050M" "PMA027P" "PMA018P") then Equivalent_rate=5/4;*Methylprednisolone;
		else if RCFNO_C in ("PMA012M") then Equivalent_rate=0.8/4;*Cortisone;
		else if RCFNO_C in ("PMA024P") then Equivalent_rate=1/4;*Hydrocortisone;
	All_dose  = QTY*Dose_mg;
	Aver_dose = All_dose/Drug_days;
	if Drug_cate="Oral_Steroid" and Aver_dose>=100 then output x;*37;
		else output Steroid;*19660;
run;

/*===   (3) focus on Methylprednisolone 500mg   ===*/
data Methy(drop=RCFNO_C QTY);
	set Steroid (keep=&ID QTY Drug_days RCFNO_C MED_D where=(RCFNO_C="PMA027P"));
	if QTY=4  then Drug_days=2;
	if QTY=5  then Drug_days=2;
	if QTY=6  then Drug_days=3;
	if QTY=8  then Drug_days=4;
	if QTY=10 then Drug_days=5;
	Aver_QTY = QTY/Drug_days;
run;
Proc sort data=Methy; by &ID med_d; run;

****假設一天最多2~3隻，把多的藥物往後面日子計算;
%Macro aaa(in=Methy);
	data temp1;
		set &IN;
	run;
	%do i=1 %to 12;
		/*將開藥記錄改成一天一筆*/
		data temp2 (keep=&ID med_d Aver_QTY);
			set temp1 ;
			End_d = med_d+Drug_days-1;
			do while (med_d <= End_d);
				output temp2;
				med_d = med_d+1;
			end;
			format End_d yymmdd10.;
		run;

		/*加總同一天開藥*/
		Proc SQL;
		create table temp3 as
		select &ID, med_d, sum(Aver_QTY) as QTY
			from temp2
			group by &ID, med_d
			order by &ID, med_d ;
		quit;
		Proc means data=temp3 ;title "temp3 for the &i.th time"; var QTY; run;title;

		/*設定一天最高3支，重新迴圈*/
		data temp1;
			set temp3;
			if QTY<=3 then Drug_days=1;
				else if QTY=4 then Drug_days=2;
				else if QTY=5 then Drug_days=2;
				else if QTY=6 then Drug_days=3;
			Aver_QTY = QTY/Drug_days;
		run;
	%end;
%mend;
%aaa()
Proc means data=temp1; var Aver_QTY;title "temp1"; run;title;

****最後輸出為temp3，一天一筆，計算每個療程(period);
Proc sort data=temp3; by &ID med_d;
data Methy2 (drop=lag_d);
	set temp3; by &ID med_d;
	if first.&ID then period=1;
	lag_d = lag(med_d);
	if not(first.&ID) and abs(med_d - lag_d) > 2 then period+1;
	aver_dose = QTY*500;
	format lag_d yymmdd10.;
run;
Proc sort data=Methy2 nodupkey out=x ; by &ID; run;*310;

/*===   (4) 計算復發   ===*/
Proc sort data=Methy2 ; by &ID period;
data IV2;
	set Methy2; by &ID period;
	if first.period then do;
		cum_QTY=0; cum_days=0;
	end;
	cum_QTY  + QTY;
	cum_days + 1;
run; 

Proc sort data=Methy2                    ; by &ID period MED_D; run;
Proc sort data=Methy2 nodupkey out=Methy3; by &ID period; 
run;*711;

Proc SQL;*518;
create table Methy4 as 
select a.&ID, a.MED_D, a.period
	from Methy3 a
	inner join (select distinct &ID, period
			    from IV2
				where 6<=cum_QTY and 3<=cum_days<=5) c1 on a.&ID=c1.&ID and a.period=c1.period /*1265*/ ;
quit;

/*===   (5) 加入 Plasma exchange & IV IgG  ===*/
data relapse (keep=&ID med_d relapse PLEX IVIG);*608;
	set OODO2  (in=_a keep=&ID med_d Source RCFNO_C where=(Source = "DD" and RCFNO_C in (&PLASMA_EXCHANGE &IVIG)))
        Methy4 (in=_b drop=period); 
	if RCFNO_C in (&PLASMA_EXCHANGE) then PLEX = 1;
	if RCFNO_C in (&IVIG) then IVIG = 1;
	relapse = 1;
run;

/*===   (6) 28 days內算同一次  ===*/
Proc sort data=relapse; by &ID med_d; run;
data temp;
	set relapse; by &ID med_d;
	if first.&ID then do;
		seq=1;
		end_d = med_d+28;
	end;
	if med_d>end_d then do;  
		seq+1;
		end_d = med_d+28;
	end;
	retain end_d;
	format end_d yymmdd10.;
run;

Proc SQL;*523;
create table temp2 as
select &ID, seq, min(MED_D) as relapse_d format=yymmdd10., max(max(relapse),0) as Methy6_in5, max(max(PLEX),0) as PLEX, max(max(IVIG),0) as IVIG 
	from temp
	group by &ID, seq;
quit;

proc sort data=temp2  ; by &ID;
proc sort data=imf.pop; by &ID;
data imf.relapse_new; *523;
    merge temp2(in=_a drop=Methy6_in5) imf.pop (in=_b keep=&ID NMOSD_d); by &ID;
    if _a;
	dif_days = relapse_d-NMOSD_d;
run;
Proc sort data=imf.relapse_new nodupkey out=x ; by &ID; run;*266;

/*===   (7) 第一次診斷也算一次 ===*/
Proc sort data=imf.relapse_new ; by &ID relapse_d; run;
data relapse_new2 (drop=dif_days);
	set imf.relapse_new (drop=seq); by &ID relapse_d;
	if first.&ID then do;
		if dif_days <= 28 then do;
			relapse_d = NMOSD_d ; * 第一次診斷28 days內算同一次;
			output;
		end;
		else do;
			output;
			relapse_d = NMOSD_d ;
			output;
		end;
	end;
	else output;
run;

Proc sort data = relapse_new2 ; by &ID relapse_d; run;
data relapse_new3;
	set relapse_new2; by &ID relapse_d;
	if first.&ID then seq=0;
	seq+1;
	if not(first.&ID) and dif(relapse_d)<=730 then inten=1;
	if not(first.&ID) and dif(relapse_d)<=365 then extrm=1;
	fy = yrdif(NMOSD_d, relapse_d);
run;
Proc means data = relapse_new3 mean std median q1 q3 min max; var fy; run;

Proc sort data= relapse_new3 			   ; by &ID descending seq;
Proc sort data= relapse_new3 nodupkey out=x; by &ID ; 
run;
Proc freq data = x; table seq; run;

PROC SQL;*485;
CREATE TABLE imf.pop2 AS 
	SELECT a.*, coalesce(b.no_of_relapse,1) as no_of_relapse, 
		   coalesce(b2.relapses,1) as relapses_less3, coalesce(b3.relapses,0) as relapses_larger3, 
		   ifn(c.inten_d ne . , 1, 0) as inten_ys, c.inten_d format=yymmdd10., 
		   ifn(d.extrm_d ne . , 1, 0) as extrm_ys, d.extrm_d format=yymmdd10.
	FROM imf.pop A
	LEFT JOIN (select &ID, max(seq)   as no_of_relapse from relapse_new3 group by &ID) B ON A.&ID=B.&ID
	LEFT JOIN (select &ID, count(seq) as relapses from relapse_new3 where yrdif(NMOSD_d,relapse_d)< 3 group by &ID) B2 ON A.&ID=B2.&ID
	LEFT JOIN (select &ID, count(seq) as relapses from relapse_new3 where yrdif(NMOSD_d,relapse_d)>=3 group by &ID) B3 ON A.&ID=B3.&ID
	LEFT JOIN (select &ID, min(relapse_d) as inten_d from relapse_new3 where inten=1 group by &ID) C ON A.&ID=C.&ID
	LEFT JOIN (select &ID, min(relapse_d) as extrm_d from relapse_new3 where extrm=1 group by &ID) D ON A.&ID=D.&ID;
QUIT;

%Macro Retri_All_Dx(IN=, OUT=All_dx);
	Proc sort data=&IN (keep=&ID) nodupkey out=tempid; by &ID; run;

	Proc SQL;
	create table &OUT as 
	select b.&ID, input(IPDAT,yymmdd8.) as MED_D format=yymmdd10., DSSID as DXKD1, "CD" as source
	    from tempid a
	        inner join CGRD_K.OP_DIAG b on a.&id=b.&id

	OUTER UNION CORR

	select b.&ID, input(IPDAT,yymmdd8.) as MED_D format=yymmdd10., DSSID as DXKD1, "ER" as source
	    from tempid a
	        inner join CGRD_K.ER_DIAG b on a.&id=b.&id

	OUTER UNION CORR

	select b.&ID, input(ADMDAT,yymmdd8.) as MED_D format=yymmdd10., b.DXKD1, b.DXKD2, b.DXKD3, b.DXKD4, b.DXKD5, "DD" as source 
	    from tempid a
	        inner join CGRD_K.IP_ICD b on a.&id=b.&id;
	quit;
%Mend Retri_All_Dx;
%Retri_All_Dx(IN= imf.pop2, OUT=imf.All_dx );

Proc SQL;
create table temp_dx as 
select a.*, b.NMOSD_d
	from imf.All_dx a
	inner join imf.pop2 b on a.&ID=b.&ID ;
quit;

/*
SLE: M32.*   710.0
Systemic sclerosis: M34.* 710.1
Rheumatic arthritis: M05.* 714.0
Sjogren syndrome: M35.*  710.2
Ankylosing spondylitis: M45.* 720.0
Vasculitis: L95.9  446.0-446.7
Autoimmune thyroiditis: E06.3  245.2
*/
data claim_all;
	set temp_dx;
	array ICD $ DXKD1-DXKD5;
	do over ICD;
		if 140 <= input(substr(ICD,1,3),3.) <= 208 or 
		  (substr(ICD,1,1) = "C" and 0 <= input(substr(ICD,2,2),2.) <= 97) then cancer = 1;
		if ICD in: ("M32" "7100") then SLE = 1;
		if ICD in: ("M34" "7101") then sclerosis = 1;
		if ICD in: ("M05" "7140") then RA = 1;
		if ICD in: ("M35" "7102") then Sjogren = 1;
		if ICD in: ("M45" "7200") then AS = 1;
		if ICD in: ("L959" "4460" "4461" "4462" "4463" "4464" "4465" "4466" "4467") then Vasculitis = 1;
		if ICD in: ("E063" "2452") then AT = 1;
	end;
	if nmiss(of cancer--AT) ne 8;
	*if med_d <= NMOSD_d then output claim_prior;
	*drop DXKD1-DXKD5;
run;
Proc freq data= claim_all; table cancer--AT; run;
 

%Macro aaa(VAR=);
	Proc sort data=claim_all(where=(&VAR=1 and source="CD")) nodupkey out=cd; by &ID med_d; run;
	data cd;
		set cd; by &ID med_d;
		if first.&ID then count=0;
		count+1;
		if count = 2;
	run;

	data &VAR;
		set cd claim_all(where=(&VAR=1 and source="DD"));
	run;

	Proc sort data=&VAR(keep=&ID &VAR med_d)              ; by &ID med_d;
	Proc sort data=&VAR(rename=(med_d = &VAR._d)) nodupkey; by &ID; 
	run;
%Mend aaa;
%aaa(VAR=cancer)
%aaa(VAR=SLE)
%aaa(VAR=sclerosis)
%aaa(VAR=RA)
%aaa(VAR=Sjogren)
%aaa(VAR=AS)
%aaa(VAR=Vasculitis)
%aaa(VAR=AT)

/*Which cancer type is the major?*/
PROC SQL;
CREATE TABLE x (keep=&ID DXKD1-DXKD5) AS 
	SELECT distinct a.*
	FROM claim_all A
	INNER JOIN cancer B ON A.&ID=B.&ID and A.med_d=B.cancer_d;
QUIT;
Proc print data=x ;run;

data cancer_31 (keep=&ID ICD);
	set x;
	length ICD $10;
	array ICDarray (5) DXKD1-DXKD5;
	do i = 1 to 5;
		if ICD = "" then do;
			if 140 <= input(substr(ICDarray(i),1,3),3.) <= 208 or 
			  (substr(ICDarray(i),1,1) = "C" and 0 <= input(substr(ICDarray(i),2,2),2.) <= 97) then ICD = ICDarray(i);
		end;
	end;
run;
Proc print data=cancer_31;run;
/*End*/

proc sort data=IMF.POP2 ; by &ID;
data IMF.POP3; *485;
    merge IMF.POP2(in=_a) cancer SLE sclerosis RA Sjogren AS Vasculitis AT; by &ID;
    if _a;
	array ICD cancer--AT;
	do over ICD;
		if ICD=. then ICD=0;
	end;
	if cancer     = 1 then after_cancer     = ifn(cancer_d > NMOSD_d, 1, 0);
	if SLE        = 1 then after_SLE        = ifn(SLE_d > NMOSD_d, 1, 0);
	if sclerosis  = 1 then after_sclerosis  = ifn(sclerosis_d > NMOSD_d, 1, 0);
	if RA         = 1 then after_RA         = ifn(RA_d > NMOSD_d, 1, 0);
	if Sjogren    = 1 then after_Sjogren    = ifn(Sjogren_d > NMOSD_d, 1, 0);
	if AS         = 1 then after_AS         = ifn(AS_d > NMOSD_d, 1, 0);
	if Vasculitis = 1 then after_Vasculitis = ifn(Vasculitis_d > NMOSD_d, 1, 0);
	if AT         = 1 then after_AT         = ifn(AT_d > NMOSD_d, 1, 0);
run;

/********************************************************************************************************************************/
/*  Result                                                                                                                      */
/********************************************************************************************************************************/

/***   Table 1   ***/
***	Adult;
Proc means data=imf.pop3 n mean std median q1 q3; var age; where Age_larger18=1; run;  
Proc means data=imf.pop3 n median q1 q3 min max ; var fy ; where Age_larger18=1; run; 
Proc freq  data=imf.pop3; 
	table SEX cancer SLE sclerosis RA Sjogren AS Vasculitis AT NMOSD_hospitalization had_AQP4 AQP4_Result fy_larger5 inten_ys extrm_ys;
	where Age_larger18=1; 
run;

Proc freq  data = imf.pop3; 
	table after_cancer after_SLE after_sclerosis after_RA after_Sjogren after_AS after_Vasculitis after_AT;
	where Age_larger18=1; 
run;

Proc sort data= imf.pop2; by Age_larger18; run;
proc univariate data = imf.pop2 NORMAL ; 
	var age fy;
	*qqplot age / Normal(mu=est sigma=est color=red l=1);
	by Age_larger18;
run;

*** Pediatric;
Proc means data=imf.pop3 n mean std median q1 q3; var age; where Age_larger18=0; run;
Proc means data=imf.pop3 n median q1 q3 min max ; var fy ; where Age_larger18=0; run; 
Proc freq  data=imf.pop3 ; 
	table SEX cancer SLE sclerosis RA Sjogren AS Vasculitis AT NMOSD_hospitalization had_AQP4 AQP4_Result fy_larger5 inten_ys extrm_ys;
	where Age_larger18=0; 
run;

Proc freq  data = imf.pop3 ; 
	table after_cancer after_SLE after_sclerosis after_RA after_Sjogren after_AS after_Vasculitis after_AT;
	where Age_larger18=0; 
run;

/***   Table 2   ***/
Proc freq  data=imf.pop ; table NMOSD_y; where Age_larger18=1; run;
Proc freq  data=imf.pop ; table NMOSD_y*AQP4_Result/nopercent nocol norow; where Age_larger18=1; run;

Proc freq  data=imf.pop ; table NMOSD_y; where Age_larger18=0; run;
Proc freq  data=imf.pop ; table NMOSD_y*AQP4_Result/nopercent nocol norow; where Age_larger18=0; run;

%Macro cal_incid_dem(agelimit);
	%do year = 2006 %to 2021;
		Proc SQL;
		create table x_&year as 
		select distinct &ID
		    from CGRD_K.OP_DIAG(keep=&ID IPDAT) a
		    where substr(IPDAT,1,4) = "&year"

		OUTER UNION CORR

		select distinct &ID
			from CGRD_K.ER_DIAG(keep=&ID IPDAT) a
		    where substr(IPDAT,1,4) = "&year"

		OUTER UNION CORR

		select distinct &ID
			from CGRD_K.IP_ICD(keep=&ID ADMDAT) a
		    where substr(ADMDAT,1,4) = "&year"
		;
		quit;

		PROC SQL;
			SELECT count(distinct a.&ID) as No_&year
			FROM (select distinct &ID from x_&year) A
			INNER JOIN CGRD_K.UNIQUE_IDCODE(keep=&ID BIRTHDAY) B ON A.&ID=B.&ID 
			LEFT JOIN imf.pop2 C ON A.&ID=C.&ID
			where (&year - input(substr(BIRTHDAY,1,4),4.)) &agelimit and (c.NMOSD_d = . or c.NMOSD_y >= &year);
		QUIT;
	%end;
%Mend cal_incid_dem;
%cal_incid_dem(>=18);
%cal_incid_dem(<18 );

* focus on AQP-4 positive;
Proc freq  data=imf.pop ; table NMOSD_y*AQP4_Result/nopercent nocol norow; where Age_larger18=1; run;


/***   Table 5   ***/
data larger smaller; 
	set imf.pop2 (drop=fy_larger5); 
	length ARR_c $15;
	ARR = no_of_relapse/fy;
	ARR_less3 = relapses_less3 / min(fy, 3);
	if fy >=3 then ARR_larger3 = relapses_larger3/(fy-3);
	if fy < 3 then relapses_larger3 = .;
	if ARR=0 then ARR_c = "1.0";
		else if 0 <  ARR < 1 then ARR_c = "2.>0 to <1";
		else if 1 <= ARR < 2 then ARR_c = "3.>=1 to <2";
		else if 2 <= ARR < 3 then ARR_c = "4.>=2 to <3";
		else ARR_c = "5.>=3";
	if Age_larger18 = 1	then output larger;
		else output smaller;
run;

Proc means data=larger n median q1 q3 min max MAXDEC=2; var no_of_relapse ARR;run;
Proc means data=larger n median q1 q3 min max MAXDEC=2; var relapses_less3 relapses_larger3 ARR_less3 ARR_larger3; run;

Proc means data=smaller n median q1 q3 min max MAXDEC=2; var no_of_relapse ARR; run;
Proc means data=smaller n median q1 q3 min max MAXDEC=2; var relapses_less3 relapses_larger3 ARR_less3 ARR_larger3; run;

****TEST: Wilcoxon signed-rank test;
data temp;
	set larger (in=_a keep=ARR_less3   relapses_less3   rename=(relapses_less3=relapses ARR_less3=ARR)) 
		larger (      keep=ARR_larger3 relapses_larger3 rename=(relapses_larger3=relapses ARR_larger3=ARR));
	if _a then group=1; else group=2;
run;
PROC NPAR1WAY data = temp WILCOXON;
	VAR relapses ARR;
	CLASS group;
RUN;

data temp;
	set smaller (in=_a keep=ARR_less3   relapses_less3   rename=(relapses_less3=relapses ARR_less3=ARR)) 
		smaller (      keep=ARR_larger3 relapses_larger3 rename=(relapses_larger3=relapses ARR_larger3=ARR));
	if _a then group=1; else group=2;
run;
PROC NPAR1WAY data = temp WILCOXON;
	VAR relapses ARR;
	CLASS group;
RUN;


/***   S4   ***/
Proc means data=larger n median q1 q3 MAXDEC=2; var ARR; where AQP4_Result = "Positive"; run;
Proc means data=larger n median q1 q3 MAXDEC=2; var ARR; where AQP4_Result = "Negative"; run;
Proc means data=larger n median q1 q3 MAXDEC=2; var ARR_less3 ARR_larger3; where AQP4_Result = "Positive"; run;
Proc means data=larger n median q1 q3 MAXDEC=2; var ARR_less3 ARR_larger3; where AQP4_Result = "Negative"; run;

Proc means data=smaller n median q1 q3 MAXDEC=2; var ARR; where AQP4_Result = "Positive"; run;
Proc means data=smaller n median q1 q3 MAXDEC=2; var ARR; where AQP4_Result = "Negative"; run;
Proc means data=smaller n median q1 q3 MAXDEC=2; var ARR_less3 ARR_larger3; where AQP4_Result = "Positive"; run;
Proc means data=smaller n median q1 q3 MAXDEC=2; var ARR_less3 ARR_larger3; where AQP4_Result = "Negative"; run;

data temp;
	set larger (in=_a keep=ARR_less3 relapses_less3 rename=(relapses_less3=relapses ARR_less3=ARR)) 
		smaller(      keep=ARR_less3 relapses_less3 rename=(relapses_less3=relapses ARR_less3=ARR));
	if _a then group=1; else group=2;
run;
PROC NPAR1WAY data = temp WILCOXON;
	VAR ARR;
	CLASS group;
RUN;*0.278; 

%Macro cal_p(group1, status1, var1, group2, status2, var2);
	data temp;
		set &group1 (in=_a keep=AQP4_Result &var1 rename=(&var1 = ARR) where=(AQP4_Result = &status1 )) 
			&group2 (      keep=AQP4_Result &var2 rename=(&var2 = ARR) where=(AQP4_Result = &status2 ));
		if _a then group=1; else group=2;
	run;
	PROC NPAR1WAY data = temp WILCOXON; VAR ARR; CLASS group; RUN;
%Mend ;
%cal_p(larger , "Positive", ARR, larger, "Negative", ARR)  *0.0970; 
%cal_p(smaller, "Positive", ARR, smaller, "Negative", ARR)*0.3865; 
%cal_p(larger , "Positive", ARR, smaller, "Positive", ARR) *0.4269; 
%cal_p(smaller, "Positive", ARR_less3, smaller, "Negative", ARR_less3)*0.2454; 

%cal_p(larger, "Positive", ARR_less3, larger, "Positive", ARR_larger3)  *<.001; 
%cal_p(larger, "Negative", ARR_less3, larger, "Negative", ARR_larger3)  *<.001; 
%cal_p(smaller, "Positive", ARR_less3, smaller, "Positive", ARR_larger3)*0.064; 
%cal_p(smaller, "Negative", ARR_less3, smaller, "Negative", ARR_larger3)*0.057; 

 

/***   Figure 2   ***/
Proc freq data= larger ; table ARR_c; run;
Proc freq data= smaller; table ARR_c; run;

/*malignancy病人得發生腫瘤的平均年齡 IQR 可以在臨床上用  在NMOSD診斷前 ? 年  或診斷後 ? 年會出現呢*/

PROC SQL; 
CREATE TABLE cal_age AS 
	SELECT a.*, input(b.BIRTHDAY, yymmdd10.) as BIRTH_DAY format=yymmdd10. 
	FROM imf.pop3(keep=&ID NMOSD_d after_cancer cancer_d cancer where=(cancer=1)) A
	INNER JOIN CGRD_K.UNIQUE_IDCODE B ON A.&ID=B.&ID ;
QUIT;

data cal_age2;
	set cal_age;
	cancer_age = yrdif(BIRTH_DAY, cancer_d);
	NMOSD_age  = yrdif(BIRTH_DAY, NMOSD_d );
	dif = abs(yrdif(NMOSD_d,cancer_d));
run;
Proc sort data=cal_age2; by cancer_age; run;

Proc means data=cal_age2 mean std median q1 q3 min max; var cancer_age; run;
Proc means data=cal_age2 mean std median q1 q3; var dif; class after_cancer; run;

/* 2023-02-14 癌症跟自體免疫疾病發生的年齡跟性別 */

proc sort data=imf.pop3 ; by &ID;
proc sort data=CGRD_K.UNIQUE_IDCODE ; by &ID;
data cal_age (Keep= &ID SEX Age_larger18 cancer SLE sclerosis RA Sjogren AS Vasculitis AT cancer_age--AT_age);
    merge imf.pop3 (in=_a keep=&ID NMOSD_d SEX Age_larger18 cancer_d--AT) 
		  CGRD_K.UNIQUE_IDCODE (keep=&ID BIRTHDAY); by &ID;
    if _a;
	BIRTH_DAY = input(BIRTHDAY, yymmdd10.); 
	if cancer     = 1 then cancer_age     = yrdif(BIRTH_DAY, cancer_d);
	if SLE        = 1 then SLE_age        = yrdif(BIRTH_DAY, SLE_d);
	if sclerosis  = 1 then sclerosis_age  = yrdif(BIRTH_DAY, sclerosis_d);
	if RA         = 1 then RA_age         = yrdif(BIRTH_DAY, RA_d);
	if Sjogren    = 1 then Sjogren_age    = yrdif(BIRTH_DAY, Sjogren_d);
	if AS         = 1 then AS_age         = yrdif(BIRTH_DAY, AS_d);
	if Vasculitis = 1 then Vasculitis_age = yrdif(BIRTH_DAY, Vasculitis_d);
	if AT         = 1 then AT_age         = yrdif(BIRTH_DAY, AT_d);
run;

Proc means data=cal_age n mean std maxdec=1; 
	var cancer_age--AT_age;
	where Age_larger18=1;  
run;

Proc freq data=cal_age ; 
	table (cancer SLE sclerosis RA Sjogren AS Vasculitis AT)*SEX/nopercent nocol; 
	where Age_larger18=1; 
run;


Proc means data=cal_age n mean std maxdec=1; 
	var cancer_age--AT_age;
	where Age_larger18=0;  
run;

Proc freq data=cal_age ; 
	table (cancer SLE sclerosis RA Sjogren AS Vasculitis AT)*SEX/nopercent nocol; 
	where Age_larger18=0; 
run;

/***   2023-03-06 revise and add some tables  ***/
data cancer_15;
	set cancer_31 ;
	length cancer_name $5.;
	if ICD in ("1742" "1749" "1743"	"1749" "C50919") then cancer_name = "BC";
	if ICD in ("1919" "C717" "C719") then cancer_name = "Brain";	
	if ICD in ("1550") then cancer_name = "Liver";				
	if ICD in ("C20") then cancer_name = "colon"; 
	if cancer_name ne "";
run;

PROC SQL;
CREATE TABLE cancer_15_2 AS 
	SELECT cancer_name, b.*
	FROM cancer_15 A
	INNER JOIN imf.pop3 B 
	ON A.&ID=B.&ID;
QUIT;
Proc freq data=cancer_15_2 ; table cancer_name*AQP4_Result / nocol nopercent; run;

Proc freq  data=imf.pop3; 
	table had_AQP4 AQP4_Result after_cancer*AQP4_Result;
	where Age_larger18=1 and cancer=1; 
run;
 
PROC SQL;
CREATE TABLE x AS 
	SELECT a.*
	FROM IMF.NMOSD_FINAL_V2 A
	LEFT JOIN imf.pop2 B 
	ON A.&ID=B.&ID 
	WHERE B.&ID is null;
QUIT;

PROC SQL;
CREATE TABLE x AS 
	SELECT a.*
	FROM IMF.NMOSD_FINAL_V2 A
	INNER JOIN imf.pop2 B 
	ON A.&ID=B.&ID;
QUIT;
Proc freq data=x ; table cohort; run;

/***   Table 1-2   ***/
PROC SQL;
CREATE TABLE temp_pop AS 
	SELECT a.*, b.cohort, c.cohort as mis_diag 
	FROM imf.pop3 A
	LEFT JOIN IMF.NMOSD_FINAL_V2  B ON A.&ID=B.&ID
	LEFT JOIN IMF.MIS_DIAG_RESULT C ON A.&ID=C.&ID;
QUIT;

data temp_pop2 (drop=cohort mis_diag);
	set temp_pop;
	length cate $15;
	if cohort = "2.MS" then do;
		cate = "1.MS";*255;
		output;
	end;
	if mis_diag = "2.MS" then do;
		cate = "2.MS with DPT";*70;
		output;
	end;
	if mis_diag = "1.NMOSD" then do;
		cate = "3.NMOSD";*230;
		output;
	end;
run;



%Macro table_1_2(indata);
	Proc freq data = &indata; table cate; run;

	Proc means data = &indata n mean std median q1 q3 maxdec=1; var age; class cate; run;  
	Proc means data = &indata n  median q1 q3 min max maxdec=3; var fy ; class cate; run; 
	Proc freq  data = &indata; 
		table (SEX cancer SLE sclerosis RA Sjogren AS Vasculitis AT NMOSD_hospitalization had_AQP4 AQP4_Result fy_larger5 inten_ys extrm_ys)
			   *cate / nopercent norow nocol; 
	run;

	Proc freq  data = &indata; 
		table (SEX cancer SLE sclerosis RA Sjogren AS Vasculitis AT NMOSD_hospitalization had_AQP4 AQP4_Result fy_larger5 inten_ys extrm_ys)
			   *cate / nopercent norow nocol nofreq chisq; 
	run;

	Proc freq  data = &indata; 
		table (after_cancer after_SLE after_sclerosis after_RA after_Sjogren after_AS after_Vasculitis after_AT)
			   *cate / nopercent norow nocol chisq fisher; 
	run;
 
	proc anova data = &indata;  
	  class cate; 
	  model age = cate; 
	  ods select ModelANOVA;
	run;

	PROC NPAR1WAY data = &indata WILCOXON;     
		VAR age fy;
		CLASS cate;
	RUN;
%Mend table_1_2;
%table_1_2(temp_pop2)


* focus on AQP-4 positive;
data temp_pop_AQP4;*222;
	set temp_pop2 (where=(AQP4_Result="Positive"));
run;

Proc freq data = temp_pop_AQP4; table cate; run;

Proc means data = temp_pop_AQP4 n mean std median q1 q3 maxdec=1; var age; class cate; run;  
Proc means data = temp_pop_AQP4 n median q1 q3 min max  maxdec=3; var fy ; class cate; run; 
Proc freq  data = temp_pop_AQP4; 
	table (sex cancer after_cancer SLE after_SLE sclerosis after_sclerosis RA after_RA Sjogren after_Sjogren AS after_AS 
 		   Vasculitis after_Vasculitis AT after_AT NMOSD_hospitalization fy_larger5 inten_ys extrm_ys) * cate / nopercent norow nocol; 
run;

%Macro p_value(subpop);
	Proc freq  data = temp_pop_AQP4; 
		where cate in (&subpop );
		table (sex cancer after_cancer SLE after_SLE sclerosis after_sclerosis RA after_RA Sjogren after_Sjogren AS after_AS 
	 		   Vasculitis after_Vasculitis AT after_AT NMOSD_hospitalization fy_larger5 inten_ys extrm_ys) * cate / fisher; 
		ods select CrossTabFreqs FishersExact;
	run;

	PROC TTEST DATA = temp_pop_AQP4;
		where cate in (&subpop );
		CLASS cate;
		VAR age;
		ods select Statistics TTests Equality;
	RUN;

	PROC NPAR1WAY data = temp_pop_AQP4 WILCOXON; 
		where cate in (&subpop ); 
		VAR age fy;
		CLASS cate;
	RUN;
%Mend p_value;
%p_value('1.MS' '3.NMOSD')
%p_value('2.MS with DPT' '3.NMOSD')

 



**** supp: 255 vs. 230 and 70 vs. 230;
%Macro get_pvalue(group); 
	Proc freq  data=temp_pop2; 
		TABLE (SEX cancer after_cancer SLE after_SLE sclerosis after_sclerosis RA after_RA Sjogren after_Sjogren AS after_AS 
			   Vasculitis after_Vasculitis AT after_AT NMOSD_hospitalization had_AQP4 AQP4_Result fy_larger5 inten_ys extrm_ys )
			   *cate / nopercent norow nocol chisq; 
		WHERE cate in ( &group );
		ODS SELECT ChiSq;
	run;

	PROC TTEST DATA = temp_pop2;
		WHERE cate in ( &group );
		CLASS cate;
		VAR age;
		ods select Statistics TTests Equality;
	RUN;

	PROC NPAR1WAY data=temp_pop2 WILCOXON;  
		WHERE cate in ( &group ); 
		VAR age fy;
		CLASS cate;
	RUN; 
%Mend get_pvalue;
%get_pvalue("1.MS" "3.NMOSD")
%get_pvalue("2.MS with DPT" "3.NMOSD")



/***   Table 5-2. Annualized Relapse Rates for Patients with NMOSD in CGMH.   ***/
data temp_pop3; 
	set temp_pop2 (drop=fy_larger5); 
	length ARR_c $15;
	ARR = no_of_relapse/fy;
	ARR_less3 = relapses_less3 / min(fy, 3);
	if fy >=3 then ARR_larger3 = relapses_larger3/(fy-3);
	if fy < 3 then relapses_larger3 = .;
	if ARR=0 then ARR_c = "1.0";
		else if 0 <  ARR < 1 then ARR_c = "2.>0 to <1";
		else if 1 <= ARR < 2 then ARR_c = "3.>=1 to <2";
		else if 2 <= ARR < 3 then ARR_c = "4.>=2 to <3";
		else ARR_c = "5.>=3";
	*if Age_larger18 = 1	then output larger;
		*else output smaller;
run;
Proc means data=temp_pop3 mean std; var no_of_relapse fy; class cate; where cate in ("2.MS with DPT" "3.NMOSD"); run;

%Macro aaa(var,out,digit=8.0);
	Proc means data=temp_pop3 noprint; 
		var &var; 
		class cate; 
		output out=&out(where=(_type_=1)) n=num MEDIAN=median QRANGE=IQR min=min max=max;
	run;
	data &out (keep=cate value);
		set &out;
		value = compress(put(median,&digit)||"±"||put(IQR,&digit))||" ("||compress(put(min,&digit)||"-"||put(max,&digit)||")");
	run;
%Mend aaa;
%aaa(no_of_relapse   , x1)
%aaa(relapses_less3  , x2)
%aaa(relapses_larger3, x3)
%aaa(ARR             , x4, digit=8.2)
%aaa(ARR_less3       , x5, digit=8.2)
%aaa(ARR_larger3     , x6, digit=8.2)

PROC SQL;
CREATE TABLE alldata AS 
	SELECT a.cate, a.value as no_of_relapse, b2.value as relapses_less3, b3.value as relapses_larger3, 
		   b4.value as ARR, b5.value as ARR_less3, b6.value as ARR_larger3
	FROM x1 A
	INNER JOIN x2 B2 ON A.cate=B2.cate
	INNER JOIN x3 B3 ON A.cate=B3.cate
	INNER JOIN x4 B4 ON A.cate=B4.cate
	INNER JOIN x5 B5 ON A.cate=B5.cate
	INNER JOIN x6 B6 ON A.cate=B6.cate;
QUIT;
Proc print data = alldata ;run;

data temp;
	set temp_pop3 (in=_a keep=no_of_relapse ARR ARR_less3 ARR_larger3 relapses_less3 relapses_larger3 rename=(relapses_less3=relapses ARR_less3=ARR)) 
		temp_pop3 (      keep=no_of_relapse ARR ARR_less3 ARR_larger3 relapses_less3 relapses_larger3 rename=(relapses_larger3=relapses ARR_larger3=ARR));
	if _a then group=1; else group=2;
run;

PROC NPAR1WAY data = temp_pop3 WILCOXON;
	VAR no_of_relapse ARR ARR_less3 ARR_larger3 relapses_less3 relapses_larger3;
	CLASS cate;
	WHERE cate ne "1.MS";
RUN;

****  figure 5-2;
Proc freq data= temp_pop3; table ARR_c*cate/nofreq nopercent norow ; run;


/* 2023-03-15 羅醫師要求*/
**** 1.幾歲的人會得cancer，有cut-off point?;
 
%Macro cutoff(IN= , OUTCOME= , VAR= );
	proc logistic data = &IN; 
		model &OUTCOME (event='1') = &VAR / outroc=rocdata; 
		ods select ParameterEstimates;
		ods output ParameterEstimates=x;
	run; 

	proc sql noprint;
		select Estimate into : intercept 
		from x
		where Variable="Intercept";
		select Estimate into : slope 
		from x
		where Variable="&VAR";
	quit;

	data rocdata2 (keep=cutoff prob Sensitivity Specificity PPV NPV Youden );
		set rocdata;
		logit = log(_prob_/(1-_prob_));
		cutoff = (logit-( &intercept )) / &slope;
		prob = _prob_;
		Sensitivity = _SENSIT_;
		Specificity = 1-_1MSPEC_;
		PPV = _POS_ / (_POS_+_FALPOS_);
		NPV = _NEG_ / (_NEG_+_FALNEG_);
		Youden = _SENSIT_ + (1-_1MSPEC_) - 1;
	run;
	proc sort ; by descending Youden; run;
	Proc print ;title "&VAR for &OUTCOME"; run;title ;
%Mend cutoff;
%cutoff(IN= imf.pop3, VAR= Age , OUTCOME= cancer)
*best cutoff point is 43.42 years, Sensitivity = 0.8387, Specificity = 0.4604;
 
**** 2. 癌症跟非癌症的人前三年的ARR比較; 
data result_como (keep=&ID NMOSD_d ARR_less3 sex age fy no_of_relapse ARR cancer autoimmune_disease AQP4_Result); 
	set imf.pop3 (drop=fy_larger5 where=(Age_larger18 = 1)); 
	ARR = no_of_relapse/fy;
	ARR_less3 = relapses_less3 / min(fy, 3);
	if after_cancer=1 then cancer=0;
	autoimmune_disease = ifn( sum(of SLE, sclerosis, RA, Sjogren, AS, Vasculitis, AT) >= 1, 1, 0);
	if sum(of after_SLE, after_sclerosis, after_RA, after_Sjogren, after_AS, after_Vasculitis, after_AT) >= 1 then autoimmune_disease = 0;
run;

Proc means data=result_como mean median; var no_of_relapse ; run;
Proc means data=result_como n median q1 q3 MAXDEC=2; var age no_of_relapse fy ARR ARR_less3; class autoimmune_disease;run;

%Macro p_value(outcome, group, subgroup);
	Proc means data = result_como n median q1 q3 MAXDEC=2; var &outcome ; class &group; &subgroup;run;
	PROC NPAR1WAY data = result_como WILCOXON;
		&subgroup;
		VAR &outcome;
		CLASS &group;
	RUN;
%Mend p_value;
%p_value(ARR, cancer)
%p_value(ARR_less3, cancer) 

**** 3. 免疫疾病跟非免疫疾病的人前三年比較;
%p_value(ARR, autoimmune_disease)
%p_value(ARR_less3, autoimmune_disease) 

/*   AQP4_Result eq "Positive"   */
%p_value(ARR	  , cancer, where AQP4_Result eq "Positive")
%p_value(ARR_less3, cancer, where AQP4_Result eq "Positive") 

%p_value(ARR	  , autoimmune_disease, where AQP4_Result eq "Positive")
%p_value(ARR_less3, autoimmune_disease, where AQP4_Result eq "Positive") 

/*Rituximab*/
%Macro Retri_Rx_NHI(MACRO_DRUGS=, DRUGS=, OUT=);
	DATA &OUT  (drop=temp_d);
		SET CGRD_K.IP_DO (in=_b drop=RCFNO   rename=(FRTM=temp_d)    where=(NHINO in (&MACRO_DRUGS )))
			CGRD_K.OP_OO (drop=CHRDSMK RCFNO rename=(CHRGDAT=temp_d) where=(NHINO in (&MACRO_DRUGS )))            
        ;
		if _b=0 and QTY <= 0 then delete;/*刪除批價資料識別碼為D或批價數量為負數(代表減退項目)*/ 
		if _b then source="DD"; else source="CD";
		MED_D = input(temp_d,yymmdd8.);
		format MED_D yymmdd10.;
	run;
%Mend Retri_Rx_NHI;
%Retri_Rx_NHI(
MACRO_DRUGS = &Rituximab, 
DRUGS = Rituximab, 
OUT = Rituximab);

****確認是否ARD病患的ARR較低是因為有服用Rituximab;
PROC SQL;
CREATE TABLE result_como2 AS 
	SELECT a.*, coalesce(Rituximab, 0) as Rituximab
	FROM result_como A 
	LEFT JOIN ( SELECT distinct a2.&ID, 1 as Rituximab
			    FROM result_como A2
				INNER JOIN Rituximab B2 ON A2.&ID=B2.&ID
				WHERE a2.NMOSD_d <= b2.MED_D) B ON A.&ID=B.&ID;
QUIT;
Proc freq data=result_como2; table autoimmune_disease*Rituximab/chisq; run;


/*2023-06-02 
我需要的另外2個比較數字(在255人一開始MS診斷組與230人一開始NMOSD診斷組)
1. 從收案開始到正確診斷NMOSD時間(應該MS組比較長)
2. 從收案開始到抽血驗AQP4時間(不知MS組會不會比較長)
*/

****從收案開始到正確診斷NMOSD時間;
PROC SQL;
CREATE TABLE data1 AS 
	SELECT a.&ID, a.NMOSD_d, b.med_d, (b.med_d-a.NMOSD_d) as dif
	FROM temp_pop2 (where=(cate in ("1.MS"))) /*255*/ a
	INNER JOIN IMF.NMOSD_DX b 
	ON a.&ID=b.&ID and a.NMOSD_d <= b.med_d
	ORDER BY a.&ID, b.med_d;
QUIT;
Proc sort data=data1 nodupkey out=data2 ; by &ID; run;

Proc means data=data2 mean std median q1 q3 min max; var dif; run;

****從收案開始到抽血驗AQP4時間; 
data AQP4;
	set CGRD_K.LAB_RESULT (where=(LABSH1IT in ("72-292")));
	if LABRESUVAL="Negative" then Result="Negative";
		else if LABRESUVAL="Positive" then Result="Positive";
		else if .<Value<3 then Result="Negative";
		else if   Value>3 then Result="Positive";
run;

PROC SQL;
CREATE TABLE x AS 
	SELECT distinct a.&ID, b.Lab_date, a.cate, (Lab_date-NMOSD_d) as dif
	FROM temp_pop2 (where=(cate in ("1.MS" "3.NMOSD") and had_AQP4=1)) a
	INNER JOIN AQP4 b 
	ON a.&ID=b.&ID and Lab_date > NMOSD_d
	ORDER BY a.&ID, dif;
QUIT; 
Proc sort  data=x nodupkey out=x2; by &ID; run;
Proc means data=x2 mean std median q1 q3 min max; var dif; class cate; run;


PROC NPAR1WAY data=x2 WILCOXON;     
	VAR dif;
	CLASS cate;
RUN;

