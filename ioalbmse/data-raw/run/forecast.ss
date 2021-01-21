#V3.24f
#C  generic forecast file
# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr
1 # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy 
2 # MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr) 
0.4 # SPR target (e.g. 0.40)
0.4 # Biomass target (e.g. 0.40)
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF (enter actual year, or values of 0 or -integer to be rel. endyr)
 0 0 0 0 0 0
#  2010 2010 2010 2010 2010 2010 # after processing 
1 #Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below
#
0 # Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=Ave F (uses first-last relF yrs); 5=input annual F scalar
10 # N forecast years 
1 # F scalar (only used for Do_Forecast==5)
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF  (enter actual year, or values of 0 or -integer to be rel. endyr)
 0 0 0 0
#  0 0 0 0 # after processing 
1 # Control rule method (1=catch=f(SSB) west coast; 2=F=f(SSB) )
0.01 # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40)
0.001 # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10)
1 # Control rule target as fraction of Flimit (e.g. 0.75)
3 #_N forecast loops (1-3) (fixed at 3 for now)
3 #_First forecast loop with stochastic recruitment
0 #_Forecast loop control #3 (reserved for future bells&whistles)
0 #_Forecast loop control #4 (reserved for future bells&whistles)
0 #_Forecast loop control #5 (reserved for future bells&whistles)
2050  #FirstYear for caps and allocations (should be after years with fixed inputs) 
0 # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)
0 # Do West Coast gfish rebuilder output (0/1) 
-1 # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)
-1 # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)
1 # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below
# Note that fleet allocation is used directly as average F if Do_Forecast=4
2 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)
# Conditional input if relative F choice = 2
# Fleet relative F:  rows are seasons, columns are fleets
#_Fleet:  FISHERY1
#  1
# max totalcatch by fleet (-1 to have no max)
 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
# max totalcatch by area (-1 to have no max)
 -1
# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)
 0 0 0 0 0 0 0 0 0 0 0
#_Conditional on >1 allocation group
# allocation fraction for each of: 0 allocation groups
# no allocation groups
280 # Number of forecast catch levels to input (else calc catch from forecast F) 
2 # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)
# Input fixed catch values
# from E:\iotc\ALB2016\datasets\CEdata\ALB2016_catches_by_fishery_v2_working.xlsx
#Year Seas Fleet Catch(or_F) 
2015	1	1	2466
2015	2	1	448
2015	3	1	288
2015	4	1	2808
2015	1	2	338
2015	2	2	302
2015	3	2	394
2015	4	2	462
2015	1	3	409
2015	2	3	7971
2015	3	3	7441
2015	4	3	361
2015	1	4	499
2015	2	4	10114
2015	3	4	4218
2015	4	4	80
2015	1	7	100
2015	2	7	180
2015	3	7	17
2015	4	7	233
2015	1	8	19
2015	2	8	19
2015	3	8	19
2015	4	8	19
2015	1	9	124
2015	2	9	124
2015	3	9	124
2015	4	9	124
2016	1	1	2466
2016	2	1	448
2016	3	1	288
2016	4	1	2808
2016	1	2	338
2016	2	2	302
2016	3	2	394
2016	4	2	462
2016	1	3	409
2016	2	3	7971
2016	3	3	7441
2016	4	3	361
2016	1	4	499
2016	2	4	10114
2016	3	4	4218
2016	4	4	80
2016	1	7	100
2016	2	7	180
2016	3	7	17
2016	4	7	233
2016	1	8	19
2016	2	8	19
2016	3	8	19
2016	4	8	19
2016	1	9	124
2016	2	9	124
2016	3	9	124
2016	4	9	124
2017	1	1	2466
2017	2	1	448
2017	3	1	288
2017	4	1	2808
2017	1	2	338
2017	2	2	302
2017	3	2	394
2017	4	2	462
2017	1	3	409
2017	2	3	7971
2017	3	3	7441
2017	4	3	361
2017	1	4	499
2017	2	4	10114
2017	3	4	4218
2017	4	4	80
2017	1	7	100
2017	2	7	180
2017	3	7	17
2017	4	7	233
2017	1	8	19
2017	2	8	19
2017	3	8	19
2017	4	8	19
2017	1	9	124
2017	2	9	124
2017	3	9	124
2017	4	9	124
2018	1	1	2466
2018	2	1	448
2018	3	1	288
2018	4	1	2808
2018	1	2	338
2018	2	2	302
2018	3	2	394
2018	4	2	462
2018	1	3	409
2018	2	3	7971
2018	3	3	7441
2018	4	3	361
2018	1	4	499
2018	2	4	10114
2018	3	4	4218
2018	4	4	80
2018	1	7	100
2018	2	7	180
2018	3	7	17
2018	4	7	233
2018	1	8	19
2018	2	8	19
2018	3	8	19
2018	4	8	19
2018	1	9	124
2018	2	9	124
2018	3	9	124
2018	4	9	124
2019	1	1	2466
2019	2	1	448
2019	3	1	288
2019	4	1	2808
2019	1	2	338
2019	2	2	302
2019	3	2	394
2019	4	2	462
2019	1	3	409
2019	2	3	7971
2019	3	3	7441
2019	4	3	361
2019	1	4	499
2019	2	4	10114
2019	3	4	4218
2019	4	4	80
2019	1	7	100
2019	2	7	180
2019	3	7	17
2019	4	7	233
2019	1	8	19
2019	2	8	19
2019	3	8	19
2019	4	8	19
2019	1	9	124
2019	2	9	124
2019	3	9	124
2019	4	9	124
2020	1	1	2466
2020	2	1	448
2020	3	1	288
2020	4	1	2808
2020	1	2	338
2020	2	2	302
2020	3	2	394
2020	4	2	462
2020	1	3	409
2020	2	3	7971
2020	3	3	7441
2020	4	3	361
2020	1	4	499
2020	2	4	10114
2020	3	4	4218
2020	4	4	80
2020	1	7	100
2020	2	7	180
2020	3	7	17
2020	4	7	233
2020	1	8	19
2020	2	8	19
2020	3	8	19
2020	4	8	19
2020	1	9	124
2020	2	9	124
2020	3	9	124
2020	4	9	124
2021	1	1	2466
2021	2	1	448
2021	3	1	288
2021	4	1	2808
2021	1	2	338
2021	2	2	302
2021	3	2	394
2021	4	2	462
2021	1	3	409
2021	2	3	7971
2021	3	3	7441
2021	4	3	361
2021	1	4	499
2021	2	4	10114
2021	3	4	4218
2021	4	4	80
2021	1	7	100
2021	2	7	180
2021	3	7	17
2021	4	7	233
2021	1	8	19
2021	2	8	19
2021	3	8	19
2021	4	8	19
2021	1	9	124
2021	2	9	124
2021	3	9	124
2021	4	9	124
2022	1	1	2466
2022	2	1	448
2022	3	1	288
2022	4	1	2808
2022	1	2	338
2022	2	2	302
2022	3	2	394
2022	4	2	462
2022	1	3	409
2022	2	3	7971
2022	3	3	7441
2022	4	3	361
2022	1	4	499
2022	2	4	10114
2022	3	4	4218
2022	4	4	80
2022	1	7	100
2022	2	7	180
2022	3	7	17
2022	4	7	233
2022	1	8	19
2022	2	8	19
2022	3	8	19
2022	4	8	19
2022	1	9	124
2022	2	9	124
2022	3	9	124
2022	4	9	124
2023	1	1	2466
2023	2	1	448
2023	3	1	288
2023	4	1	2808
2023	1	2	338
2023	2	2	302
2023	3	2	394
2023	4	2	462
2023	1	3	409
2023	2	3	7971
2023	3	3	7441
2023	4	3	361
2023	1	4	499
2023	2	4	10114
2023	3	4	4218
2023	4	4	80
2023	1	7	100
2023	2	7	180
2023	3	7	17
2023	4	7	233
2023	1	8	19
2023	2	8	19
2023	3	8	19
2023	4	8	19
2023	1	9	124
2023	2	9	124
2023	3	9	124
2023	4	9	124
2024	1	1	2466
2024	2	1	448
2024	3	1	288
2024	4	1	2808
2024	1	2	338
2024	2	2	302
2024	3	2	394
2024	4	2	462
2024	1	3	409
2024	2	3	7971
2024	3	3	7441
2024	4	3	361
2024	1	4	499
2024	2	4	10114
2024	3	4	4218
2024	4	4	80
2024	1	7	100
2024	2	7	180
2024	3	7	17
2024	4	7	233
2024	1	8	19
2024	2	8	19
2024	3	8	19
2024	4	8	19
2024	1	9	124
2024	2	9	124
2024	3	9	124
2024	4	9	124
#
999 # verify end of input 
