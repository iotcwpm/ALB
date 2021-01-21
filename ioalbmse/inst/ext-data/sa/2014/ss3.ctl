#V3.24f
#_data_and_control_files: abt.dat // abt.ctl
#_SS-V3.24f-safe-Win64;_08/03/2012;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11
1  #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
4 #  number of recruitment assignments (overrides GP*area*seas parameter values) 
0 # recruitment interaction requested
#GP seas area for each recruitment assignment
 1 1 1
 1 2 1
 1 3 1
 1 4 1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
0.5 #_fracfemale 
4 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
 0.4 0.3641 0.3283 0.2924 0.2566 0.2207 0.2207 0.2207 0.2207 0.2207 0.2207 0.2207 0.2207 0.2207 0.2207
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented
1 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=read fec and wt from wtatage.ss
#_Age_Maturity by growth pattern
0 0 2.44477410705495e-15 0.000126930488211787 0.0885235134330865 0.466233711152446 0.745856032826186 0.88104458119241 0.943739660712201 0.973162337976756 0.987136436219432 0.993820846084611 0.997030307122203 0.998574204278824 0.99931757569506
4 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
# 
10 60 43.806 32 -1 99 -5 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
100 150 108.56 125 -1 99 -5 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
0.01 0.4 0.2914 0.149 -1 99 -5 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
0.01 0.3 0.1 0.1 -1 99 -5 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
0.01 0.3 0.1 0.08 -1 99 -5 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
-2 2 5.691e-005 8.7e-005 -1 99 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem
-2 4 2.7514 2.67 -1 99 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem
1 10 5 5 -1 99 -3 0 0 0 0 0 0 0 # Mat50%_Fem
-5 5 -3.746 -3.746 -1 99 -3 0 0 0 0 0 0 0 # Mat_slope_Fem
0 3 1 1 -1 99 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
0 3 0 0 -1 99 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
-4 4 0 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_GP_1
-4 4 0 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_Area_1
-10 4 -10 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_Seas_1
-10 4 -10 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_Seas_2
-10 4 -10 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_Seas_3
-10 4 0 1 -1 99 -3 0 0 0 0 0 0 0 # RecrDist_Seas_4
-4 4 1 1 -1 99 -3 0 0 0 0 0 0 0 # CohortGrowDev
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters
#
#_Cond 0  #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Cond -4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
 5 15 9.35244 11.4 -1 99 1 # SR_LN(R0)
0.2	1	0.8	0.75	-1	99	-4	# SR_BH_steep
0	2	0.4	0.4	-1	99	-1	# SR_sigmaR
 -5 5 0 0 -1 99 -1 # SR_envlink
 -10 10 0 0 -1 99 -1 # SR_R1_offset
 0 0 0 0 -1 99 -1 # SR_autocorr
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1980 # first year of main recr_devs; early devs can preceed this era
2012 # last year of main recr_devs; forecast devs start in following year
5 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 -5 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1970 #_last_early_yr_nobias_adj_in_MPD
 1980 #_first_yr_fullbias_adj_in_MPD
 2007 #_last_yr_fullbias_adj_in_MPD
 2013 #_first_recent_yr_nobias_adj_in_MPD
 1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -5 #min rec_dev
 5 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#DisplayOnly 0.628023 # Main_RecrDev_1980
#DisplayOnly 0.135831 # Main_RecrDev_1981
#DisplayOnly 0.145053 # Main_RecrDev_1982
#DisplayOnly 0.244108 # Main_RecrDev_1983
#DisplayOnly -0.179272 # Main_RecrDev_1984
#DisplayOnly -0.217684 # Main_RecrDev_1985
#DisplayOnly -0.408093 # Main_RecrDev_1986
#DisplayOnly -0.387857 # Main_RecrDev_1987
#DisplayOnly -0.41213 # Main_RecrDev_1988
#DisplayOnly 0.705252 # Main_RecrDev_1989
#DisplayOnly -0.482541 # Main_RecrDev_1990
#DisplayOnly -0.323643 # Main_RecrDev_1991
#DisplayOnly -0.0126021 # Main_RecrDev_1992
#DisplayOnly 0.131166 # Main_RecrDev_1993
#DisplayOnly 0.0601587 # Main_RecrDev_1994
#DisplayOnly -0.0215959 # Main_RecrDev_1995
#DisplayOnly 0.133429 # Main_RecrDev_1996
#DisplayOnly 0.543976 # Main_RecrDev_1997
#DisplayOnly 0.433729 # Main_RecrDev_1998
#DisplayOnly -0.0392866 # Main_RecrDev_1999
#DisplayOnly -0.107198 # Main_RecrDev_2000
#DisplayOnly -0.250779 # Main_RecrDev_2001
#DisplayOnly -0.275822 # Main_RecrDev_2002
#DisplayOnly -0.199985 # Main_RecrDev_2003
#DisplayOnly -0.0930291 # Main_RecrDev_2004
#DisplayOnly 0.250793 # Main_RecrDev_2005
#
#Fishing Mortality info 
0.5 # F ballpark for tuning early phases
-2008 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
5  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 3 0 0 -1 99 -1 # InitF_1F1_JPN_LL_N
 0 1 0 0 -1 99 -1 # InitF_2F2_TWN_LL_N
 0 1 0 0 -1 99 -1 # InitF_3F3_PS_N
 0 1 0 0 -1 99 -1 # InitF_4F4_Other_N
 0 1 0 0 -1 99 -1 # InitF_5F5_JPN_LL_S
 0 1 0 0 -1 99 -1 # InitF_6F6_TWN_LL_S
 0 1 0 0 -1 99 -1 # InitF_7F7_Driftnet_S
#
#_Q_setup
 # Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
0 0 0 0 # 1 F1_JPN_LL_N
0 0 0 0 # 2 F2_TWN_LL_N
0 0 0 0 # 3 F3_PS_N
0 0 0 0 # 4 F4_Other_N
0 0 0 0 # 5 F5_JPN_LL_S
0 0 0 0 # 6 F6_TWN_LL_S
0 0 0 0 # 7 F7_Drift
0 0 0 0 # 8 S1_JPLL_N
0 0 0 0 # 9 S2_TWLL_N
0 0 0 0 # 10 S5_JPLL_S
  0 0 0 0 # 11 S4_TWN_LL_S
#
#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any)
#  0 1 0 0 -1 99 2 # Qvar1
#
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
24 0 0 0 # 1 F1_JPN_LL_N
24 0 0 0 # 2 F2_TWN_LL_N
24 0 0 0 # 3 F3_PS_N
24 0 0 0 # 4 F4_Other_N
24 0 0 0 # 5 F5_JPN_LL_S
24 0 0 0 # 6 F6_TWN_LL_S
24 0 0 0 # 7 F7_Drift
5 0 0 1 # 8 S1_JPLL_N
5 0 0 2 # 9 S2_TWLL_N
5 0 0 5 # 10 S5_JPLL_S
5 0 0 6 # 11 S6_TWLL_S
#
#_age_selex_types
#_Pattern ___ Male Special
10 0 0 0 # 1 F1_JPN_LL_N
10 0 0 0 # 2 F2_TWN_LL_N
10 0 0 0 # 3 F3_PS_N
10 0 0 0 # 4 F4_Other_N
10 0 0 0 # 5 F5_JPN_LL_S
10 0 0 0 # 6 F6_TWN_LL_S
10 0 0 0 # 7 F7_Drift
10 0 0 0 # 8 S1_JPLL_N
10 0 0 0 # 9 S2_TWLL_N
10 0 0 0 # 10 S5_JPLL_S
10 0 0 0 # 11 S6_TWLL_S
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
  30 130 105.234 66 -1 99 2 0 0 0 0 0 0 0 # SizeSel_1P_1_F1_JPN_LL_N
  -9 4 -8.95385 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_1P_2_F1_JPN_LL_N
  -1 9 6.13059 4 -1 99 3 0 0 0 0 0 0 0 # SizeSel_1P_3_F1_JPN_LL_N
  -1 9 3.74418 5 -1 99 4 0 0 0 0 0 0 0 # SizeSel_1P_4_F1_JPN_LL_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_1P_5_F1_JPN_LL_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_1P_6_F1_JPN_LL_N
  30 130 94.0089 66 -1 99 2 0 0 0 0 0 0 0 # SizeSel_2P_1_F2_TWN_LL_N
  -9 4 -1.2197 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_2P_2_F2_TWN_LL_N
  -1 9 6.13098 4 -1 99 3 0 0 0 0 0 0 0 # SizeSel_2P_3_F2_TWN_LL_N
  -1 9 5.56613 5 -1 99 4 0 0 0 0 0 0 0 # SizeSel_2P_4_F2_TWN_LL_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_2P_5_F2_TWN_LL_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_2P_6_F2_TWN_LL_N
  30 130 107.667 75 -1 99 2 0 0 0 0 0 0 0 # SizeSel_3P_1_F3_PS_N
  -9 4 -8.98414 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_3P_2_F3_PS_N
  -1 9 4.17611 6 -1 99 3 0 0 0 0 0 0 0 # SizeSel_3P_3_F3_PS_N
  -1 9 3.67232 3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_3P_4_F3_PS_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_3P_5_F3_PS_N
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_3P_6_F3_PS_N
  30 130 32.3744 89 -1 99 2 0 0 0 0 0 0 0 # SizeSel_4P_1_F4_Other_N
  -9 4 -1.46831 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_4P_2_F4_Other_N
  -4 9 0.18144 6 -1 99 3 0 0 0 0 0 0 0 # SizeSel_4P_3_F4_Other_N
  -4 9 4.06893 3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_4P_4_F4_Other_N
  -999 -999 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_4P_F4_Other_N
  -999 -999 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_4P_F4_Other_N
  30 130 105.234 66 -1 99 2 0 0 0 0 0 0 0 # SizeSel_5P_1_F5_JPN_LL_S
  -9 4 -8.95385 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_5P_2_F5_JPN_LL_S
  -1 9 6.13059 4 -1 99 3 0 0 0 0 0 0 0 # SizeSel_5P_3_F5_JPN_LL_S
  -1 9 3.74418 5 -1 99 4 0 0 0 0 0 0 0 # SizeSel_5P_4_F5_JPN_LL_S
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_5P_5_F5_JPN_LL_S
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_5P_6_F5_JPN_LL_S
  30 130 94.0089 66 -1 99 2 0 0 0 0 0 0 0 # SizeSel_6P_1_F6_TWN_LL_S
  -9 4 -1.2197 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_6P_2_F6_TWN_LL_S
  -1 9 6.13098 4 -1 99 3 0 0 0 0 0 0 0 # SizeSel_6P_3_F6_TWN_LL_S
  -1 9 5.56613 5 -1 99 4 0 0 0 0 0 0 0 # SizeSel_6P_4_F6_TWN_LL_S
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_6P_5_F6_TWN_LL_S
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_6P_6_F6_TWN_LL_S
  30 130 84.1817 66 -1 99 2 0 0 0 0 0 0 0 # SizeSel_7P_1_F7_Drift
  -9 4 -2.65728 -3 -1 99 4 0 0 0 0 0 0 0 # SizeSel_7P_2_F7_Drift
  -1 9 5.7648 4 -1 99 3 0 0 0 0 0 0 0 # SizeSel_7P_3_F7_Drift
  -1 9 5.09633 5 -1 99 4 0 0 0 0 0 0 0 # SizeSel_7P_4_F7_Drift
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_7P_5_F7_Drift
  -999 -600 -999 -5 -1 99 -2 0 0 0 0 0 0 0 # SizeSel_7P_6_F7_Drift
  1 80 1 1 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_8P_1_S1_JPLL_N
  -80 -80 -80 -80 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_8P_2_S1_JPLL_N
  1 80 1 1 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_9P_1_S2_TWLL_N
  -80 -80 -80 -80 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_9P_2_S3_TWLL_N
  1 80 1 1 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_10P_1_S3_JPLL_S
  -80 -80 -80 -80 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_10P_2_S3_JPLL_S
  1 80 1 1 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_11P_1_S4_TWLL_S
  -80 -80 -80 -80 -1 99 -4 0 0 0 0 0 0 0 # SizeSel_11P_2_S4_TWLL_S
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 4 5 6 
  0 0 0 0 0 0 0 0 0 0 0.1 #
0 0 0 0 0 0 0 0 0 0 0 #
0 0 0 0 0 0 0 0 0 0 0 #
1 1 1 1 1 1 1 1 1 1 1 #
1 1 1 1 1 1 1 1 1 1 1 #
1 1 1 1 1 1 1 1 1 1 1 #
#
4 #_maxlambdaphase
1 #_sd_offset
#
15 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
#like_comp fleet/survey  phase  value  sizefreq_method
  1 8 1 0 1 
  1 9 1 0 1 
  1 10 1 0 1 
  1 11 1 1 1 
  4 1 1 1 1 #
  4 2 1 1 1 #
  4 3 1 1 1 #
  4 4 1 1 1 #
  4 5 1 1 1 #
  4 6 1 1 1 #
  4 7 1 1 1 #
  9 1 1 1 1 #
  11 1 1 0 1 #
  12 1 1 0 1 #
  13 1 1 100 1 #
#  0 0 0 0 #_CPUE/survey:_1
#  0 0 0 0 #_CPUE/survey:_2
#  0 0 0 0 #_CPUE/survey:_3
#  0 0 0 0 #_CPUE/survey:_4
#  0 0 0 0 #_CPUE/survey:_5
#  1 1 1 1 #_CPUE/survey:_6
#  1 1 1 1 #_lencomp:_1
#  1 1 1 1 #_lencomp:_2
#  1 1 1 1 #_lencomp:_3
#  1 1 1 1 #_lencomp:_4
#  1 1 1 1 #_lencomp:_5
#  0 0 0 0 #_lencomp:_6
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  0 0 0 0 #_parameter-priors
#  0 0 0 0 #_parameter-dev-vectors
#  100 100 100 100 #_crashPenLambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

