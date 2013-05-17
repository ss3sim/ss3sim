#V3.24f
#_data_and_control_files: simple_cod.dat // simple_cod.ctl
1 #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
#_Cond 0  #  N recruitment designs goes here if N_GP*nseas*area>1
#_Cond 0  #  placeholder for recruitment interaction request
#_Cond 1 1 1  # example recruitment design element for GP=1, seas=1, area=1
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
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented
1 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=read fec and wt from wtatage.ss   
#_Age_Maturity by growth pattern
2 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO	HI      INIT        PRIOR       PR     SD      PHASE  env    use      dev     dev    dev    Block  Block
#                                      type                   var    dev     minyr   maxyr  stddev          Fxn
0.01    1.8     0.2         0.1         -1     0.8     -3      0	1     1913    2012    0.05	0	0	# NatM_p_1_Fem_GP_1 
10      80      20          30.8        -1     0.2     -2      0	0	0	0	0	0	0	# L_at_Amin_Fem_GP_1
25      250   132.0       120.1         -1     0.2     -5      0	0	0	0	0	0	0	# L_at_Amax_Fem_GP_1
0.01    2       0.2        0.25         -1     0.8     -2      0	0	0	0	0	0	0	# VonBert_K_Fem_GP_1 
-0.01   0.5     0.1         0.1         -1     0.8     -3      0	0	0	0	0	0	0	# CV_young_Fem_GP_1
0.01    0.5     0.1         0.1         -1     0.8     -5      0	0	0	0	0	0	0	# CV_old_Fem_GP_1
0       3   0.0000068   0.0000068       -1     0       -1      0	0	0	0	0	0	0	# Wtlen_1_Fem
2.5     3.5     3.101       3.101       -1     0.2     -3      0	0	0	0	0	0	0	# Wtlen_2_Fem
10      50     38.18        0           -1     0       -3      0	0	0	0	0	0	0	# Mat50%_Fem -     
-2      2      -0.276       0           -1     0       -3      0	0	0	0	0	0	0	# Mat_slope_Fem    
-3      3       1           0           -1     0       -3      0	0	0	0	0	0	0	# Eggs/kg_inter_Fem
-3      4       0           0           -1     0       -3      0	0	0	0	0	0	0	# Eggs/kg_slope_wt_Fem
-4      4       0           0           -1     0       -4      0	0	0	0	0	0	0	# RecrDist_GP_1
-4      4       0           0           -1     0       -4      0	0	0	0	0	0	0	# RecrDist_Area_1
-4      4       0           0           -1     0       -4      0	0	0	0	0	0	0	# RecrDist_Seas_1
-4      4       1           0           -1     0       -4      0	0	0	0	0	0	0	# CohortGrowDev
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
4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm
#_LO	HI  INIT    PRIOR  PR_type  SD      PHASE
  10    20  18.7     10.3   -1       10      1    # SR_LN(R0)
  0.2   1   0.65     0.7    -1       0.05    4    # SR_BH_steep
  0     2   0        0.8    -1       0.8    -5    # SR_sigmaR
 -5     5   0        0      -1       1      -3    # SR_envlink
 -5     5   0        0      -1       1      -4    # SR_R1_offset
  0     0   0        0      -1       0      -99   # SR_autocorr
0  #_SR_env_link
0  #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1  #do_recdev:  0=none; 1=devvector; 2=simple deviations
1913 # first year of main recr_devs; early devs can preceed this era - ##### NOTE: May need to be changed
2012 # last year of main recr_devs; forecast devs start in following year - ##### NOTE: May need to be changed
-2  #_recdev phase 
1    # (0/1) to read 13 advanced options
0    #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-4   #_recdev_early_phase
0    #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1    #_lambda for Fcast_recr_like occurring before endyr+1
1949 #_last_early_yr_nobias_adj_in_MPD
1982 #_first_yr_fullbias_adj_in_MPD
2009 #_last_yr_fullbias_adj_in_MPD
2010 #_first_recent_yr_nobias_adj_in_MPD
0    #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs) - 
0    #_period of cycles in recruitment (N parms read below)
-5   #min rec_dev
5    #max rec_dev
0    #_read_recdevs

#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#
#Fishing Mortality info 
0.3 # F ballpark for tuning early phases
-2001 # F ballpark year (neg value to disable)
2 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
0.4 1 100 # overall start F value; overall phase; N detailed inputs to read
# N iterations for tuning F in hybrid method (recommend 3 to 7)

#Fleet  Year   Seas    F_value  SE     Phase
1	1913	1	0.004	0.005	-1
1	1914	1	0.008	0.005	-1
1	1915	1	0.012	0.005	-1
1	1916	1	0.016	0.005	-1
1	1917	1	0.02	0.005	-1
1	1918	1	0.024	0.005	-1
1	1919	1	0.028	0.005	-1
1	1920	1	0.032	0.005	-1
1	1921	1	0.036	0.005	-1
1	1922	1	0.04	0.005	-1
1	1923	1	0.044	0.005	-1
1	1924	1	0.048	0.005	-1
1	1925	1	0.052	0.005	-1
1	1926	1	0.056	0.005	-1
1	1927	1	0.06	0.005	-1
1	1928	1	0.064	0.005	-1
1	1929	1	0.068	0.005	-1
1	1930	1	0.072	0.005	-1
1	1931	1	0.076	0.005	-1
1	1932	1	0.08	0.005	-1
1	1933	1	0.084	0.005	-1
1	1934	1	0.088	0.005	-1
1	1935	1	0.092	0.005	-1
1	1936	1	0.096	0.005	-1
1	1937	1	0.1	0.005	-1
1	1938	1	0.104	0.005	-1
1	1939	1	0.108	0.005	-1
1	1940	1	0.112	0.005	-1
1	1941	1	0.116	0.005	-1
1	1942	1	0.12	0.005	-1
1	1943	1	0.124	0.005	-1
1	1944	1	0.128	0.005	-1
1	1945	1	0.132	0.005	-1
1	1946	1	0.136	0.005	-1
1	1947	1	0.14	0.005	-1
1	1948	1	0.144	0.005	-1
1	1949	1	0.148	0.005	-1
1	1950	1	0.152	0.005	-1
1	1951	1	0.156	0.005	-1
1	1952	1	0.16	0.005	-1
1	1953	1	0.164	0.005	-1
1	1954	1	0.168	0.005	-1
1	1955	1	0.172	0.005	-1
1	1956	1	0.176	0.005	-1
1	1957	1	0.18	0.005	-1
1	1958	1	0.184	0.005	-1
1	1959	1	0.188	0.005	-1
1	1960	1	0.192	0.005	-1
1	1961	1	0.196	0.005	-1
1	1962	1	0.2	0.005	-1
1	1963	1	0.198	0.005	-1
1	1964	1	0.196	0.005	-1
1	1965	1	0.194	0.005	-1
1	1966	1	0.192	0.005	-1
1	1967	1	0.19	0.005	-1
1	1968	1	0.188	0.005	-1
1	1969	1	0.186	0.005	-1
1	1970	1	0.184	0.005	-1
1	1971	1	0.182	0.005	-1
1	1972	1	0.18	0.005	-1
1	1973	1	0.178	0.005	-1
1	1974	1	0.176	0.005	-1
1	1975	1	0.174	0.005	-1
1	1976	1	0.172	0.005	-1
1	1977	1	0.17	0.005	-1
1	1978	1	0.168	0.005	-1
1	1979	1	0.166	0.005	-1
1	1980	1	0.164	0.005	-1
1	1981	1	0.162	0.005	-1
1	1982	1	0.16	0.005	-1
1	1983	1	0.158	0.005	-1
1	1984	1	0.156	0.005	-1
1	1985	1	0.154	0.005	-1
1	1986	1	0.152	0.005	-1
1	1987	1	0.15	0.005	-1
1	1988	1	0.148	0.005	-1
1	1989	1	0.146	0.005	-1
1	1990	1	0.144	0.005	-1
1	1991	1	0.142	0.005	-1
1	1992	1	0.14	0.005	-1
1	1993	1	0.138	0.005	-1
1	1994	1	0.136	0.005	-1
1	1995	1	0.134	0.005	-1
1	1996	1	0.132	0.005	-1
1	1997	1	0.13	0.005	-1
1	1998	1	0.128	0.005	-1
1	1999	1	0.126	0.005	-1
1	2000	1	0.124	0.005	-1
1	2001	1	0.122	0.005	-1
1	2002	1	0.12	0.005	-1
1	2003	1	0.118	0.005	-1
1	2004	1	0.116	0.005	-1
1	2005	1	0.114	0.005	-1
1	2006	1	0.112	0.005	-1
1	2007	1	0.11	0.005	-1
1	2008	1	0.108	0.005	-1
1	2009	1	0.106	0.005	-1
1	2010	1	0.104	0.005	-1
1	2011	1	0.102	0.005	-1
1	2012	1	0.1	0.005	-1

#_initial_F_parms
#_LO  HI  INIT   PRIOR  PR_type   SD   PHASE
   0  2   0.087  0.01     0       99    -1    # InitF_1Fishery  [ PR_type-1 ]???

#_Q_setup
 # Q_type options:  <0=mirror, 0=float_nobiasadj, 1=float_biasadj, 2=parm_nobiasadj, 3=parm_w_random_dev, 4=parm_w_randwalk, 5=mean_unbiased_float_assign_to_parm
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
0 0 0 2 # 1 FISHERY
0 0 0 2 # 2 SURVEY
0 0 0 2 # 3 CPUE

#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any)
#LO  HI    INIT    PRIOR  PR_type  SD   PHASE
-3   3      0       0       -1     99     5    # Q_base_1_TheFleet
-3   3      0       0       -1     99     5    # Q_base_2_Survey1
-3   3      0       0       -1     99     5    # Q_base_3_Survey2

#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
1  0  0  0 # 1 FISHERY
1  0  0  0 # 2 SURVEY
15 0  0  1 # 3 CPUE - Mirrors Fishery

#_age_selex_types
#_Pattern ___ Male Special
10 0 0 0  # 1 Fishery 
10 0 0 0  # 2 SURVEY
10 0 0 0  # 3 CPUE - Mirrors Fishery

#_LO    HI      INIT      PRIOR     PR     SD   PHASE   env     use    dev     dev     dev  Block   Block 
 20      150     38.18     0         -1     99    -1      0       0      0       0       0	0	0	# SizeSel_1P_1_Fishery 
 0       300     10.63     0         -1     99    -1      0       0      0       0       0	0	0	# SizeSel_1P_2_Fishery  
 20      150     30.54     0         -1     99    -1      0       0      0       0       0	0	0	# SizeSel_2P_1_SURVEY  - 0.8*size at 50% selectivity from fishery -decided in class
 0       300     10.63     0         -1     99    -1      0       0      0       0       0	0	0	# SizeSel_2P_2_SURVEY 

# #_LO    HI      INIT      PRIOR     PR          SD    PHASE   env     use    dev     dev     dev    Block   Block - NOTE: Transtitioned to age-specific selectivity
# 20      150     60        0         -1          99    -1      0       0      0       0       0	0	0	# SizeSel_1P_1_Fishery
# 0       300     30        0         -1          99    -1      0       0      0       0       0	0	0	# SizeSel_1P_2_Fishery  
# 20      150     48        0         -1          99    -1      0       0      0       0       0	0	0	# SizeSel_2P_1_SURVEY  - 0.8*size at 50% selectivity from fishery -decided in class
# 0       300     30        0         -1          99    -1      0       0      0       0       0	0	0	# SizeSel_2P_2_SURVEY 


#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)

# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 
  0   0    0    #_add_to_survey_CV
  0   0    0    #_add_to_discard_stddev
  0   0    0    #_add_to_bodywt_CV
  1   1    1    #_mult_by_lencomp_N
  1   1    1    #_mult_by_agecomp_N
  1   1    1    #_mult_by_size-at-age_N\
#
4 #_maxlambdaphase
1 #_sd_offset
#
0 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
#like_comp fleet/survey  phase  value  sizefreq_method

# lambdas (for info only; columns are phases)
#  0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 #_CPUE/survey:_2
#  0 0 0 0 #_CPUE/survey:_3
#  1 1 1 1 #_agecomp:_1
#  1 1 1 1 #_agecomp:_2
#  0 0 0 0 #_agecomp:_3
#  1 1 1 1 #_size-age:_1
#  0 0 0 0 #_size-age:_2
#  0 0 0 0 #_size-age:_3
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  1 1 1 1 #_parameter-priors
#  1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 #_crashPenLambda

0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999



