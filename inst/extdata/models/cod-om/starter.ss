#V3.24O
#C starter file written by R function SS_writestarter
#C rerun model to get more complete formatting in starter.ss_new
#C should work with SS version: SSv3.10b_or_later
#C file write time: 2015-02-20 08:58:27
ss3.dat
ss3.ctl
1 # 0=use init values in control file; 1=use ss3.par
0 # run display detail (0,1,2)
1 # detailed age-structured reports in REPORT.SSO (0,1) 
0 # write detailed info from first call to echoinput.sso (0,1) 
0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)
0 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)
0 # Include prior_like for non-estimated parameters (0,1) 
1 # Use Soft Boundaries to aid convergence (0,1) (recommended)
2 # Number of datafiles to produce: 1st is input, 2nd is estimates, 3rd and higher are bootstrap
0 # Turn off estimation for parameters entering after this phase
0 # MCeval burn interval
1 # MCeval thin interval
0 # jitter initial parm value by this fraction
-1 # min yr for sdreport outputs (-1 for styr)
100 # max yr for sdreport outputs (-1 for endyr; -2 for endyr+Nforecastyrs
0 # N individual STD years 
#vector of year values 

0.0001 # final convergence criteria (e.g. 1.0e-04) 
0 # retrospective year relative to end year (e.g. -4)
1 # min age for calc of summary biomass
1 # Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel X*Bmsy; 3=rel X*B_styr
1 # Fraction (X) for Depletion denominator (e.g. 0.4)
4 # SPR_report_basis:  0=skip; 1=(1-SPR)/(1-SPR_tgt); 2=(1-SPR)/(1-SPR_MSY); 3=(1-SPR)/(1-SPR_Btarget); 4=rawSPR
1 # F_report_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Frates); 4=true F for range of ages
#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4
0 # F_report_basis: 0=raw; 1=F/Fspr; 2=F/Fmsy ; 3=F/Fbtgt
999 # check value for end of file
