*!!!!!!!!!!!Change the path!!!!!!!!!
import excel "C:\Users\Андрей\Downloads\data BoL.xlsx", sheet("Sheet1") cellrange(A4:F256)firstrow

*A)
* setting the time for the sample and creating a log-file*
generate date = tm(1999m1) + _n-1
tsset date, monthly

log using Andrejs_Migunovs_BoL_analysis.log, replace

* summing up extra and intra Eurozone imports*
generate m_ea = m_ea_in + m_ea_ex

* seasonal adjustment of the variables p_lv and p_ea*
* here also other technique might be used, for example, TRAMO/SEATS, census X-12 or X-13, or some moving average method, I chose this one because my old stata version has some technical issue with census X-12*
tssmooth shwinters p_ea_sa = p_ea, iterate(20)
tssmooth shwinters p_lv_sa = p_lv, iterate(20)
label variable p_ea_sa "p_ea_sa"
label variable p_lv_sa "p_lv_sa"

* now the one can compare the seasonally adjusted data with the original variables and compare two obtained HIPC indices of the eurozone and Latvia*
tsline p_lv_sa p_lv if date > 650
tsline p_ea_sa p_ea if date > 650
tsline p_lv_sa p_ea_sa

* Putting the import data into logarithms and comparing them*
gen ln_m_ea = ln(m_ea)
gen ln_x_lv_ea = ln(x_lv_ea)
tsline ln_m_ea ln_x_lv_ea
gen ln_p_lv_sa = ln(p_lv_sa)
gen ln_p_ea_sa = ln(p_ea_sa)
tsline ln_p_lv_sa ln_p_ea_sa
*by the way, the trends are similar*


*B)
summarize date
*Here Dickey-Fuller test can be applied and the number of lags can be specified as well by using IC. Other DF test specifications can also be tested (noncons and trend), but I decide to stick to this model. The optimal model is chosen based on the smallest AIC and BIC values. I also tend to keep the same sample period to be able to compare IC. The truncation parameter is 0.75*T^(1/3) which is approximately 5 in this case
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa if date>473
estat ic
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa dl.ln_p_ea_sa if date>473
estat ic
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa dl.ln_p_ea_sa dl2.ln_p_ea_sa if date>473
estat ic
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa dl.ln_p_ea_sa dl2.ln_p_ea_sa dl3.ln_p_ea_sa if date>473
estat ic
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa dl.ln_p_ea_sa dl2.ln_p_ea_sa dl3.ln_p_ea_sa dl4.ln_p_ea_sa if date>473
estat ic
quietly regress d.ln_p_ea_sa l.ln_p_ea_sa dl.ln_p_ea_sa dl2.ln_p_ea_sa dl3.ln_p_ea_sa dl4.ln_p_ea_sa dl5.ln_p_ea_sa if date>473
estat ic
dfuller ln_p_ea_sa, lags(4) regress
*We can see that this variable is stationary with 10% significance with this concrete specification. It is up to the statistician to decide, which significance level to choose. Let us assume this is not enough.

quietly regress d.ln_p_lv_sa l.ln_p_lv_sa if date>473
estat ic
quietly regress d.ln_p_lv_sa l.ln_p_lv_sa dl.ln_p_lv_sa if date>473
estat ic
quietly regress d.ln_p_lv_sa l.ln_p_lv_sa dl.ln_p_lv_sa dl2.ln_p_lv_sa if date>473
estat ic
quietly regress d.ln_p_lv_sa l.ln_p_lv_sa dl.ln_p_lv_sa dl2.ln_p_lv_sa dl3.ln_p_lv_sa if date>473
estat ic
quietly regress d.ln_p_lv_sa l.ln_p_lv_sa dl.ln_p_lv_sa dl2.ln_p_lv_sa dl3.ln_p_lv_sa dl4.ln_p_lv_sa if date>473
estat ic
quietly regress d.ln_p_lv_sa l.ln_p_lv_sa dl.ln_p_lv_sa dl2.ln_p_lv_sa dl3.ln_p_lv_sa dl4.ln_p_lv_sa dl5.ln_p_lv_sa if date>473
estat ic
dfuller ln_p_lv_sa, lag(4) regress

quietly regress d.ln_m_ea l.ln_m_ea if date>473
estat ic
quietly regress d.ln_m_ea l.ln_m_ea dl.ln_m_ea if date>473
estat ic
quietly regress d.ln_m_ea l.ln_m_ea dl.ln_m_ea dl2.ln_m_ea if date>473
estat ic
quietly regress d.ln_m_ea l.ln_m_ea dl.ln_m_ea dl2.ln_m_ea dl3.ln_m_ea if date>473
estat ic
quietly regress d.ln_m_ea l.ln_m_ea dl.ln_m_ea dl2.ln_m_ea dl3.ln_m_ea dl4.ln_m_ea if date>473
estat ic
quietly regress d.ln_m_ea l.ln_m_ea dl.ln_m_ea dl2.ln_m_ea dl3.ln_m_ea dl4.ln_m_ea dl5.ln_m_ea if date>473
estat ic
dfuller ln_m_ea, lags(2) regress

quietly regress d.ln_x_lv_ea l.ln_x_lv_ea if date>473
estat ic
quietly regress d.ln_x_lv_ea l.ln_x_lv_ea dl.ln_x_lv_ea if date>473
estat ic
quietly regress d.ln_x_lv_ea l.ln_x_lv_ea dl.ln_x_lv_ea dl2.ln_x_lv_ea if date>473
estat ic
quietly regress d.ln_x_lv_ea l.ln_x_lv_ea dl.ln_x_lv_ea dl2.ln_x_lv_ea dl3.ln_x_lv_ea if date>473
estat ic
quietly regress d.ln_x_lv_ea l.ln_x_lv_ea dl.ln_x_lv_ea dl2.ln_x_lv_ea dl3.ln_x_lv_ea dl4.ln_x_lv_ea if date>473
estat ic
quietly regress d.ln_x_lv_ea l.ln_x_lv_ea dl.ln_x_lv_ea dl2.ln_x_lv_ea dl3.ln_x_lv_ea dl4.ln_x_lv_ea dl5.ln_x_lv_ea if date>473
estat ic
dfuller ln_x_lv_ea, lags(1) regress

* as we can see, the data is non-stationary at I(0). We also can test the stationarity for the first difference*
*Generating the first difference variable*
generate dln_p_ea_sa = d.ln_p_ea_sa
generate dln_p_lv_sa = d.ln_p_lv_sa
generate dln_x_lv_ea = d.ln_x_lv_ea
generate dln_m_ea = d.ln_m_ea

*Performing the tests*

quietly regress d.dln_p_ea_sa l.dln_p_ea_sa if date>474
estat ic
quietly regress d.dln_p_ea_sa l.dln_p_ea_sa dl.dln_p_ea_sa if date>474
estat ic
quietly regress d.dln_p_ea_sa l.dln_p_ea_sa dl.dln_p_ea_sa dl2.dln_p_ea_sa if date>474
estat ic
quietly regress d.dln_p_ea_sa l.dln_p_ea_sa dl.dln_p_ea_sa dl2.dln_p_ea_sa dl3.dln_p_ea_sa if date>474
estat ic
quietly regress d.dln_p_ea_sa l.dln_p_ea_sa dl.dln_p_ea_sa dl2.dln_p_ea_sa dl3.dln_p_ea_sa dl4.dln_p_ea_sa if date>474
estat ic
quietly regress d.dln_p_ea_sa l.dln_p_ea_sa dl.dln_p_ea_sa dl2.dln_p_ea_sa dl3.dln_p_ea_sa dl4.dln_p_ea_sa dl5.dln_p_ea_sa if date>474
estat ic
dfuller dln_p_ea_sa, lags(5) regress

quietly regress d.dln_p_lv_sa l.dln_p_lv_sa if date>474
estat ic
quietly regress d.dln_p_lv_sa l.dln_p_lv_sa dl.dln_p_lv_sa if date>474
estat ic
quietly regress d.dln_p_lv_sa l.dln_p_lv_sa dl.dln_p_lv_sa dl2.dln_p_lv_sa if date>474
estat ic
quietly regress d.dln_p_lv_sa l.dln_p_lv_sa dl.dln_p_lv_sa dl2.dln_p_lv_sa dl3.dln_p_lv_sa if date>474
estat ic
quietly regress d.dln_p_lv_sa l.dln_p_lv_sa dl.dln_p_lv_sa dl2.dln_p_lv_sa dl3.dln_p_lv_sa dl4.dln_p_lv_sa if date>474
estat ic
quietly regress d.dln_p_lv_sa l.dln_p_lv_sa dl.dln_p_lv_sa dl2.dln_p_lv_sa dl3.dln_p_lv_sa dl4.dln_p_lv_sa dl5.dln_p_lv_sa if date>474
estat ic
dfuller dln_p_lv_sa, lags(3) regress

quietly regress d.dln_x_lv_ea l.dln_x_lv_ea if date>474
estat ic
quietly regress d.dln_x_lv_ea l.dln_x_lv_ea dl.dln_x_lv_ea if date>474
estat ic
quietly regress d.dln_x_lv_ea l.dln_x_lv_ea dl.dln_x_lv_ea dl2.dln_x_lv_ea if date>474
estat ic
quietly regress d.dln_x_lv_ea l.dln_x_lv_ea dl.dln_x_lv_ea dl2.dln_x_lv_ea dl3.dln_x_lv_ea if date>474
estat ic
quietly regress d.dln_x_lv_ea l.dln_x_lv_ea dl.dln_x_lv_ea dl2.dln_x_lv_ea dl3.dln_x_lv_ea dl4.dln_x_lv_ea if date>474
estat ic
quietly regress d.dln_x_lv_ea l.dln_x_lv_ea dl.dln_x_lv_ea dl2.dln_x_lv_ea dl3.dln_x_lv_ea dl4.dln_x_lv_ea dl5.dln_x_lv_ea if date>474
estat ic
dfuller dln_x_lv_ea, lags(0) regress


quietly regress d.dln_m_ea l.dln_m_ea if date>474
estat ic
quietly regress d.dln_m_ea l.dln_m_ea dl.dln_m_ea if date>474
estat ic
quietly regress d.dln_m_ea l.dln_m_ea dl.dln_m_ea dl2.dln_m_ea if date>474
estat ic
quietly regress d.dln_m_ea l.dln_m_ea dl.dln_m_ea dl2.dln_m_ea dl3.dln_m_ea if date>474
estat ic
quietly regress d.dln_m_ea l.dln_m_ea dl.dln_m_ea dl2.dln_m_ea dl3.dln_m_ea dl4.dln_m_ea if date>474
estat ic
quietly regress d.dln_m_ea l.dln_m_ea dl.dln_m_ea dl2.dln_m_ea dl3.dln_m_ea dl4.dln_m_ea dl5.dln_m_ea if date>474
estat ic
dfuller dln_m_ea, lags(1) regress

*Here all the variables are stationary*

*C)
*Now we can see the correlation graphs which would give us some idea of which model is better, AR(1) or AR(2) - it seems that AR(1) is better*
pac dln_x_lv_ea
corrgram dln_x_lv_ea, lags(5)
summarize date

*Now we can compare the models and use information criteria to make a forecast, however, the sample size should be the same*
var dln_x_lv_ea if date>470 & date<708, lag(1)
estat ic
fcast compute AR1, step(12) nose
*predict AR1, dynamic(708) 

*VAR with 1 variable is the same as AR
var dln_x_lv_ea if date>469 & date<708, lag(1 2)
estat ic
fcast compute AR2, step(12) nose
*predict AR2_u, dynamic(708) residual
label variable AR1 "AR1"
label variable AR2 "AR2"
tsline AR1 AR2 dln_x_lv_ea if date>700

*** HAC does not matter for forecasting, however, this regression can be substituted by newey command if the one is interested in correct standard errors and significance results'

*D)
*To choose the parameter of truncation, I again use the following formula
display 0.75*250^(1/3)
*4.7247039
* I test the models using IC again. There are, of course more possible models to test, but I tend to add more lags of one variable, choose the most appropriate model by IC, and then add lags to the second variable and choose the most appropriate model by IC
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l.dln_m_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea  if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea l5.dln_m_ea if date>tm(1999m6)
estat ic

*The most appropriate model contains five lags 

regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l2.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea l5.dln_m_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l2.dln_x_lv_ea l3.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea l5.dln_m_ea if date>tm(1999m6)
estat ic
regress dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l2.dln_x_lv_ea l3.dln_x_lv_ea l4.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea l5.dln_m_ea if date>tm(1999m6)
estat ic

display (0.993+0.142+0.470+0.327+0.390)/(1-(-0.518)-(-0.282)-(-0.217)-(-0.128))
*1.0825175
*We can see that AIC criteria gives ambiguous results (the criteria "jumps" from one regression to another), so I believe that the BIC criteria should be applied in this case because it punishes the model stronger. As the BIC results show, the right model to use is the model with 1 export lag and eurozone's current import with 0 lags*

*As the best model has been chosen, it should be checked on the whole sample and tested further. The approach with simple regress command might suffer from heteroscedasticity or autocorellation - although heteroscedasticity can be eliminated by using robust command, autocorrelation might still be a problem. That is why I decide to use newey command

*Now we can run the Newey regression using the calculated number of lags(5)
newey dln_x_lv_ea dln_m_ea l.dln_x_lv_ea l2.dln_x_lv_ea l3.dln_x_lv_ea l4.dln_x_lv_ea l.dln_m_ea l2.dln_m_ea l3.dln_m_ea l4.dln_m_ea l5.dln_m_ea, lag(5)

*We can see that there is some positive autocorrelation, so the Newey-West HAC was the right approach to use`*
predict ardl_u, residual
corrgram ardl_u, lags(10)


*Now we can calculate the long-term coefficient using the formula*
display (0.993+0.142+0.470+0.327+0.390)/(1-(-0.518)-(-0.282)-(-0.217)-(-0.128))
*1.0825175
*In the long run, if the eurozone import growth increases by 1 percentage point, the growth of Latvian exports to the eurozone will increase by 1.083 pp. (value from the previous expression)*



*E)

newey ln_x_lv_ea ln_m_ea ln_p_lv_sa ln_p_ea_sa, lag(5)
predict coin_u, residual
quietly regress d.coin_u l.coin_u if date>473
estat ic
quietly regress d.coin_u l.coin_u dl.coin_u if date>473
estat ic
quietly regress d.coin_u l.coin_u dl.coin_u dl2.coin_u if date>473
estat ic
quietly regress d.coin_u l.coin_u dl.coin_u dl2.coin_u dl3.coin_u if date>473
estat ic
quietly regress d.coin_u l.coin_u dl.coin_u dl2.coin_u dl3.coin_u dl4.coin_u if date>473
estat ic
quietly regress d.coin_u l.coin_u dl.coin_u dl2.coin_u dl3.coin_u dl4.coin_u dl5.coin_u if date>473
estat ic
dfuller coin_u, lag(1) regress
* The values for dfuller test are different, I have looked them up in the statistical tables - the link is in the description of the tables. The results show that the cointegration with the best specification using IC does not exist, however, the it depends on the number of lags*
* There is another way to do it through the egranger module which gives the correct statistics for u*
ssc install egranger
egranger ln_x_lv_ea ln_m_ea ln_p_lv_sa ln_p_ea_sa, lag(1) regress
* Also, the one might consider exluding ln_p_lv_sa from this analysis because it does not give significant results in the regression.*
tsline ln_x_lv_ea ln_p_ea_sa ln_p_lv_sa ln_m_ea

*Further we can get rid of the insignificant factor (basically, create another model) and implement the error ECM analysis to run short-term model to find the iteration where the data is stationary*
newey ln_x_lv_ea ln_m_ea ln_p_ea_sa, lag(5)
predict coin2_u, resid
*Now the optimal DF model should be found
quietly regress d.coin2_u l.coin2_u if date>473
estat ic
quietly regress d.coin2_u l.coin2_u dl.coin2_u if date>473
estat ic
quietly regress d.coin2_u l.coin2_u dl.coin2_u dl2.coin2_u if date>473
estat ic
quietly regress d.coin2_u l.coin2_u dl.coin2_u dl2.coin2_u dl3.coin2_u if date>473
estat ic
quietly regress d.coin2_u l.coin2_u dl.coin2_u dl2.coin2_u dl3.coin2_u dl4.coin2_u if date>473
estat ic
quietly regress d.coin2_u l.coin2_u dl.coin2_u dl2.coin2_u dl3.coin2_u dl4.coin2_u dl5.coin2_u if date>473
estat ic
dfuller coin2_u, lag(1) regress
egranger ln_x_lv_ea ln_m_ea ln_p_ea_sa, lag(1) regress

**************************************************************

* Short-run equation (in first log-differences) should include lag of the residual from the long run equation
* However, we can have different versions of the short run equation:
* Use statistics on individual coefficients and/or IC to chose the preferred one
newey dln_x_lv_ea dln_m_ea dln_p_ea_sa l.coin2_u  if date>470, lag(5)
quietly estimates store eq1
quietly regress dln_x_lv_ea dln_m_ea dln_p_ea_sa l.coin2_u if date>470
estat ic

newey dln_x_lv_ea l.coin2_u l.dln_m_ea dln_p_ea_sa if date>470, lag(5)
quietly estimates store eq2
quietly regress dln_x_lv_ea l.coin2_u l.dln_m_ea dln_p_ea_sa if date>470
estat ic

newey dln_x_lv_ea dln_m_ea l.dln_p_ea_sa l.coin2_u if date>470, lag(5)
quietly estimates store eq3
quietly regress dln_x_lv_ea dln_m_ea l.dln_p_ea_sa l.coin2_u if date>470
estat ic

newey dln_x_lv_ea l.coin2_u l.dln_x_lv_ea dln_p_ea_sa dln_m_ea if date>470, lag(5)
quietly estimates store eq4
quietly regress dln_x_lv_ea l.coin2_u l.dln_x_lv_ea dln_p_ea_sa ln_m_ea if date>470
estat ic

newey dln_x_lv_ea l.coin2_u l.dln_x_lv_ea l.dln_p_ea_sa dln_m_ea if date>470, lag(5)
quietly estimates store eq5
quietly regress dln_x_lv_ea l.coin2_u l.dln_x_lv_ea l.dln_p_ea_sa dln_m_ea if date>470
estat ic

newey dln_x_lv_ea l.coin2_u l.dln_x_lv_ea dln_p_ea_sa l.dln_m_ea if date>470, lag(5)
quietly estimates store eq6
quietly regress dln_x_lv_ea l.coin2_u l.dln_x_lv_ea dln_p_ea_sa l.dln_m_ea if date>470
estat ic

newey dln_x_lv_ea l.coin2_u l.dln_x_lv_ea l.dln_p_ea_sa l.dln_m_ea if date>470, lag(5)
quietly estimates store eq7
quietly regress dln_x_lv_ea l.coin2_u l.dln_x_lv_ea l.dln_p_ea_sa l.dln_m_ea if date>470
estat ic

* Now we can show all 7 models, but we already know that the equation 5 looks more preferable to others using IC
esttab eq1 eq2 eq3 eq4 eq5 eq6 eq7

*Running the regression using the whole sample
newey dln_x_lv_ea l.coin2_u l.dln_x_lv_ea l.dln_p_ea_sa dln_m_ea, lag(5)
*The interpretation is in the results description file.

*Closing the log
log close
