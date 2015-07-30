### Multilevel model examining relationship between after school dosage
### participation and ISAT reading gains, SY 2014

### R version 3.1.2
### models built using lme4, version 1.1-7

### preprocessing ###

# grand mean centering of covariates

data2014isat$Centered.Grade.1 = scale(data2014isat$Grade.1, scale=FALSE)
data2014isat$Centered.Grade.2 = scale(data2014isat$Grade.2, scale=FALSE)
data2014isat$Centered.Grade.3 = scale(data2014isat$Grade.3, scale=FALSE)
data2014isat$Centered.Grade.4 = scale(data2014isat$Grade.4, scale=FALSE)
data2014isat$Centered.Grade.5 = scale(data2014isat$Grade.5, scale=FALSE)
data2014isat$Centered.Grade.6 = scale(data2014isat$Grade.6, scale=FALSE)
data2014isat$Centered.Grade.7 = scale(data2014isat$Grade.7, scale=FALSE)
data2014isat$Centered.Grade.8 = scale(data2014isat$Grade.8, scale=FALSE)
data2014isat$Centered.African.American = scale(data2014isat$African.American, scale=FALSE)
data2014isat$Centered.Asian = scale(data2014isat$Asian, scale=FALSE)
data2014isat$Centered.Asian.Pac.Islander = scale(data2014isat$Asian.Pac.Islander, scale=FALSE)
data2014isat$Centered.Hispanic = scale(data2014isat$Hispanic, scale=FALSE)
data2014isat$Centered.Native.American = scale(data2014isat$Native.American, scale=FALSE)
data2014isat$Centered.White = scale(data2014isat$White, scale=FALSE)
data2014isat$Centered.Multi.Other = scale(data2014isat$Multi.Other, scale=FALSE)
data2014isat$Centered.Gender = scale(data2014isat$Gender, scale=FALSE)
data2014isat$Centered.F.R.Lunch = scale(data2014isat$F.R.Lunch, scale=FALSE)
data2014isat$Centered.Disability = scale(data2014isat$Disability, scale=FALSE)
data2014isat$Centered.IEP = scale(data2014isat$IEP, scale=FALSE)
data2014isat$Centered.LEP = scale(data2014isat$LEP, scale=FALSE)
data2014isat$Centered.Student.Mobility = scale(data2014isat$Student.Mobility, scale=FALSE)
data2014isat$Centered.Susp.in.Sept = scale(data2014isat$Susp.in.Sept, scale=FALSE)
data2014isat$Centered.Present.Days.Prorated = scale(data2014isat$Present.Days.Prorated, scale=FALSE)
data2014isat$Centered.2013.Math = scale(data2014isat$Prior.Math.Scale.Score, scale=FALSE)
data2014isat$Centered.2013.Reading = scale(data2014isat$Prior.Reading.Scale.Score, scale=FALSE)
data2014isat$Centered.Present.Days.Prorated = scale(data.isat$Present.Days.Prorated)
data2014isat$Centered.Hrs.per.Week = scale(data$Hours.per.Week,scale=FALSE)

### Initial model ###

# multilevel model predicting ISAT reading scores from weeks and
# hours per week of after school participation, with random effect
# parameter for school attended

risat.lm = lmer(Reading.Gains ~ Mean.Weeks.Attended. + Centered.Hrs.per.Week +  
          Mean.Weeks.Attended.:Centered.LEP + Mean.Weeks.Attended.:Centered.F.R.Lunch +  
          Centered.Grade.5 * Centered.2013.Reading + Centered.Grade.6 *  
          Centered.2013.Reading + Centered.Grade.7 * Centered.2013.Reading +  
          Centered.Grade.8 * Centered.2013.Reading + Centered.Gender +  
          Centered.African.American + Centered.Asian + Centered.Asian.Pac.Islander +  
          Centered.Hispanic + Centered.Native.American + Centered.White +  
          Centered.F.R.Lunch + Centered.Disability + Centered.IEP +  
          Centered.LEP + Centered.Student.Mobility + Centered.Susp.in.Sept +  
          Centered.Present.Days.Prorated + Centered.2013.Math + Centered.2013.Reading +  
          (1 | School.ID),data2014isat)

# examining model parameters

summary(risat.lm)

### checking model assumptions ###
### diagnostic functions from package HLMdiag, version 0.2.1 ###

# normality of conditional residuals

qqnorm(HLMresid(risat.lm,1))

# homogeneity of variance (conditional residuals by predicted)

plot(HLMresid(risat.lm,1) ~ fitted(risat.lm),main="residual plot")

# normality of random effects

l2resid = as.numeric(HLMresid(risat.lm,"School.ID")[,1])
qqnorm(l2resid)

# presence of outlying schools

plot(l2resid)

# collinearity; checked using vif.mer, a function created by a
# psycholinguist to test for collinearity of predictors from
# lmer models

vif.mer(risat.lm)

### revised model due to non-normal conditional residuals ###

# transformation of ISAT reading gains
# If > 0: [Sqrt(Reading Gains+1)-1]/0.5
# If <= 0: -[Sqrt(1-Reading Gains)-1]/0.5

risat.lm2 = lmer(Reading.Gains.Transform ~ Mean.Weeks.Attended. + Centered.Hrs.per.Week +  
                  Mean.Weeks.Attended.:Centered.LEP + Mean.Weeks.Attended.:Centered.F.R.Lunch +  
                  Centered.Grade.5 * Centered.2013.Reading + Centered.Grade.6 *  
                  Centered.2013.Reading + Centered.Grade.7 * Centered.2013.Reading +  
                  Centered.Grade.8 * Centered.2013.Reading + Centered.Gender +  
                  Centered.African.American + Centered.Asian + Centered.Asian.Pac.Islander +  
                  Centered.Hispanic + Centered.Native.American + Centered.White +  
                  Centered.F.R.Lunch + Centered.Disability + Centered.IEP +  
                  Centered.LEP + Centered.Student.Mobility + Centered.Susp.in.Sept +  
                  Centered.Present.Days.Prorated + Centered.2013.Math + Centered.2013.Reading +  
                  (1 | School.ID),data2014isat)

# examining model parameters
summary(risat.lm2)

### re-checking assumptions for revised model ###

# normality of conditional residuals

qqnorm(HLMresid(risat.lm2,1))

# homogeneity of variance (conditional residuals by predicted)

plot(HLMresid(risat.lm2,1) ~ fitted(risat.lm2),main="residual plot")

# normality of random effects

l2resid2 = as.numeric(HLMresid(risat.lm2,"School.ID")[,1])
qqnorm(l2resid2)

# presence of outlying schools

plot(l2resid2)

### significance tests and effect sizes ###

# significance tests from model with transformed ISAT reading gains

t_vals = coef(summary(risat.lm2))[,"t value"]

# fixed effect parameters from untransformed model, for comprehensibility
# (per Raudenbush & Bryk, 2002, only hypothesis tests are biased in the 
# face of non-normal conditional residuals, not parameter estimates)

param = fixef(risat.lm)

# standardized effect size for weeks in After School All Stars

es = param["Mean.Weeks.Attended."] / sigma(risat.lm)

# intraclass correlation coefficient

var_comp = as.data.frame(VarCorr(risat.lm))
icc = var_comp$vcov[1] / (var_comp$vcov[1] + var_comp$vcov[2])
