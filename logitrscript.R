setwd("~/Documents/GRAD SCHOOL/Spring 2017/Executives, Legislatures, & Public Poliy/Final Paper")
lawchange <- read.csv("lawchange2.csv")
attach(lawchange)
logit <- glm(law_restrict ~ repub_both + form_preclear 
             + white_min50 + crime_rate + vep_highest100 + 
               median_age, 
             data = lawchange, family ="binomial")
summary(logit)
library(stargazer)
stargazer(logit)

lowrylogit <- glm(law_restrict ~ repub_both + form_preclear + percent_hisp100 + percent_black100 + 
                    vep_highest100 + median_age+ repub_both*percent_hisp100*percent_black100,
                  data = lawchange, family ="binomial")
summary(lowrylogit)
stargazer(lowrylogit)

paperlogit <- glm(law_restrict ~ repub_both + form_preclear + percent_hisp100 + percent_white100 + 
                    vep_highest100 + median_age, data= lawchange, family = "binomial")
summary(paperlogit)
stargazer(paperlogit)

library(rms)
logitb <- lrm(law_restrict ~ repub_both + form_preclear 
             + white_min50 + crime_rate + vep_highest100 + 
               median_age,
              data= lawchange)
print(logitb)

lowrylogitb <- lrm(law_restrict ~ repub_both + form_preclear + percent_hisp100 + percent_black100 + 
                    vep_highest100 + median_age+ repub_both*percent_hisp100*percent_black100,
                  data = lawchange)
print(logitb)
paperlogitb <- glm(law_restrict ~ repub_both + form_preclear + percent_hisp100 + percent_white100 + 
                    vep_highest100 + median_age, data= lawchange, family = "binomial")
print(paperlogitb)



exp(cbind("Odds Ratio" = coef(logit), confint(logit)))
with(logit, null.deviance - deviance)
with(logit, df.null - df.residual)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, 
                     lower.tail = FALSE))








#SIMPLE LOGIT: ONLY REPUB_BOTH, FORMER PRECLEARANCE STATE & WHITE MIN
simplelogit <- glm(law_restrict ~ repub_both + form_preclear 
             + white_min50, 
             data = lawchange, family ="binomial")
summary(simplelogit)

logit_vep <- glm(law_restrict ~ repub_both + form_preclear 
            , 
             data = lawchange, family ="binomial")
summary(logit_vep)


exp(cbind(OR = coef(simplelogit), confint(simplelogit)))
with(simplelogit, null.deviance - deviance)
with(simplelogit, df.null - df.residual)
with(simplelogit, pchisq(null.deviance - deviance, df.null - df.residual, 
                     lower.tail = FALSE))




#probit
probit <- glm(law_restrict ~ repub_leg + repub_gov + repub_both + form_preclear 
             + white_min50 + log_violent_crrate + log_prop_crrate + log_gdp
             + unemploy_rate + vep_highest100 + median_age, 
          family = binomial(link= "probit"), data = lawchange)
summary(probit)
