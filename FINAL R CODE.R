setwd("~/Documents/GRAD SCHOOL/Spring 2017/EPPS 7316/FINAL PAPER")
voterdata <- read.csv("voterturnout4_add_var.csv")
attach(voterdata)
voterdata$midterm[voterdata$year==2014] <- 1
voterdata$midterm[voterdata$year==2010] <- 1
voterdata$midterm[voterdata$year==2006] <- 1
voterdata$midterm[voterdata$year==2002] <- 1
voterdata$midterm[voterdata$year==1998] <- 1
voterdata$midterm[voterdata$year==1994] <- 1
voterdata$midterm[voterdata$year==1990] <- 1
voterdata$midterm[voterdata$year==1986] <- 1
voterdata$midterm[voterdata$year==1982] <- 1
voterdata$midterm[is.na(voterdata$midterm)] <- 0

voterdata$law_change[voterdata$year==2014 & voterdata$state=="Alabama"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="Alabama"] <- 1
voterdata$law_change[voterdata$year==2014 & voterdata$state=="Arizona"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="Arizona"] <- 1
voterdata$law_change[voterdata$year==2014 & voterdata$state=="Mississippi"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="Mississippi"] <- 1
voterdata$law_change[voterdata$year==2014 & voterdata$state=="South Carolina"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="South Carolina"] <- 1
voterdata$law_change[voterdata$year==2014 & voterdata$state=="Texas"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="Texas"] <- 1
voterdata$law_change[voterdata$year==2014 & voterdata$state=="Virginia"] <- 1
voterdata$law_change[voterdata$year==2016 & voterdata$state=="Virginia"] <- 1
voterdata$law_change[is.na(voterdata$law_change)] <- 0

preclear_states.and.counties<- voterdata[(voterdata$state %in% c("Alabama", "Alaska", "Arizona", 
            "Georgia", "Louisiana", "Mississippi", 
            "South Carolina", "Texas", "Virginia", "North Carolina", "California",
            "Florida", "New York", "South Dakota")), ]

preclear_states<- voterdata[(voterdata$state %in% c("Alabama", "Alaska", "Arizona", 
            "Georgia", "Louisiana", "Mississippi", 
            "South Carolina", "Texas", "Virginia")), ]



library(plm)
pdata_states <- plm.data(preclear_states, indexes = c("state", "year"))
pdata_states.and.counties <- plm.data(preclear_states.and.counties, 
                                      indexes = c("state", "year"))



#pooledOLS STATES VEP
pooled.ols_vep_states <- plm(vep_highest ~ law_change + midterm
      + race_percent_minority100 + median_age + 
      gender_percent_females100, data = pdata_states, model= "pooling")


#pooledOLS STATES & COUNTIES VEP
pooled.ols_vep_states.and.counties <- plm(vep_highest ~ law_change + midterm
        + race_percent_minority100 + median_age + 
        gender_percent_females100, 
        data = pdata_states.and.counties, model= "pooling")

#fixed effects STATES vep_highest
fixed.effects_vep_states <- plm(vep_highest ~ law_change + midterm
    + race_percent_minority100 + median_age + 
      gender_percent_females100, data = pdata_states, model= "within")


#fixed effects STATES & COUNTIES vep_highest
fixed.effects_vep_states.and.counties <- plm(vep_highest ~ law_change + midterm
          + race_percent_minority100 + median_age + 
          gender_percent_females100, 
      data = pdata_states.and.counties, model= "within")


#random.effects with vep_highest STATES
random.effects_vep_states <- plm(vep_highest ~ law_change + midterm
      + race_percent_minority100 + median_age + 
      gender_percent_females100, data = pdata_states, model= "random")

#random.effects with vep_highest STATES AND COUNTIES
random.effects_vep_states.and.counties <- plm(vep_highest ~ law_change 
    + midterm + race_percent_minority100 + median_age + 
      gender_percent_females100, 
      data = pdata_states.and.counties, model= "random")


#LM test for random effects vs. OLS
plmtest(pooled.ols_vep_states)
lmtest <- plmtest(pooled.ols_vep_states)
library(pander)
pander(lmtest)

#F test for fixed effects vs. OLS
pFtest(fixed.effects_vep_states, pooled.ols_vep_states)
ftest<- pFtest(fixed.effects_vep_states, pooled.ols_vep_states)
pander(ftest)

pFtest(fixed.effects_vep_states.and.counties, pooled.ols_vep_states.and.counties)
pFtest(fixed.effects_vap_states, pooled.ols_vap_states)
pFtest(fixed.effects_vap_states.and.counties, pooled.ols_vap_states.and.counties)


#hausmantest #if below 0.05 then use fixed effects 
phtest(fixed.effects_vep_states, random.effects_vep_states) 
hausman <- phtest(fixed.effects_vep_states, random.effects_vep_states) 
pander(hausman)

phtest(fixed.effects_vep_states.and.counties, random.effects_vep_states.and.counties)
phtest(fixed.effects_vap_states, random.effects_vap_states)
phtest(fixed.effects_vap_states.and.counties, random.effects_vap_states.and.counties)


#RESULTS VEP STATES
summary(fixed.effects_vep_states)
fixef(fixed.effects_vep_states)

residuals = residuals(fixed.effects_vep_states)
fix = fixef(fixed.effects_vep_states)
cor_ui_xb = cbind(residuals, fix)
presidualsfe <- cor(cor_ui_xb) #shows correlation coefficient between errors(residuals) and fixed effects
stargazer(presidualsfe)

#READ VEP STATES RESULTS INTO NICE CHART
library(stargazer)
stargazer(random.effects_vep_states)


#HOW TO FUCKING PLOT
p <- ggplot(data = preclear_states, 
            aes(x = year, y = vep_highest, group = state))

#plotting points
p + geom_point()

#lineplot
p+ geom_line()

#condition based on law_change dummy
p + geom_line() + facet_grid(.~law_change)
p + geom_line() + facet_grid(.~midterm)

p + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),
    geom = "point", fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ law_change)
p + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),
    geom = "point", fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ midterm)

#car pacakge
library(car)
scatterplot(vep_highest~ year|state, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=preclear_states)

