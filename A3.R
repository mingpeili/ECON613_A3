
#Mingpei Li
#ECON613 
#Assignement 3


#i refered to professors' code when complete this assignement.

library(moments)
library(tidyverse)
library(dplyr)
library(ggplot2)


set.seed(1234)


########## Exercise 1 #############
population = read.csv(file = 'population.csv')
crime_long = read.csv(file = 'crime_long.csv')
officers = read.csv(file = 'officers.csv')



########## Exercise 2 ##############

#first, add up crimes in crimes.csv with all other variables being the same
crimel = crime_long %>% rename(month = crime_month)
crimel=crimel%>%group_by(month, district, crime_type)%>%summarise(crimes= sum(crimes))%>%ungroup()





#total crime per month and plot the time series of crime.
tot_crimel = crimel %>% group_by(month) %>% summarise(tot_crimes = sum(crimes))
head(tot_crimel, n=20)
tot_crimel=ungroup(tot_crimel)
crimel_plot = tot_crimel%>%subset(select=c('month','tot_crimes'))
crimel_plot$month = as.Date(crimel_plot$month,"%Y-%m-%d")
plot(crimel_plot, aes(month,tot_crimes))


#merge the two datasets by districts-units and period.
pop_crimel = left_join(population, crimel, by = c("month", "district"))
head(pop_crimel, n=20)

#construct a panel data of unit over time with the following variable

#create wide from of crime data
crimew = pivot_wider(crimel, id_cols = c('month','district'),
                     names_from = crime_type, 
                     values_from = crimes,
                     names_prefix = "crimes_")
crimew = crimew%>%mutate(crimes=crimes_drug+crimes_other+crimes_property+crimes_violent)

#merege with popualtion
pop_crimew = left_join(population, crimew, by = c("month", "district"))


time_pop_crimew = pop_crimew%>%group_by(month,district)%>%summarise(total_crimes_per_resident = sum(crimes)/sum(tot_pop),
                                                  violent_crimes_per_resident = sum(crimes_violent)/sum(tot_pop),
                                                  property_crimes_per_resident = sum(crimes_property)/sum(tot_pop),
                                                  median_income = mean(p50_inc),
                                                  black = sum(tot_black)/sum(tot_pop),
                                                  hispanic = sum(tot_hisp)/sum(tot_pop),
                                                  white = sum(tot_white)/sum(tot_pop)
                                                  ) %>%ungroup()
head(time_pop_crimew,n=20)







###########Exercise 3################
officers1 = rename(officers, district = unit)
 
#merge
alldata = left_join( officers1,pop_crimew, by = c("month", "district"))
#drop missing values
alldata = alldata%>%filter(is.na(alldata)==0)
alldata = alldata%>%mutate(black_share = tot_black/tot_pop,
                     hispanic_share = tot_hisp/tot_pop,
                     white_share = tot_white/tot_pop,
                     )
reg1 = lm(arrest ~ tenure + crimes + p50_inc + black_share + hispanic_share + white_share, data=alldata)
summary(reg1)






#########Exercise 4#################
#creat a new varibale which is a string version of district
#use string variables directly in the lm function can set it as fixed effects
alldata$dist_str = paste("district", alldata$district, sep ="" )

#regression
reg2 = lm(arrest ~ tenure + crimes + p50_inc + black_share + hispanic_share + white_share +dist_str +month, data=alldata)
summary(reg2)







#########Exercise 5################
#creat a new varibale which is a string version of NUID
alldata$NUID_str = paste("NUID", alldata$NUID, sep ="" )

data_ex5 = alldata %>% subset(select = c('NUID','NUID_str','arrest','tenure','crimes','p50_inc','black_share', 'hispanic_share', 'white_share','district','dist_str','month'))


#between estimator
data_between = data_ex5%>%group_by(NUID)%>%summarise(marrest=mean(arrest),
                                                    mtenure=mean(tenure),
                                                    mcrimes=mean(crimes),
                                                    mp50_inc=mean(p50_inc),
                                                    mblack_share=mean(black_share),
                                                    mhispanic_share=mean(hispanic_share),
                                                    mwhite_share = mean(white_share)
                                                    )%>%ungroup()

reg_between = lm(marrest ~  mtenure + mcrimes + mp50_inc + mblack_share + mhispanic_share + mwhite_share , data=data_between)
summary(reg_between)




#within estimator
data_within = left_join(data_ex5,data_between)
data_within = data_within%>%mutate(darrest=arrest-marrest,
                                   dtenure=tenure-mtenure,
                                   dcrimes=crimes-mcrimes,
                                   dp50_inc=p50_inc-mp50_inc,
                                   dblack_share=black_share-mblack_share,
                                   dhispanic_share=hispanic_share-mhispanic_share,
                                   dwhite_share = white_share-mwhite_share,
                                   )


reg_within = lm(darrest ~  dtenure + dcrimes + dp50_inc + dblack_share + dhispanic_share + dwhite_share , data=data_within)
summary(reg_within)





#first difference estimator
data_fd=data_ex5
data_fd$month = as.Date(data_fd$month,"%Y-%m-%d")
#order according to NUID and month
data_fd = data_fd[order(data_fd$NUID, data_fd$month),]
#market observations which will be overlapping after we generate previous values 
data_fd = data_fd%>%group_by(NUID)%>%mutate(min_month=min(month))%>%ungroup()
data_fd = data_fd%>%mutate(drop = if_else(month==min_month,1,0))
#generate previous values
data_fd = data_fd%>%mutate(pre_arrest=dplyr::lag(arrest),
                           pre_tenure=dplyr::lag(tenure),
                           pre_crimes=dplyr::lag(crimes),
                           pre_p50_inc=dplyr::lag(p50_inc),
                           pre_black_share=dplyr::lag(black_share),
                           pre_hispanic_share=dplyr::lag(hispanic_share),
                           pre_white_share = dplyr::lag(white_share))
#drop overlapping and missing observations
data_fd = data_fd %>% filter(drop==0)
data_fd = data_fd %>% filter(is.na(data_fd)==0)
#subtraction 
data_fd = data_fd%>%mutate(fd_arrest= arrest-pre_arrest,
                           fd_tenure=tenure-pre_tenure,
                           fd_crimes=crimes-pre_crimes,
                           fd_p50_inc=p50_inc-pre_p50_inc,
                           fd_black_share=black_share-pre_black_share,
                           fd_hispanic_share=hispanic_share-pre_hispanic_share,
                           fd_white_share = white_share-pre_white_share )
#regression
reg_fd = lm(fd_arrest ~  fd_tenure + fd_crimes + fd_p50_inc + fd_black_share + fd_hispanic_share + fd_white_share , data=data_fd)
summary(reg_fd)


#the estimated values are not the same.


#GMM

GMM = function(param, data_GMM){
  
  NUID = data_GMM$NUID
  month = data_GMM$month
  district = data_GMM$district
  arrest = data_GMM$arrest
  tenure = data_GMM$tenure
  crimes = data_GMM$crimes
  p50_inc = data_GMM$p50_inc
  black_share = data_GMM$black_share
  hispanic_share = data_GMM$hispanic_share
  white_share = data_GMM$white_share
  
  NUID_rank = factor(NUID)
  month_rank = factor(month)
  
  ni=length(unique(alldata$NUID))
  nj=length(unique(alldata$district))
  nt=length(unique(alldata$month))
  
  nrow=nrow(data_GMM)
  
  
  #assign parameters to coefficients
  alpha = param[1:ni]
  beta = param[ni+1]
  gamma = param[(ni+2):(ni+6)]
  psi = param[(ni+7):(nj+6+nj)]
  kappa = param[(ni+7+nj):(nj+6+nj+nt)]
  
  
  #get the vector of predicted arrest
  pred= mat.or.vec(nrow,1)
  for (row in 1:nrow){
    pred[row] = alpha[NUID_rank[row]] + beta*tenure[row] + 
      gamma[1]*crimes[row] +gamma[2]*p50_inc[row] + gamma[3]*black_share[row] + gamma[4]*hispanic_share[row] + 
      gamma[5]*white_share[row]+psi[district[row]]+kappa[month_rank[row]]
      
  }
  
  #calculate the moments and the differences in moments
  mom_pred = all.moments(pred,order.max=5)[-1]
  mom_arrest = all.moments(arrest,order.max=5)[-1]
  like = sum(( mom_pred - mom_arrest)^2)
  
  return(like)
  
}


ni=length(unique(alldata$NUID))
nj=length(unique(alldata$district))
nt=length(unique(alldata$month))
param = runif(ni+6+nj+nt, -10, 10)

res = optim(param,fn=GMM,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),data_GM=alldata)
summary(res)




