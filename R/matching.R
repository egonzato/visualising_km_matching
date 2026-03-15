# load libraries 
pacman::p_load(MatchIt)
# seelct random participants
## filter
trial=df %>% filter(trt==1)
hc=df %>% filter(trt==0)
## create trial data
set.seed(11022004)
patients=trial[sample(1:dim(trial)[1],120),]
registry=hc[sample(1:dim(hc)[1],300),]
## put together
dftrial=rbind(patients,registry)
# run matching
set.seed(11022004)
matched=matchit(trt~age+sex,
                data=dftrial,
                replace=F,
                caliper=0.1,
                distance='glm',
                #m.order='random',
                method='nearest',
                ratio=2)
# summary procedure
summary(matched)
# extract matched
matcheddf=match.data(matched)
# see results from cox model
fit=matcheddf %>% 
  mutate(event=ifelse(timedays>1,0,event),
         time=ifelse(timedays>1,1,timedays),
         trt_label=ifelse(trt==1,'treated','untreated')) %>% 
  coxph(Surv(time,event)~trt_label,.)
# summary
summary(fit)
