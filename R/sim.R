# libraries
pacman::p_load(dplyr,ggplot2,survival,survminer,ggsurvfit)
# set number of observations
n=1e4
# set seed for reproducibility
set.seed(03111963)
# simulate observations with id, date of onset of the diseases, event and error
df=data.frame(id=1:n,
              sex=rbinom(n,1,0.5),
              age=rnorm(n,45,12),
              event=1,
              e=rnorm(n,0.9,1.2)) 
# define probability of treatment based on age and sex
df=df %>% 
  mutate(ptrt=1/(1+exp(-(-20 + 0.35*age + 2.5*sex))),
         trt=rbinom(n(),1,ptrt))
# see probability distirbution
hist(df$ptrt); table(df$trt)
# distribution of covariates between treatment groups
## age
df %>% 
  mutate(trt_label=ifelse(trt==1,'treated','untreated')) %>% 
  ggplot(.,aes(x=age,group=trt_label,fill=trt_label,alpha=.7))+
  scale_fill_manual(values=c('#00CCFF','#FF3300'))+
  geom_density()+
  labs(fill='treatment group')+
  guides(alpha='none')+
  theme_bw()+
  theme(legend.position='bottom')
## sex
df %>% 
  mutate(trt_label=ifelse(trt==1,'treated','untreated'),
         sex_label=ifelse(sex==1,'female','male')) %>% 
  ggplot(.,aes(x=trt_label,group=sex_label,fill=sex_label,alpha=.7))+
  scale_fill_manual(values=c('blue','pink'))+
  geom_bar(position='dodge')+
  labs(fill='treatment group')+
  guides(alpha='none')+
  theme_bw()+
  theme(legend.position='bottom')
# treatment effect
df=df%>% 
  mutate(linearpred=0.001*age-0.5*sex-1.6*trt+e,
         hazard=exp(linearpred),
         event=1,
         timedays=rexp(n,hazard))
# surv object
km=df %>% 
  mutate(trt_label=ifelse(trt==1,'treated','untreated'),
         event=ifelse(timedays>1,0,event),
         time=ifelse(timedays>1,1,timedays)) %>% 
  survfit(Surv(time,event)~trt_label,.)
# plot km
ggsurvfit(km)+
  scale_color_manual(values=c('#00CCFF','#FF3300'))+
  ylim(0,1)
