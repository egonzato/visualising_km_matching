pacman::p_load(readxl,dplyr,rlang,ggplot2,tidyr,gifski,MatchIt,
               survival,survminer,ggsurvfit,smd,gtsummary)
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
# initialize quantities
betas=c();ses=c(); models=list(); datasets=list()
# run loop
for(i in 1:1000){
  # set seed for each run
  dftrial=dftrial[sample(1:dim(dftrial)[1],dim(dftrial)[1]),]
  # perform matching
  matched=matchit(trt~age+sex,
                  data=dftrial,
                  replace=F,
                  caliper=0.05,
                  distance='glm',
                  m.order='data',
                  method='nearest',
                  ratio=1)
  # extract matched
  matcheddf=match.data(matched); 
  # see results from cox model
  matcheddf=matcheddf %>% 
    mutate(event=ifelse(timedays>1,0,event),
           time=ifelse(timedays>1,1,timedays),
           trt_label=factor(ifelse(trt == 1, 'treated', 'untreated'), 
                            levels = c('untreated', 'treated')))
  # save dataset already truncated
  datasets[[i]]=matcheddf
  # model
  model=coxph(Surv(time,event)~trt_label,matcheddf)
  # store model in list
  models[[i]]=model
  # append estimates for beta and ses
  betas[i]=coefficients(model)[1]; ses[i]=sqrt(diag(vcov(model)))[1]
  # at what point are we?
  if (i %in% seq(0,1000,100)){print(paste('iteration n.: ',i))}
}
# bind estimates
estimates=as.data.frame(cbind(betas,ses))
# save
# save models
saveRDS(models,"path_models")
# save dataset
saveRDS(datasets,"path_dataset")
# save estimates
writexl::write_xlsx(estimates,"path_estimates")
# read dataset and models
datasets=readRDS("path_dataset")
models=readRDS("path_models")
estimates=readxl::read_xlsx("path_estimates")
# video
## KM
plot_files=c()
img_dir="path_img_km"
for(i in 1:100) {
  df_post=datasets[[i]]
  fit=survfit(Surv(time, event) ~ trt_label, data = df_post)
  km_plot=ggsurvfit(fit,linewidth=0.8)+
    add_confidence_interval()+
    scale_color_manual(values=c('#FF3300','#00CCFF'))+
    scale_fill_manual(values=c('#FF3300','#00CCFF'),labels=c('Untreated','Treated'))+
    #add_risktable()+
    #scale_x_continuous(breaks=seq(0,5,1))+
    labs(x='Time (years)',y='Survival probability',
         title=paste("Permutation n.:",i),
         fill='Treatment group')+
    guides(col='none')+
    ylim(0,1)
  filename=file.path(img_dir, sprintf("kmplot_%04d.png", i))
  ggsave(filename, plot = km_plot, width = 5, height = 4, dpi = 100)
  plot_files=c(plot_files, filename)
  if (i %in% seq(0,100,10)){print(paste('iteration n.: ',i))}
}
# video km
gifski(png_files=plot_files, gif_file = "path_gif_km",delay=0.1)
# loop histogram
img_dir="path_img_histogram"
hr_vector=exp(estimates$betas)
plot_files=c()
for(i in 1:1000){
  df_hr=data.frame(HR = hr_vector[1:i])
  p=ggplot(df_hr)+
    geom_histogram(aes(x=HR),fill='white',alpha=0.4,col='black')+
    geom_vline(xintercept = exp(median(estimates$betas))+0.001,lty=2)+
    labs(x='Hazard Ratios',y='Count')+
    theme_bw()
  
  filename=file.path(img_dir, sprintf("hist_%04d.png", i))
  ggsave(filename, plot = p, width = 5, height = 4, dpi = 100)
  plot_files=c(plot_files, filename)
  if (i %in% seq(0,1000,100)){print(paste('iteration n.: ',i))}
}
# Create MP4 video
gifski(png_files=plot_files, gif_file= "path_gif_histogram",delay=0.01)
