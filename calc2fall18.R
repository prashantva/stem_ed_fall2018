# set up the working directory
#setwd("~/000_Fall_2018_AEM_STEM_Research/data_and_models") 
source("myRfunctions.R")
ccc()
source("myRfunctions.R") # Redo the source, since ccc wiped out everything
library("mice")
libraries() # get the necessary libraries

# Get and clean the Excel data unless you already have it in the workspace
#if (!exists("df", envir=.GlobalEnv)){
  df<-getNcleanXL("datafor_r.xlsx");
#}
df<-within(df, rm('GpaPr'));
# Regression Analysis on completed data

corrs <- cor(df)
corrplot(corrs, type="lower", method="color", cl.pos = "b", addCoef.col="black", tl.pos = "l", tl.col = "black")

# Linear modeling
fit<-lm(Calc2~Calc1+iphone, df); # The linear model
fit0<-lm(Calc2~ 1, df); # The linear model with only intercept
fit_backward <- stepAIC(fit, direction="backward", trace=0)
fit_forward <- stepAIC(fit0, direction="forward", scope=list(upper=fit,lower=fit0), trace=0);
if (fit_forward$call$formula==fit_backward$call$formula)
{
  fit_final<-fit_forward
  #cat("Both forward and backward models give the same model, namely:", sep="\n")
  #print(fit_final$call)
}else
{
  #cat("Forward and backward models give different models.", sep="\n")
  fit_final <- stepAIC(fit, direction="both")
  fit_final$anova
}
Calc2_fit<-fitted(fit_final) # predicted values

# Regression diagnostics 
ggplot(data=df)+
  my_theme+
  coord_fixed()+
  labs(x="Calculus I grade points", y="Calculus II grade points")+
  geom_point(mapping = 
               aes(x = Calc1, y = Calc2), size=5)+
  geom_line(aes(x=Calc1, y = Calc2_fit), color="black", size=1)
  #geom_segment(aes(x=Calc1, y = Calc2, xend = Calc1, yend = Calc2_fit), alpha = .5)

# Analysis of residuals
# Prepare the dataset madeup of residuals only
x_pts<-1:nrow(df);
res<-residuals(fit_final) # residuals
standard_res <- stdres(fit_final) # standardized residuals
studentized_res <- studres(fit_final) ; # studentized residuals
df_res<-data.frame(x_pts, res, standard_res, studentized_res); # dataset of residuals

# Plotting the reisuals 
ggplot(data=df_res)+
  my_theme+
  labs(x="", y="Residuals")+
  ylim(-4.5, 4.5)+
  geom_point(mapping = aes(x=x_pts, y =res), shape=21, size=5, fill="lightblue", stroke=2)+
  geom_segment(aes(x=x_pts, y = 0, xend = x_pts, yend=res ), alpha=0.5)+
  geom_segment(aes(x=0, y = 0, xend = 72, yend = 0))

# *** Plotting of Residuals with normal curve
lim<-1.2*ceil(max(abs(min(df_res$res)), abs(max(df_res$res))));
ggplot(df_res, aes(x=res)) + 
  my_theme+
  labs(x="Residuals", y="Density")+
  geom_histogram(aes(y =..density..), 
                 #breaks = seq(-3, 3, by = 0.5),
                 color="black", 
                 fill="lightblue", 
                 binwidth=1, 
                 size=1)+
  stat_function(fun = dnorm, args = list(mean = mean(df_res$res), sd = sd(df_res$res)), size=1)+
  xlim(-lim,lim);

# QQ plots 
ggplot(data = df_res, mapping = aes(sample = res))+
  stat_qq_band(conf = 0.95, size=0.5, fill="lightblue")  +
  stat_qq_line() +
  stat_qq_point(shape=21, size=2, stroke=1, fill="blue") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  my_theme

# *** Normality of the Residuals
shapiro_res<-shapiro.test(res);               shapiro_res$p.value
anderson_res<-ad.test(res);                   anderson_res$p.value
lillie_res<-lillie.test(res);                 lillie_res$p.value

# Power analysis:
# Power of the test (f2=0.02, 0.15, and 0.35 for small medium and large)
# Number of subjects required for 80% power using 1 variable
f2large<-0.35; 
f2medium<-0.15; 
f2small<-0.02; 

num_DF<-as.numeric((summary(fit_final))$fstatistic[2])
den_DF<-as.numeric((summary(fit_final))$fstatistic[3])
p<-pwr.f2.test(u = num_DF, v = den_DF, f2large, sig.level = 0.05, power = NULL);
cat(sep="\n")
cat("Power of the test with large effect size is ",p$power)
cat(sep="\n")
p<-pwr.f2.test(u=num_DF, v=NULL, f2large, power = 0.8);
N80<-ceil(p$u+p$v+1)
cat("Samples needed for 80% power with large effect size = ", N80,".") 
cat(sep="\n")
if (N80<72)
{
  cat("We have enough sample size for 80% power with large effect size.", sep="\n")
  cat(sep="\n")
  
}else{
  cat("We do not have enough sample size for 80% power with large effect size.", sep="\n")
  cat(sep="\n")
}
p<-pwr.f2.test(u = num_DF, v=den_DF, f2=0.15, sig.level = 0.05, power = NULL)
cat(sep="\n")
cat("Power of the test with medium effect size:", p$power,".")
cat(sep="\n")

# Number of subjects required for 80% power using 1 variable
p<-pwr.f2.test(u = num_DF, v=NULL, f2=0.15, sig.level = 0.05, power = 0.8)
N80<-ceil(p$u+p$v+1) # 
cat("Samples needed for 80% power with medium effect size = ", N80,".")
cat(sep="\n")
if (N80<72)
{
  cat("We have enough sample size for 80% power with medium effect size.", sep="\n")
  cat(sep="\n")}else{
  cat("We do not have enough sample size for 80% power with medium effect size.", sep="\n")
  cat(sep="\n")
}
p<-pwr.f2.test(u = num_DF, v = den_DF, f2=0.02, sig.level = 0.05, power = NULL); p$power
cat("Power of the test with small effect size is ", p$power)
cat(sep="\n")
# Number of subjects required for 80% power using 1 variable
p<-pwr.f2.test(u = num_DF, v=NULL, f2=0.02, sig.level = 0.05, power = 0.8)
N80<-ceil(p$u+p$v+1) # 
cat("Samples needed for 80% power with small effect size = ", N80,".")
cat(sep="\n")
if (N80<72)
{
  cat("We have enough sample size for 80% power with small effect size.", sep="\n")
  cat(sep="\n")}else{
  cat("We do not have enough sample size for 80% power with small effect size.", sep="\n")
  cat(sep="\n")
}
print(summary(fit))
print(confint(fit))
print(summary(fit_final))
print(confint(fit_final))

