library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)

COVID19 = read.table("COVID19.txt", header = T, sep = "\t")
COVID19[652,c(5:7)] = 0

# Sex
# Male = 1
# Female = 2
# AgeGroup
# 0-17 years = 1
# 18-29 years = 2
# 30-49 years = 3
# 50-64 years = 4
# 65-74 years = 5
# 75-84 years = 6
# 85 years and over = 7



COVID19$Sex = ifelse(COVID19$Sex == "Male", 1, 2)
COVID19$AgeGroup = as.factor(COVID19$AgeGroup)
COVID19$AgeGroup = factor(COVID19$AgeGroup,levels(COVID19$AgeGroup),1:7)
COVID19$AgeGroup = as.integer(COVID19$AgeGroup)
COVID19 = COVID19[-which(COVID19$AgeGroup == 1),]
#COVID19 = COVID19[-which(COVID19$AgeGroup == 2),]
#COVID19 = COVID19[-which(COVID19$AgeGroup == 3),]
COVID19$AgeGroup = factor(COVID19$AgeGroup,2:7,1:6)
COVID19$AgeGroup = as.integer(COVID19$AgeGroup)
Y = COVID19$InfluenzaProp
factorA = COVID19$AgeGroup
factorB = COVID19$Sex
a = length(unique(factorA))
b = length(unique(factorB))
Yijbar = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijbar[i,j] = mean(Y[factorA==i&factorB==j])
  }
}
u = mean(Y)
Yidotbar = apply(Yijbar,MARGIN = 1,mean)
Ydotjbar  = apply(Yijbar,MARGIN = 2,mean)
alpha = Yidotbar-u
beta = Ydotjbar-u
gamma = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    gamma[i,j] = Yijbar[i,j]-Yidotbar[i]-Ydotjbar[j]+u
  }
}

Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}

e = Y-Yhat
plot(Yhat, e, pch = 19, xlab = "Fitted Values", ylab = "errors", main = "Residual Plot")

residual_plot <- ggplot()+
  geom_point(data = COVID19,aes(x = Yhat, y = e ,shape = AgeGroup ,color = Sex ))+
  xlab("Fitted Values")+ylab("Residuals")

residual_plot <- residual_plot + map2(fitted _values,residuals,~geom_segment(
                                                           aes(x = .x %>% unique,y %>% min,xend = .x %>% unique,
                                                               yend = .y %>% max),alpha = 0.25))


residual_plot

qqnorm(e)
qqline(e)

residuals %<>% unlist

residuals_standard <- residuals -(residuals %>% mean) 
residuals_standard <- residuals_standard/residuals %>% sd 


qq_plot <- ggplot(mapping = aes(sample = residuals_standard)) + geom_qq()+geom_qq_line(col = 2)+labs(title = "Normal Q-Q Plot")+xlab("Theoretical Quantiles")+ylab("Sample Quantiles")+theme_bw()+theme(panel.grid = element_blank())

qq_plot

n = 52
SSA = n*b*sum(alpha^2)
SSB = n*a*sum(beta^2)
SSAB = n*sum(gamma^2)
Yhat = rep(0,length(Y))
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yhat[factorA==i&factorB==j] = Yijbar[i,j]
  }
}
e = Y-Yhat
SSE = sum((Y-Yhat)^2)
SSTotal = sum((Y-u)^2)
AnovaTable = matrix(0,nrow = 5,ncol = 3)
AnovaTable[1,1] = SSA
AnovaTable[1,2] = a-1
AnovaTable[1,3] = SSA/(a-1)
AnovaTable[2,1] = SSB
AnovaTable[2,2] = b-1
AnovaTable[2,3] = SSB/(b-1)
AnovaTable[3,1] = SSAB
AnovaTable[3,2] = (a-1)*(b-1)
AnovaTable[3,3] = SSAB/((a-1)*(b-1))
AnovaTable[4,1] = SSE
AnovaTable[4,2] = a*b*(n-1)
AnovaTable[4,3] = SSE/(a*b*(n-1))
AnovaTable[5,1] = SSTotal
AnovaTable[5,2] = n*a*b-1
AnovaTable[5,3] = '-'
AnovaTable = as.data.frame(AnovaTable)
rownames(AnovaTable) = c('Factor A','Factor B','AB Interaction','Error','Total')
colnames(AnovaTable) = c('SS','df','MS')
AnovaTable$SS %<>% as.numeric
AnovaTable$MS %<>% as.numeric
AnovaTable$df %<>% as.numeric
library(pander)
pander(pandoc.table(AnovaTable))

Yijmedian = matrix(0,nrow = a,ncol = b)
for(i in 1:a)
{
  for(j in 1:b)
  {
    Yijmedian[i,j] = median(Y[factorA==i&factorB==j])
  }
}

Yijmedian


# median = c(rep(Yijmedian[1,1],52), rep(Yijmedian[1,2],52), rep(Yijmedian[2,1],52), rep(Yijmedian[2,2],52), rep(Yijmedian[3,1],52), rep(Yijmedian[3,2],52), rep(Yijmedian[4,1],52), rep(Yijmedian[4,2],52), rep(Yijmedian[5,1],52), rep(Yijmedian[5,2],52), rep(Yijmedian[6,1],52), rep(Yijmedian[6,2],52), rep(Yijmedian[7,1],52), rep(Yijmedian[7,2],52))
# 0-17 years = 1
# 18-29 years = 2
# 30-49 years = 3
# 50-64 years = 4
# 65-74 years = 5
# 75-84 years = 6
# 85 years and over = 7

COVID19 %<>% group_by(AgeGroup,Sex)

interaction_plot <- ggplot(data =COVID19,aes(AgeGroup,COVIDProp,color = Sex,group = Sex)) +
  stat_summary(fun = mean,geom = "point")+
  stat_summary(fun = mean,geom = "line")+
  ylab("Mean Proportion of Deaths")+
  xlab("Factor A: Age Group" )+
  labs(color = "Factor B: Sex")+
  scale_x_discrete(labels = c("50-64 years","65-74 years","75-84 years","85 years and over"))+
  scale_color_manual(labels = c("Male","Female"),values = c("#7AD7F0","#FF69B4"))

interaction_plot

F_Test_Interaction <- function(anova_table,alpha){
  test_stat <- anova_table["AB Interaction","MS"]/anova_table["Error","MS"]
  critical_value <- qf(1-alpha,anova_table["AB Interaction","df"],anova_table["Error","df"])
  p_value <- pf(test_stat,anova_table["AB Interaction","df"],anova_table["Error","df"],lower.tail = FALSE)
  F_test_df <- data.frame("F_Statistic" = test_stat,"F_Critical_Value" = critical_value,"P_Value" = p_value)
  
}

F_Test_FactorA_Effect <- function(anova_table,alpha){
  test_stat <- anova_table["Factor A","MS"]/anova_table["Error","MS"]
  critical_value <- qf(1-alpha,anova_table["Factor A","df"],anova_table["Error","df"])
  p_value <- pf(test_stat,anova_table["Factor A","df"],anova_table["Error","df"],lower.tail = FALSE)
  F_test_df <- data.frame("F_Statistic" = test_stat,"F_Critical_Value" = critical_value,"P_Value" = p_value)
  
}

F_Test_FactorB_Effect <- function(anova_table,alpha){
  test_stat <- anova_table["Factor B","MS"]/anova_table["Error","MS"]
  critical_value <- qf(1-alpha,anova_table["Factor B","df"],anova_table["Error","df"])
  p_value <- pf(test_stat,anova_table["Factor B","df"],anova_table["Error","df"],lower.tail = FALSE)
  F_test_df <- data.frame("F_Statistic" = test09429e-51_stat,"F_Critical_Value" = critical_value,"P_Value" = p_value)
}

F_test_interact <- F_Test_Interaction(AnovaTable,0.05)
F_test_FactorA <- F_Test_FactorA_Effect(AnovaTable,0.05)
F_test_FactorB <- F_Test_FactorB_Effect(AnovaTable,0.05)

D11_12 = Yijbar[1,1] - Yijbar[1,2] 
D21_22 = Yijbar[2,1] - Yijbar[2,2] 
D31_32 = Yijbar[3,1] -  Yijbar[3,2]
D41_42 = Yijbar[4,1] - Yijbar[4,2] 
D51_52 = Yijbar[5,1] - Yijbar[5,2] 
D61_62 = Yijbar[6,1] -  Yijbar[6,2]
 B <- qt(1-(0.05/(2*6)),6*2*(51))
 Tukey <- qtukey(0.95,12,6*2*51)/sqrt(2)
multiplier <- B
CI1 = c(D11_12 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D11_12 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))
CI2 = c(D21_22 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D21_22 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))
CI3 = c(D31_32 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D31_32 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))
CI4 = c(D41_42 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D41_42 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))
CI5 = c(D51_52 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D51_52 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))
CI6 = c(D61_62 - multiplier*sqrt(2*AnovaTable["Error","MS"]/52),D61_62 + multiplier*sqrt(2*AnovaTable["Error","MS"]/52))

mydat = data.frame(lower.bound = c(CI1[1],CI2[1],CI3[1],CI4[1],CI5[1],CI6[1]),upper.bound = c(CI1[2],CI2[2],CI3[2],CI4[2],CI5[2],CI6[2]))
mydat$lower.bound %<>% multiply_by(100)
mydat$upper.bound %<>% multiply_by(100)
rownames(mydat) = c( "u11-u12",  "u21-u22",  "u31-u32", "u41-u42", "u51-u52", "u61-u62")
pander(pandoc.table(mydat),style = 'rmarkdown')



