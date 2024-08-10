#To generate antibody trajectories over multiple years from random center of the season
rm(list = ls())
library(ggplot2)
library(gridExtra)

timing<-result<-mm<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
month<-0
while (month < 240) {
  k<-sample(x = seq(10,15),size = 1,replace = TRUE)
  mm[i]<-k
  m=month+k
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  i<-i+1
  month=month+12
  timing[i]<-m
}
# plot(timing[-1],result, xaxt="n",pch=19,ylim=c(2.0,2.8),xlab="number of seasons")
# lines(timing[-1],result, xaxt="n",xlab = "number of seasons")
# df4<-data.frame(month=timing[-1],titer=result)

df<-read.csv("df_fig3a.csv")
df<-data.frame(time=c(df$month,df$month.1,df$month.2,df$month.3,df$month.4),
               titer=c(df$titer,df$titer.1,df$titer.2,df$titer.3,df$titer.4),
               group=c(rep("trajectory 1",length(df$month)),rep("trajectory 2",length(df$month)),rep("trajectory 3",length(df$month)),rep("trajectory 4",length(df$month)),rep("trajectory 5",length(df$month))))
p1<-ggplot(df,aes(x=time,y=titer))+
  geom_line(size=1)+ 
  theme_bw()+
  facet_grid(group ~ .) +
  geom_point(size=1.5)+
  xlab("Number of seasons")+
  ylab("Antibody level in season")+
  labs(tag = "(a)")+
  theme(
    text = element_text(size=18)
  )

df<-read.csv("df_fig3a.csv")
df<-data.frame(timing=c(diff(df$month),diff(df$month.1),diff(df$month.2),diff(df$month.3),diff(df$month.4)),
               titerdiff=c(diff(df$titer),diff(df$titer.1),diff(df$titer.2),diff(df$titer.3),diff(df$titer.4)))
p2<-ggplot(df,aes(x=timing,y=titerdiff))+
  geom_point()+
  scale_x_continuous(breaks=c(6,8,10,12,14,16,18),labels = c(6,8,10,12,14,16,18),limits = c(6,18))+
  xlab("Difference between infectivity peaks (months)")+
  ylab("Difference between antibody levels")+
  theme_bw()+
  labs(tag="(b)")+
  theme(
    text = element_text(size=18))

#histgram
timing<-result<-mm<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
month<-0
while (month < 24000) {
  k<-sample(x = seq(10,15,0.1),size = 1,replace = TRUE)
  mm[i]<-k
  m=month+k
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  i<-i+1
  month=month+12
  timing[i]<-m
}
# hist(result,breaks = 20,main="",xlab = "",ylab="")
df<-data.frame(res=result)
p3<-ggplot(df,aes(x=res))+
  geom_histogram(color="black", fill="white")+
  theme_bw()+
  scale_y_continuous(breaks=c(20,60,100,140),labels = c(20,60,100,140),limits = c(0,140))+
  xlab("antibody levels")+
  ylab("")+
  labs(tag="(c)")+
  theme(
    text = element_text(size=18))

lay <- rbind(c(1,1,1,2,2),
             c(1,1,1,3,3))
grid.arrange(p1,p2,p3, layout_matrix = lay)

