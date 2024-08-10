#This Rscript is to plot Fig 5 and 6#

rm(list = ls())
library(ggplot2)
library(patchwork)

#Effect of no missing season
mon<-AA<-timing<-result<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  a<-result[i]
  AA[i]<-m
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# plot(AA[1:50]/12,result[1:50],pch=19,ylab="titers by season",xlab="years",col="black",lwd=2,cex.lab=1.5, cex.axis=1.5,ylim = c(1.0,2.5))
# lines(AA[1:50]/12,result[1:50],col="black",lwd=4)

#Effect of missing seasons every 8 years
mon<-BB<-timing<-resultB<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultB[i]<-x
  a<-resultB[i]
  BB[i]<-m
  
  if(i %% 8!=0){
    A=1
  }else {A=0}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(BB[1:50]/12,resultB[1:50],pch=19,ylab="titers by season",xlab="years",col="blue",lwd=2)
# lines(AA[1:50]/12,resultB[1:50],col="blue",lwd=4)

#Effect of missing seasons every 4 years
mon<-CC<-timing<-resultC<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultC[i]<-x
  a<-resultC[i]
  CC[i]<-m
  if(i %% 4!=0){
    A=1
  }else {A=0}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(CC[1:50]/12,resultC[1:50],pch=19,ylab="titers by season",xlab="years",col="red",lwd=2)
# lines(CC[1:50]/12,resultC[1:50],col="red",lwd=4)

df<-data.frame(season=c(AA[1:50]/12,BB[1:50]/12,CC[1:50]/12),
           titer=c(result[1:50],resultB[1:50],resultC[1:50]),
           group=c(rep("group 1",length(AA[1:50])),rep("group 2",length(BB[1:50])),rep("group 3",length(CC[1:50]))))
p1<-ggplot(df,aes(x=season,y=titer,fill=group,colour=group,group=group))+
  geom_point(size=2.5)+
  geom_line(size=1.5)+
  xlab("Season number") + 
  ylab("Antibody level \n at end of the season")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(a)")


#Effect of no worse seasons
mon<-AA<-timing<-result<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  a<-result[i]
  AA[i]<-m
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# plot(result[9:198],result[10:199],pch=19,col="black",cex.lab=1.5, cex.axis=1.5,xlab="titers of last season",ylab="titers of this season",xlim = c(1,2.8),ylim = c(1,2.8))

#Effect of worse seasons every 8 years
mon<-AA<-timing<-resultB<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultB[i]<-x
  a<-resultB[i]
  AA[i]<-m
  
  if(i %% 8!=0){
    A=1
  }else {A=0}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(resultB[9:198],resultB[10:199],pch=19,col="blue")

#Effect of no worse seasons every 4 years 
mon<-AA<-timing<-resultC<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultC[i]<-x
  a<-resultC[i]
  AA[i]<-m
  
  if(i %% 4!=0){
    A=1
  }else {A=0}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(resultC[9:198],resultC[10:199],pch=19,col="red")

df<-data.frame(titer1=c(result[9:198],resultB[9:198],resultC[9:198]),
               titer2=c(result[10:199],resultB[10:199],resultC[10:199]),
               group=c(rep("group 1",length(result[9:198])),rep("group 2",length(result[9:198])),rep("group 3",length(result[9:198]))))
p2<-ggplot(df,aes(x=titer1,y=titer2,fill=group,colour=group,group=group))+
  geom_point(size=2.5)+
  xlab("Antibody level \n at end of current season") + 
  ylab("Antibody level \n at end of preceding season")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(b)")

#Effect of no missing seasons
mon<-AA<-timing<-result<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  a<-result[i]
  AA[i]<-m
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# plot(AA[1:50]/12,result[1:50],pch=19,ylab="titers by season",xlab="years",col="black",lwd=2,cex.lab=1.5, cex.axis=1.5,ylim = c(1.5,3.5))
# lines(AA[1:50]/12,result[1:50],col="black",lwd=4)

#Effect of missing seasons every 8 years
mon<-BB<-timing<-resultB<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultB[i]<-x
  a<-resultB[i]
  BB[i]<-m
  
  if(i %% 8!=0){
    A=1
  }else {A=2}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(BB[1:50]/12,resultB[1:50],pch=19,ylab="titers by season",xlab="years",col="blue",lwd=2)
# lines(BB[1:50]/12,resultB[1:50],col="blue",lwd=4)

#Effect of missing seasons every 4 years
mon<-CC<-timing<-resultC<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultC[i]<-x
  a<-resultC[i]
  CC[i]<-m
  
  if(i %% 4!=0){
    A=1
  }else {A=2}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(CC[1:50]/12,resultC[1:50],pch=19,ylab="titers by season",xlab="years",col="red",lwd=2)
# lines(CC[1:50]/12,resultC[1:50],col="red",lwd=4)

df<-data.frame(season=c(AA[1:50]/12,BB[1:50]/12,CC[1:50]/12),
               titer=c(result[1:50],resultB[1:50],resultC[1:50]),
               group=c(rep("group 1",length(AA[1:50])),rep("group 2",length(BB[1:50])),rep("group 3",length(CC[1:50]))))
p3<-ggplot(df,aes(x=season,y=titer,fill=group,colour=group,group=group))+
  geom_point(size=2.5)+
  geom_line(size=1.5)+
  xlab("Season number") + 
  ylab("Antibody level \n at end of the season")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(c)")


#Effect of worse seasons  
mon<-AA<-timing<-result<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  a<-result[i]
  AA[i]<-m
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# plot(result[9:198],result[10:199],pch=19,col="black",cex.lab=1.5, cex.axis=1.5,xlab="titers of last season",ylab="titers of this season",xlim = c(2.0,3.8),ylim = c(2,3.8))

#Effect of worse seasons every 8 years
mon<-AA<-timing<-resultB<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultB[i]<-x
  a<-resultB[i]
  AA[i]<-m
  
  if(i %% 8!=0){
    A=1
  }else {A=2}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(resultB[9:198],resultB[10:199],pch=19,col="blue")

#Effect of worse seasons every 4 years
mon<-AA<-timing<-resultC<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m
while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  resultC[i]<-x
  a<-resultC[i]
  AA[i]<-m
  
  if(i %% 4!=0){
    A=1
  }else {A=2}
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
# points(resultC[9:198],resultC[10:199],pch=19,col="red")

df<-data.frame(titer1=c(result[9:198],resultB[9:198],resultC[9:198]),
               titer2=c(result[10:199],resultB[10:199],resultC[10:199]),
               group=c(rep("group 1",length(result[9:198])),rep("group 2",length(result[9:198])),rep("group 3",length(result[9:198]))))
p4<-ggplot(df,aes(x=titer1,y=titer2,fill=group,colour=group,group=group))+
  geom_point(size=2.5)+
  xlab("Antibody level \n at the end of current season") + 
  ylab("Antibody level \n at end of preceding season")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(d)")

p1+p2+p3+p4

#Fig6 
mon<-AA<-timing<-result<-c()
T1=3
A=1
b=pi/2
x=2.24
i<-1
omega=1/24
m<-12
month<-m

while (month < 2400) {
  T2=m+2        
  integrand1 = function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
  q=integrate(integrand1, lower = m-2, upper = m+2)
  y=x * exp(-omega*(T2-T1))+A*q$value
  T1=T2
  x=y
  result[i]<-x
  a<-result[i]
  AA[i]<-m
  
  #titers from last season change the timing of the peak of next season
  if(a>=2.44){m2=runif(n=1,min=13,max=15)}
  else if(a>=2.33 & a<2.44){m2=runif(n=1,min=12,max=14)}
  else if(a<2.33 & a>=2.24){m2=runif(n=1,min=11,max=13)}
  else{m2=runif(n=1,min=10,max=12)}
  
  month=month+12
  m=month+m2
  mon[i]<-m2
  i<-i+1  
}
par(mfrow = c(1,2))
plot(result[9:198],result[10:199],pch=19,col="black",cex.lab=1.5, cex.axis=1.5,xlab="titers of last season",ylab="titers of this season")

df<-data.frame(month=mon,titer=result)
df<-df[-2,]
df<-df[-1,]

lm1<-lm(month~ titer, data=df)
plot( df$titer, df$month ,pch=19,col="black",cex.lab=1.5, cex.axis=1.5,xlab="titers",ylab="month") 
abline( lm1,lwd=2)
