#To plot fig 2
rm(list = ls())
library(ggplot2)
library(patchwork)

y<-c()
x<-2.24
m<-12
T1<-3
omega=1/24
A<-1
b<-pi/2
t0<-10
boost<-0
for (i in 1:1100) {

  T2<-T1+i/100
  
  if(T2<=t0){
    boost<-0
  }else if (T2>t0 & T2 <=14){
    integrand1 <- function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
    q<-integrate(integrand1, lower = t0, upper = T2)
    boost<-q$value
  }else{
    boost<-0
  }
  
  y[i]=x * exp(-omega*(T2-T1))+A*boost
}
# plot(y)
# abline(v = 1200, col="red", lwd=3, lty=2)

for (i in 1101:1600) {
  T2<-T1+i/100
  y[i]=y[1100] * exp(-omega*(T2-14)) 
}
# plot(y)

df<-data.frame(t=T1+(1:1600)/100,titer=y)
m<-12
b<-pi/2
x<-seq(from=m-pi/b,to= m+pi/b,by=1/52)

df2<-data.frame(t=seq(from=m-pi/b,to= m+pi/b,by=1/52),
                y=b*(1 + cos(b*x - b*m))/(2*pi))
p1<-ggplot()+
  geom_line(data=df,aes(x=t,y=titer),size=2)+
  geom_line(data=df2,aes(x=t,y=y*1.8+1.5),size=2,colour="brown", linetype="dashed")+
  xlab("Month number") + 
  ylab("Antibody level")+
  scale_y_continuous(
    # Features of the first axis
    name = "Antibody level",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*0.2, name="Seasonal infectivity")
  ) +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  scale_x_continuous(breaks=c(3,4,5,6,7,8,9,10,11,12,13,14,15),labels = c(3,4,5,6,7,8,9,10,11,12,13,14,15),limits = c(3,15))+
  theme_minimal() +
  theme(
    axis.text.y.right = element_text(color = "brown"),
    axis.title.y.right = element_text(colour = "brown"),
    axis.line.y.right = element_line(color = "brown"), 
    text = element_text(size=20),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(a)")

result<-matrix(0,16,20)
j<-1
for (x in 0:15) {
  for (i in 1:20) {
    A<-1
    m<-12
    b<-pi/2
    T2<-15
    T1<-3
    omega=1/24
    
    integrand1 <- function(t) {b*(1 + cos(b*t - b*m))/(2*pi)*exp(-omega*(T2-t))}
    q<-integrate(integrand1, lower = m-pi/b, upper = m+pi/b)
    
    y=x * exp(-omega*(T2-T1))+A*q$value
    x<-y
    result[j,i]<-x
  }
  j<-j+1
}
plot(result[1,],pch=19,ylim=c(0,10),xlab = "number of seasons",ylab="titer",cex.lab=1.5, cex.axis=1.5)
points(result[15,],pch=19)
  
df<-data.frame(y=c(result[1,],result[15,]),
               x=c(seq(20),seq(20)))
p2<-ggplot(df,aes(x=x,y=y))+geom_point(size=3)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12),labels = c(0,2,4,6,8,10,12),limits = c(0,12))+
  theme_minimal() +
  xlab("Season number")+
  ylab("Antibody level")+
  theme(
    text = element_text(size=20),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )+ labs(tag = "(b)")
p1+p2

