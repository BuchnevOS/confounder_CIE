
Cr_Conf <- function(T, X) {
  sapply(X, function(k) {rnorm(n = 1, mean = T*k, sd = 10)})
}  
  library(arm)
  library(dplyr) 
  options(max.print=999999)
  mas<-data.frame(J=integer(),I=integer(),L=integer(),T=numeric(),b=numeric(),p1=numeric(),sd=numeric(),b_=numeric(),p2=numeric(),sd_=numeric(),b_change=numeric(), sd_change=numeric())
  #mas
  L<-100
  N<-1000
  k<-0.1
  j<-1
  T_array<-c(0,0.1,0.5,1,5,10)
  while (!(k>0.5)) {
    print(j)
    i<-1
    while (!(i>6)) {
      print(i)
      T<-T_array[i]
      for (l in 1:L) {
        dat <- data.frame(X = rbinom(n = N, size = 1, prob = k))
        dat$C <- Cr_Conf(T, dat$X)
        dat$Y <- with(dat, 3 + 1.5*C + 20*X + rnorm(n = N, mean = 0, sd = 10)) 
        dat$Y
        dat$X <- as.factor(dat$X)
        model1 <- lm(Y ~ X, data = dat)
        summary(model1)
        model2 <- lm(Y ~  C+X, data = dat)
        summary(model2)
  
        b         <-  coef(model1)["X1"]
        p1        <-  summary(model1)$coef["X1", "Pr(>|t|)"]
        sd        <-  summary(model1)$coef["X1", "Std. Error"] 
        b_        <-  coef(model2)["X1"]
        p2        <-  summary(model2)$coef["X1", "Pr(>|t|)"]
        sd_       <-  summary(model2)$coef["X1", "Std. Error"] 
        b_change  <-  (b_-b)*100/b
        sd_change <-  (sd_-sd)*100/sd 
        new_row   <-  c(j,i,l,T,b,p1,sd,b_,p2,sd_,b_change,sd_change)
        mas[nrow(mas)+1,]<-new_row
      }
      i=i+1
    }
    k=k+0.1
    j=j+1
  
  }
I_max<-i-1
I_max
mas
  #вывод
plot(dat, col=c("red","black")[dat$X], pch=c(16,17)[dat$X], cex=2)
par(mfrow = c(2, 3))
parameters<-c("b","b_","b_change","sd","sd_","sd_change")
for (m in 1:6){ 
  parameter<-parameters[m]
  print(parameter)
  y_min=100000
  y_max=-100000
  for (j in 1:5) {
    mas_0<-filter(mas,J==j)
    for (i in 1:I_max) {
      mas_1<-filter(mas_0,I==i)
      if (m==3) {y<-quantile(mas_1[,parameter],0.5)}
      else {y<-mean(mas_1[,parameter])}
      if (y<y_min) {y_min<-y}
      if (y>y_max) {y_max<-y}
    }
  }
  for (j in 1:5) {
    mas_0=filter(mas,J==j)
    T<-NULL
    k1<-NULL
    for (i in 1:I_max) {
      mas_1=filter(mas_0,I==i)
      print(mas_1)
      T<-c(T,mas_1[1,"T"]) 
      if (m==3) {k1<-c(k1,quantile(mas_1[,parameter],0.5))}
      else { k1<-c(k1,mean(mas_1[,parameter]))}
    }
    if (j==1) { plot(T,k1,ylab=parameter, ylim=range(c(y_min,y_max)),type="l", lty =1,  lwd=2.0, col="red",  main=parameter, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)}
    if (j==2) {lines(T,k1,ylab=parameter, ylim=range(c(y_min,y_max)),type="l", lty =2,  lwd=2.0, col="green",main=parameter)}
    if (j==3) {lines(T,k1,ylab=parameter, ylim=range(c(y_min,y_max)),type="l", lty =3,  lwd=2.0, col="blue", main=parameter)}
    if (j==4) {lines(T,k1,ylab=parameter, ylim=range(c(y_min,y_max)),type="l", lty =4,  lwd=2.0, col="brown",main=parameter)}
    if (j==5) {lines(T,k1,ylab=parameter, ylim=range(c(y_min,y_max)),type="l", lty =5,  lwd=2.0, col="black",main=parameter)}
  }
  if (m==3){
  legend(6.2, 1, legend=c("k=0.1", "k=0.2","k=0.3","k=0.4","k=0.5"),col=c("red", "green","blue","brown", "black"), lty=1:5, cex=1.1,lwd=2.0)
  }
}


