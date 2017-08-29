###########################################################################

#####################     JAKE GRAHAM DSP LAB 1   #########################

###########################################################################
plot.new();plot.window(xlim=c(0,10),ylim=c(0,10));text(5,6, "Jake Graham",cex=1.5);text(5,5,"DSP",cex=1.5);text(5,4,"LAB 1",cex=1.5)


# Q1
t<-seq(0, 10 - (1/100), 1/100)
m1<-as.data.frame(cbind(t,(2 + sin(4*pi*t))))
colnames(m1)<-c("time","sin")
plot(m1$time, m1$sin, main = "Q1", ylab = "Volts", xlab = "Time (S)", bty = "l", las = 1, pch = 16, type = "l")
system("mv Rplots.pdf Graham_Lab1.pdf")

# Q2
msd<-sqrt( sum( (m1$sin - mean(m1$sin))^2) / (length(m1$sin) - 1) ) # manually calculate standard deviation
plot(0,bty="n",xlim=c(0,10),ylim=c(0,10),xaxt="n",yaxt="n",ylab=NA,xlab=NA,col="white",main = "Q2")
text(5,6,paste("Manual SD = " , round(msd,5) ) ); text(5,5,paste("Using R function SD() = " , round(sd(m1$sin), 5) ) )

# Q3 
# Function for producing square waves
squarew <- function(x){  ## Apparently R doesn't have vectorized if statments?!?! even though everyting else in R supports them,
			 ## and ifelse has undefined behaviour in this situation.... zzz... brute force I suppose...
	Vout <- rep(NA,length(x))
	for (i in 1:length(x)){
		if(sign(sin(x[i]))!=0){
			Vout[i] = sign(sin(x[i]))
		} else if( ( (x[i]/pi) %% pi) == 0){
			Vout[i] =  1
		} else if( ( (x[i]/pi) %% pi) != 0){
			Vout[i]  = -1
		}	
	}
	return(Vout)
}
m1$sqr<-(2 + squarew(4*pi*t))
plot(t,m1$sqr, main = "Q3", ylab = "Volts", xlab = "Time (S)", bty = "l", las = 1, pch = 16,type = "l")


# Q4
set.seed(1234)
m1$noise<-runif(1000,1,3) 
plot(m1$time, m1$noise, main = "Q4", bty = "l", las = 1, type = "l", ylab = "Volts", xlab = "Time (S)")


# Q5
sinh<-hist(m1$sin, breaks = seq(min(m1$sin),max(m1$sin), ( (max(m1$sin) - min(m1$sin) ) / 10 ) ), xlab = "Volts", main = "Q5 sin")
sqrh<-hist(m1$sqr, breaks = seq(min(m1$sqr),max(m1$sqr), ( (max(m1$sqr) - min(m1$sqr) ) / 10 ) ), xlab = "Volts", main = "Q5 Square")
noiseh<-hist(m1$noise, breaks = seq(min(m1$noise),max(m1$noise), ( (max(m1$noise) - min(m1$noise) ) / 10 ) ), xlab = "Volts", main = "Q5 Noise")

# Q6
atsin<-seq(0, max(sinh$counts), max(sinh$counts) / 3 )
labsin<-round(atsin/length(m1$sin),2)
atsqr<-seq(0, max(sqrh$counts), max(sqrh$counts) / 3 )
labsqr<-round(atsqr/length(m1$sqr),2)
atnoise<-seq(0, max(noiseh$counts), max(noiseh$counts) / 3)
labnoise<-round(atnoise/length(m1$noise),2)
hist(m1$sin, breaks = seq(min(m1$sin),max(m1$sin), ( (max(m1$sin) - min(m1$sin) ) / 10 ) ) , yaxt = "n", xlab = "Volts", ylab = "Probability Mass", main = "Q6 Sin")
axis(2, at = atsin, labels = labsin)
hist(m1$sqr, breaks = seq(min(m1$sqr),max(m1$sqr), ( (max(m1$sqr) - min(m1$sqr) ) / 10 ) ) , yaxt = "n", xlab = "Volts", ylab = "Probability Mass", main = "Q6 Square" )
axis(2, at = atsqr, labels = labsqr)
hist(m1$noise, breaks = seq(min(m1$noise),max(m1$noise), ( (max(m1$noise) - min(m1$noise) ) / 10 ) ) , yaxt = "n", ylab = "Probability Mass", xlab = "Volts", main = "Q6 Noise")
axis(2, at = atnoise, labels = labnoise)

# Q7
plot(density(m1$sin), bty = "l", ylab = "Probability Density", las = 1, main = "Q7 Sin pdf", xlab = "Volts")
plot(density(m1$sqr), bty = "l", ylab = "Probability Density", las = 1, main = "Q7 Square pdf", xlab = "Volts")
plot(density(m1$noise), bty = "l", ylab = "Probability Density", las = 1, main = "Q7 Noise pdf", xlab = "Volts")
