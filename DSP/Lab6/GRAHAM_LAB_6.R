##############                          Jake Graham                             ############
##############                           DSP Lab 6                             ############

######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab6#######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_6.R; gnome-open GRAHAM_LAB_6.pdf  ####

############################################################################################
### Name etc
plot(NULL,col = "white", xaxt = "n", yaxt = "n", ylab = NA, xlab = NA, bty = "n",xlim = c(0,10), ylim = c(0,10))
text(5, 6, "Jake Graham", cex = 2.5)
text(5, 5, "DSP Lab 6", cex = 2.5)

library(RSEIS)
library(R.matlab)
library(jpeg)


repmat <- function(X,m,n){
	mx <- dim(X)[1]
	nx <- dim(X)[2]
	a <- matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n, byrow =T)
	return(a)
}

dft <- function(x){
#		if (dim(x)[1] < dim(x)[2]){
#			x <- t(x)
#		}
	
	K <- length(x)/2 + 1
	fax <- seq(0,pi,length = K )
	n <- 0:(length(x)-1)
	cs <- cos(n%*%t(fax))
	ss <- sin(n%*%t(fax))
	res <- cs*rep(x,K)
	ims <- ss*rep(x,K) 

	X <- complex(sum(res), sum(ims))
	return(X)
}


### Q1
q1 <- c(0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
dft(q1)



### Q2
q2 <- fft(q1)
x <- seq(0, 2*pi, length = length(q2))


plot(x,abs(q2), )
system("mv Rplots.pdf GRAHAM_LAB_6.pdf")
plot(x, Arg(q2),)
lines(x, Arg(q2))


### Q3
q3 <- c(q1,rep(0,4080))
pi/(length(q2)/2)
pi/(length(q3)/2)

### Q4
t1 <- Sys.time()
q4<-fft(q3)
t2 <- Sys.time()
t2-t1



### PART B 

### Q5
img <- readJPEG("WS.jpg",native = TRUE)
plot(1:2, type='n')
 rasterImage(img,1,1,2,2)
dim(img)
str(img)
420

### Q6
q6 <- img[440,] 
q6 <- detrend(q6)
x6 <- 1:length(q6)
length(q6)

plot(x6, q6, type = "l")









