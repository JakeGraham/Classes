##############				Jake Graham                             ############
##############                           DSP Lab 5                             ############
 
######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab5#######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_5.R; gnome-open GRAHAM_LAB_5.pdf  ####

############################################################################################
### Name etc
plot(NULL,col = "white", xaxt = "n", yaxt = "n", ylab = NA, xlab = NA, bty = "n",xlim = c(0,10), ylim = c(0,10))
text(5, 6, "Jake Graham", cex = 2.5)
text(5, 5, "DSP Lab 5", cex = 2.5)
 

### Q1
N <- 16
K <- N/2 + 1
ff <- as.matrix(seq(0,.5,length = K)*2*pi)
n <- as.matrix(0:(N-1))
res <- cos(ff%*%t(n))
ims <- sin(ff%*%t(n))
plot(1:length(res[3,]),res[3,], type = "l", main = "Q1", xlab = "Samples", ylab = "Amplitude")
lines(1:length(ims[3,]),ims[3,], col = "red")
legend("topright", c("Real","Imag"),col =  c("black","red"), lwd = 2)
system("mv Rplots.pdf GRAHAM_LAB_5.pdf")

### Q2
ph <- c(0, -.79, -1.57, -2.36, 3.14, -.79, -1.57, -2.36, -3.14)
am <- c(2, 1.85, 1.41, .77, 0, .77, 1.41, 1.85, 2)
ReX <- am * cos(ph)
ImX <- am * sin(ph)


### Q3
x <- rep(NA, 16)
RE <- ReX /(N/2)
IM <- -ImX /(N/2)
RE[9] <- ReX[1]/N
RE[9] <- ReX[9]/N
tot <- RE%*%res + IM%*%ims
plot(1:length(tot),tot,type = "l", main = "Q3", xlab = "Samples", ylab = "Amplitude")

