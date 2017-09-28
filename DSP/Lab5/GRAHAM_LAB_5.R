##############				Jake Graham                             ############
##############                           DSP Lab 5                             ############
 
######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab5#######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_5.R; gnome-open GRAHAM_LAB_5.pdf  ####

############################################################################################

### Q1
N <- 16
K <- N/2 + 1
ff <- as.matrix(seq(0,.5,length = K)*2*pi)
n <- as.matrix(0:(N-1))
dim(n)
dim(ff)
res <- cos(ff%*%t(n))
ims <- sin(ff%*%t(n))
plot(1:length(res[3,]),res[3,], type = "l")
lines(1:length(res[4,]),res[4,], col = "red")
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
RE
plot(1:length(tot),tot,type = "l")
