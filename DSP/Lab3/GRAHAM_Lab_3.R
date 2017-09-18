############################################################################################

##############                          Jake Graham                             ############
##############                           DSP Lab 3                              ############

######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab3 #######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_3.R; gnome-open GRAHAM_LAB_2.pdf    ####
 
############################################################################################

library(R.matlab)

### Name etc
plot(NULL,col = "white", xaxt = "n", yaxt = "n", ylab = NA, xlab = NA, bty = "n",xlim = c(0,10), ylim = c(0,10))
text(5, 6, "Jake Graham", cex = 2.5)
text(5, 5, "DSP Lab 3", cex = 2.5)


### Q1
data <- readMat("Lab3.mat")
sig <- ((data$data * as.numeric(data$hdr[10])) / 3200) * 10E5
time <- (1:length(sig))/as.numeric(data$hdr[9])
par(mai=c(.9, .9, 1, 1))
plot(time, sig - mean(sig), type = "l", main = "Q1:   Velocity", xlab = "Time (s)", ylab = expression( paste( "Amplitude (", mu, "m/s)")))
system("mv Rplots.pdf GRAHAM_LAB_3.pdf")


### Q2
dt <- 1 / as.numeric(data$hdr[9])
t2 <- time[2:length(time)] - (dt / 2)
acc <- rep(NA,length(sig)-1)
for (i in 2:length(sig)){
	acc[i-1] <- (sig[i] - sig[i-1]) / dt
}
plot(t2, acc, type = "l", main = "Q2:   Acceleration", xlab = "Time (s)", ylab = expression( paste( "Amplitude (", mu, "m/s"^"2", ")")))


### Q3I
disp <- rep(NA, length(sig))
cv <- 0
for (i in 1:length(sig)){
	cv <- cv + ((sig[i] - mean(sig)) * ( 1/ as.numeric(data$hdr[9])))
	disp[i] <- cv 
}

plot(time, disp, type = "l", main = "Q3:   Displacement", xlab = "Time (s)", ylab = expression( paste( "Amplitude (", mu, "m)")))

