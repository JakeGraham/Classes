############################################################################################

##############				Jake Graham				############
##############			 	 DSP Lab 2				############

######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab2 #######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_2.R; gnome-open GRAHAM_LAB_2.pdf    ####

####################################################################
library(tuneR)
library(R.matlab)
library(signal)

### Name etc
plot(NULL,col = "white", xaxt = "n", yaxt = "n", ylab = NA, xlab = NA, bty = "n",xlim = c(0,10), ylim = c(0,10))
text(5, 6, "Jake Graham", cex = 2.5)
text(5, 5, "DSP Lab 2", cex = 2.5)


###	Q1
	## Funciton for the signal, taks time (s) and returns amplitude
s1 <-function(t){
	out <- sin(1.9*2*pi*t)
	return(out);
}
step1 <- 1/1000
sf1 <- 1/step1
nq1 <- sf1/2
t1 <- seq(step1, 10, step1)
q1 <- s1(t1)
plot(t1, q1, type = "l", xlab = "Time (s)", ylab = "Amplitude", main = paste("Part 1 Q1:   Nyquist Frequency =", nq1, " Hz; Plotted Frequency = 1.9 Hz") )
system("mv Rplots.pdf GRAHAM_LAB_2.pdf")
print(paste("Nyquist Frequency =", nq1, " Hz"))

###	Q2
step2 <- step1*50
sf2 <- 1/step2
nq2 <-sf2/2
t2 <- seq(step2, 10, step2)
q2 <- s1(t2)
plot(t2, q2, type = "l", xlab = "Time (s)", ylab = "Amplitude", main = paste("Part 1 Q2:   Nyquist Frequency =", nq2, " Hz; Plotted Frequency = 1.9 Hz")) 
print(paste("Nyquist Frequency =", nq2, "Hz & Sample Frequency = 1.9 Hz"))

###	Q3
step3 <- step1*500
sf3 <- 1/step3
nq3 <- sf3/2
t3 <- seq(step3, 10, step3)
q3 <- s1(t3)
plot(t3, q3, type = "l", xlab = "Time (s)", ylab = "Amplitude", main = paste("Part 1 Q3:   Nyquist Frequency =", nq3, " Hz; Plotted Frequency ~ 0.1 Hz"))
print(paste("Nyquist Frequency =", nq3, "Hz"))
print("Aliased Frequency ~ 0.1 Hz")

###	Q4
step4 <- step1*476
sf4 <- 1/step4
nq4 <- sf4/2
t4 <- seq(step4, 10, step4)
q4 <- s1(t4)
plot(t4, q4, type = "l", xlab = "Time (s)", ylab = "Amplitude", main = paste("Part 1 Q4:   Nyquist Frequency =", round(nq4, 2), " Hz; Plotted Frequency ~ 0.2 Hz"))
print(paste("Nyquist Frequency =", round(nq4, 4), "Hz and Aliased Frequency ~ 0.2 Hz"))

###	Q5

	# Function to calculate aliasing frequency from sample frequency. If nyquist freqeuncy
	# is greater than signal frequency returns "no aliasing", otherwise returns alias 
	# frequency in Hertz 
FalSample <- function(F,Fs){
	SpC <- Fs/F
	Ny <- Fs/2
	if(Fs/F > 2){
		Fa <- NA		
	} else {
		Fa <- abs(F - (floor(Fs/F)*Fs) )
	}
	DFO <- cbind(F,Fs,Ny,Fa)
	colnames(DFO) <- c("SigFreq","SmplFreq","NyquistFreq","AliasFreq")
	return(DFO)
}
	# Function to calculate aliasing frequency from nyquist frequency. If nyquist freqeuncy
	# is greater than signal frequency returns "no aliasing", otherwise returns alias 
	# frequency in Hertz
FalNyq <- function(F,Ny){
	Fs <- Ny*2	
	SpC <- Fs/F
	if(Fs/F > 2){
		Fa <- NA
	} else {
		Fa <- abs(F - (floor(Fs/F)*Fs) )
	}
	DFO <- cbind(F,Fs,Ny,Fa)
	colnames(DFO) <- c("SigFreq","SmplFreq","NyquistFreq","AliasFreq")
	return(DFO)
}

# test on sampling frequencies from questions 1 - 4
FalNyq(1.9,nq1)
FalNyq(1.9,nq2)
FalNyq(1.9,nq3)
FalNyq(1.9,nq4)

plot(NULL, xlim=c(0,10), ylim=c(0,10), yaxt = "n", xaxt = "n", col = "white", xlab = NA, ylab = NA, bty = "n")
text(5, 6, "Part 1 Q5:")
text(5, 5, "Alias Frequency = |FreqSig - floor( ( FreqNyq*2 ) / FreqSig) * FreqSig|")

############		Part 2

###	Q1
whis <- readMat("whistle.mat")
w1 <- Wave(left = whis$Y, bit = 32, pcm = FALSE)
plot(w1, xlab = "Time (s)", main = paste("Part 2 Q1:   Nyquist Frequency =", whis$Fs[1] / 2, " Hz"))
mtext("Amplitude", 2, 2.5)
play(w1, "play")

###	Q2
seq12 <- seq(0, 12, length(whis$Y))
y12 <-rep(NA, length(seq12))
for (i in 1:length(whis$Y)){
	y12[floor(i / 12)] <- whis$Y[i]
}
w2 <- Wave(left =  y12, samp.rate = whis$Fs[1] / 12, bit = 32, pcm = FALSE)
plot(w2, xlab = "Time (s)", main = paste("Part 2 Q2:   Nyquist Frequency =", (whis$Fs[1] / 12) / 2, " Hz"))
mtext("Amplitude", 2, 2.5)
play(w2, "play")

###	Q3
yd12 <- decimate(whis$Y,12)
w3 <- Wave(left = yd12, samp.rate = whis$Fs[1] / 12, bit = 32, pcm = FALSE)
plot(w3, xlab = "Time (s)", main = paste("Part 2 Q3:   Nyquist Frequency =", (whis$Fs[1] / 12) / 2, " Hz"))
mtext("Amplitude", 2, 2.5)
play(w3, "play")

###	Q4
par(mai = c(0,0,0,0))
plot(NULL, xlim = c(0,10), ylim = c(0,10), yaxt = "n", xaxt = "n", col = "white", xlab = NA, ylab = NA, bty = "n")
text(5, 7, "Part 2 Q4:")
text(5, 6.5, "Whistles in 2 & 3 are distorted because they have been down-sampled")
text(5, 6, "and some of the original signal has been lost.")
text(5, 5.5, "The distortions are different because they were down-sampled in different manners.")
text(5, 5, "The plot/sound in question 2 is aliased.")
text(5, 4.5, "The decimate used a low pass filter to prevent aliasing.")

