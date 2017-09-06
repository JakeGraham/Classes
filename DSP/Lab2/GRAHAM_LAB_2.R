####################################################################

############		Jake Graham			############
############		 DSP Lab 2			############

####################################################################


###	Q1
	## Funciton for the signal, taks time (s) and returns amplitude
s1 <-function(t){
	out <- sin(1.9*2*pi*t)
	return(out);
}
step1 <- 10/10000
sf1 <- 1/step1
nq1 <- sf1*2
t1 <- seq(step1, 10, step1)
q1 <- s1(t1)
plot(t1, q1, type = "l")
system("mv Rplots.pdf GRAHAM_LAB_2.pdf")
print(paste("Nyquist Frequency =", nq1, " Hz"))

###	Q2
step2 <- step1*50
sf2 <- 1/step2
nq2 <-sf2*2
t2 <- seq(step2, 10, step2)
q2 <- s1(t2)
plot(t2, q2, type = "l") 
print(paste("Nyquist Frequency =", nq2, "Hz & Sample Frequency = 1.9 Hz"))

###	Q3
step3 <- step1*500
sf3 <- 1/step3
nq3 <- sf3/2
t3 <- seq(step3, 10, step3)
q3 <- s1(t3)
plot(t3, q3, type = "l")
print(paste("Nyquist Frequency =", nq3, "Hz"))
print("Aliased Frequency ~ 0.1 Hz")

###	Q4
step4 <- step1*476
sf4 <- 1/step4
nq4 <- sf4/2
t4 <- seq(step4, 10, step4)
q4 <- s1(t4)
plot(t4, q4, type = "l")
print(paste("Nyquist Frequency =", nq4, "Hz and Aliased Frequency ~ 0.2 Hz"))

###	Q5

	# Function to calculate aliasing frequency from sample frequency. If nyquist freqeuncy
	# is greater than signal frequency returns "no aliasing", otherwise returns alias 
	# frequency in Hertz 
FalSample <- function(F,Fs){
	SpC <- Fs/F
	if(Fs/F > 2){
		Fa <- NA		
	} else {
		Fa <- abs(F - (floor(Fs/F)*Fs) )
	}
	DFO <- cbind(F,Fs,Fa)
	colnames(DFO) <- c("SigFreq","SmplFreq","AliasFreq")
	return(DFO)
}
	# Function to calculate aliasing frequency from nyquist frequency. If nyquist freqeuncy
	# is greater than signal frequency returns "no aliasing", otherwise returns alias 
	# frequency in Hertz
FalNyq <- function(F,Ny){
	Fs <- Ny*2	
	if(Fs/F > 2){
		return("No Aliasing")
	} else {
	Fal <- abs(F - (floor(Fs/F)*Fs) )
	print(" ~ ")
	print(paste("Freq =", F))
	print(paste("Sample Freq =", Fs))
	print(paste("Nyquist Freq =", Ny))
	print(paste("Samples per cycle", (Fs/F)))
	return(Fal)
	}
}

# test on sampling frequencies from questions 1 - 4
FalSample(1.9,sf1)
FalSample(1.9,sf2)
FalSample(1.9,sf3)
FalSample(1.9,sf4)

FalNyq(1.9,nq1)
FalNyq(1.9,nq2)
FalNyq(1.9,nq3)
FalNyq(1.9,nq4)

