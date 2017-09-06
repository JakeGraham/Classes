####################################################################

############		Jake Graham			############
############		 DSP Lab 2			############

####################################################################


###	Q1
s1 <-function(t){
	out <- sin(1.9*2*pi*t)
	return(out);
}
step1 <- 10/10000
sf1 <- 1/step1
snq1 <- sf1*2
t1 <- seq(step1, 10, step1)
q1 <- s1(t1)
plot(t1, q1, type = "l")
system("mv Rplots.pdf GRAHAM_LAB_2.pdf")
print(paste("Sample Nyquist Frequency =", snq1, " Hz"))

###	Q2
step2 <- step1*50
sf2 <- 1/step2
snq2 <-sf2*2
t2 <- seq(step2, 10, step2)
q2 <- s1(t2)
plot(t2, q2, type = "l") 
print(paste("Sample Nyquist Frequency =", snq2, "Hz & Sample Frequency = 1.9 Hz"))

###	Q3
step3 <- step1*500
sf3 <- 1/step3
snq3 <- sf3/2
t3 <- seq(step3, 10, step3)
q3 <- s1(t3)
plot(t3, q3, type = "l")
print(paste("Sample Nyquist Frequency =", snq3, "Hz"))
print("Aliased Frequency ~ 0.1 Hz")

###	Q4
step4 <- step1*476
sf4 <- 1/step4
snq4 <- sf4/2
t4 <- seq(step4, 10, step4)
q4 <- s1(t4)
plot(t4, q4, type = "l")
print(paste("Sample Nyquist Frequency =", snq4, "Hz and Aliased Frequency ~ 0.2 Hz"))

###	Q5
Fal <- function(F,Fs){
	Fal <- abs((F/Fs)*Fs-F)
	print(F/Fs)
	print((F/Fs)*Fs)
	return(Fal)
}
Fal(1.9,sf1)
Fal(1.9,sf2)
Fal(1.9,sf3)
Fal(1.9,sf4)
