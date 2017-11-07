#library(signal)
im1 <- as.matrix(read.csv("img1.txt"), rownames=FALSE)
im2 <- as.matrix(read.csv("img2.txt"))
m1 <- as.matrix(read.csv("mask1.txt"))
m2 <- as.matrix(read.csv("mask2.txt"))


decon <- function(II, MI){
	OP <- fft( (fft(II)) / (fft(MI)), inverse = TRUE)
	return(abs(OP))
}

test <- decon(im1, m1)
image(test)

#fftshift <- function(IP){
#	tmp = matrix(NA, ncol=ncol(IP), nrow=nrow(IP))
#	tmp[1:nrow(tmp)/2, 1:ncol(tmp)/2] <- IP[(nrow(tmp)/2)+1:(nrow(tmp)), 1(nrow(tmp)/2)+1:(nrow(tmp))]
#	tmp[(nrow(tmp)/2)+1:(nrow(tmp)), 1(nrow(tmp)/2)+1:(nrow(tmp))] <- tmp[1:nrow(tmp)/2, 1:ncol(tmp)/2]
#	tmp[1:nrow(tmp)/2, 1:ncol(tmp)/2] <- tmp[(nrow(tmp)/2)+1:(nrow(tmp)), 1(nrow(tmp)/2)+1:(nrow(tmp))]
#	tmp[1:nrow(tmp)/2, 1:ncol(tmp)/2] <- tmp[(nrow(tmp)/2)+1:(nrow(tmp)), 1(nrow(tmp)/2)+1:(nrow(tmp))]
#	return(tmp)
#}
#t2 <- fftshift(test)
#image(t2)


comps2polar <- function(IP){
	angle <- atan(Im(IP)/Re(IP))
	mag <- sqrt(Re(IP)^2 + Im(IP)^2)
	return(list(mag,angle))
}

decon2 <- function(II, MI, WL){
	tmp <- matrix(NA, nrow = nrow(II), ncol = ncol(II))
	ftI <- fft(II)
	ftM <- fft(MI)
	pi <- comps2polar(ftI)
	pm <- comps2polar(ftM)
	#pm[pm < WL] <- WL
	mout <- pi[[1]] / pm[[1]]
 	pout <- pi[[2]] - pm[[2]]
	for(i in 1:ncol(mout)){
		for(j in 1:nrow(mout)){
			tmp[i][j] <- complex(real = mout[i][j], imaginary = pout[i][j])
		}
	}
print(	str(tmp))
print(	dim(tmp))
#	OP <- fft(tmp, inverse = TRUE)
	return(tmp)	
}

test2 <- decon2(im1, m1, .00001);
t3 <- fft(test2, inverse = TRUE)
#image(t3)
#warnings()



