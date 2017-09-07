
#function for producing square waves
 squarew <- function(x){  ## Apparently R doesn't have vectorized if statments?!?! even though everyting else supports it,
                          ## and ifelse has undefined behaviour in this situation.... zzz.. brute force I suppose...
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

