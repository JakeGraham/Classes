
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

