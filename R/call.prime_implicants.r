call.prime_implicants<-function(DF, ft_node=1)  {
	ftree.validate(DF)	

if(DF$Type[ft_node] < 10)  stop("ft_node must be a gate")
				
chars_in<-DF$Tag					
## DF$MOE now provided for cpp import, uncertainty removed					
ints_in<-c(DF$ID, DF$Type, DF$CParent, DF$MOE, DF$EType)					
## DF$CFR & DF$CRT removed from import					
nums_in<-c( DF$PBF, DF$P1, DF$P2)	
  
ret<-.Call( "prime_implicants", chars_in, ints_in, nums_in, ft_node=1, PACKAGE = "FaultTreeBDD" )

ret

}