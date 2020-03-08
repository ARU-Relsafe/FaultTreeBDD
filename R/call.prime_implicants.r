call.prime_implicants<-function(DF, ft_node=1, by="tag")  {

if(DF$Type[ft_node] < 10)  stop("ft_node must be a gate")
				
chars_in<-DF$Tag								
ints_in<-c(DF$ID, DF$Type, DF$CParent, DF$MOE, DF$EType)									
nums_in<-c( DF$PBF, DF$P1, DF$P2)
	
if(tolower(by)%in% c("tag","tags")) out_form<-1 else out_form<-0  
ret<-.Call( "prime_implicants", chars_in, ints_in, nums_in, ft_node=1, out_form, PACKAGE = "FaultTreeBDD" )

ret
}
