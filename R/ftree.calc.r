# ftree.calc.R
# copyright 2015-2020, openreliability.org
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

ftree.calc<-function(DF, use.bdd=FALSE)  {					
## ftree.validate is used to check for ability to calculate by BDD					
	if(use.bdd == TRUE)  {				
		ftree.validate(DF)			
	}				
					
	if(!test.ftree(DF)) stop("first argument must be a fault tree")				
					
	 if(any(DF$Type==16)) {				
		stop("atleast gate requires SCRAM calculation")			
	 }				
					
					
		NDX<-order(DF$Level)			
		sDF<-DF[NDX,]			
					
## note the for loop starts at bottom working up					
for(row in dim(sDF)[1]:1)  {					
## only calculating gate nodes					
	if(sDF$Type[row] > 9)  {				
## Build the siblingDF starting with first child					
		child_rows<-which(sDF$CParent==sDF$ID[row])			
		if(!length(child_rows)>0)  stop(paste0("empty gate found at ID ", as.character(sDF$ID[row])))			
## the first child is of course at child-rows[1]					
	siblingDF<-data.frame(ID=sDF$ID[child_rows[1]],				
		CFR=sDF$CFR[child_rows[1]],			
		PBF=sDF$PBF[child_rows[1]],			
		CRT=sDF$CRT[child_rows[1]],			
		Type=sDF$Type[child_rows[1]],			
		P1=sDF$P1[child_rows[1]]			
		)			
## Fail rate for exposed type does not pass upward in calculations					
		if(siblingDF$Type[1]==5) {siblingDF$CFR[1]<- (-1) }			
					
	if(length(child_rows)>1)  {				
		if(sDF$Type[row]==15) {			
			stop("more than one feed to vote")		
		}			
					
		for(child in 2:length(child_rows))  {			
## thisChild is now at child_rows[child] in the sDF					
		DFrow<-data.frame(ID=sDF$ID[child_rows[child]],			
			CFR=sDF$CFR[child_rows[child]],		
			PBF=sDF$PBF[child_rows[child]],		
			CRT=sDF$CRT[child_rows[child]],		
			Type=sDF$Type[child_rows[child]],		
			P1=sDF$P1[child_rows[child]]		
			)		
## Fail rate for exposed type does not pass upward in calculations					
			if(DFrow$Type[1]==5) {DFrow$CFR[1]<- (-1) }		
					
			siblingDF<-rbind(siblingDF,DFrow)		
		}			
	}else{				
		if(sDF$Type[row]>10 && sDF$Type[row]!=15) {			
## less than 2 feeds to other than OR calc					
		stop(paste0("insufficient feeds at gate ", sDF$ID[row]))			
		}			
	}				
					
	## VOTE gate calculation				
	if(sDF$Type[row]==15)  {				
	resultDF<-VOTEcalc(siblingDF, c(sDF$P1[row],sDF$P2[row]))				
	}				
					
	## OR gate calculation				## The gate node is identified as sDF$ID[row]
	if(sDF$Type[row]==10)  {				
	resultDF<-ORcalc(siblingDF)				
	}				
					
	## AND gate calculation				
	if(sDF$Type[row]==11)  {				
	resultDF<-ANDcalc(siblingDF)				
	}				
					
	if(sDF$Type[row]>11 && sDF$Type[row]!=15)  {				
## Code is required in addXXX to assign the first entry as Condition==1)					
					
## test the Condition setting for the first child					
		##firstSib_DFrow<-which(DF$ID==siblingDF$ID[1] )			
		##secondSib_DFrow<-which(DF$ID==siblingDF$ID[2] )			
		Cond1<-DF$Condition[which(DF$ID==siblingDF$ID[1] )]			
		Cond2<-DF$Condition[which(DF$ID==siblingDF$ID[2] )]			
		if( !(Cond1 + Cond2) == 1 )  {			
			stop(paste0("No indication of Condition at ID", as.character(sDF$ID[row])))		
		}			
		if(Cond1==0)  {			
## re-order the siblingDF rows making sure new row names apply					
			siblingDF<-siblingDF[c(2,1),]		
			row.names(siblingsDF)<-c(1,2)		
		}			
## first feed must have probability of failure for remaining combination gates					
		if(siblingDF$PBF[1]<=0)  {			
			stop(paste0("first feed must have prob of failure at gate ", sDF$ID[row]))		
		}			
					
## with this code repairable does not need to be in json					
## cfr print does not have to be suppressed on conditional					
## eliminate fail rate (cfr) values (set to -1) for the conditional feed to advanced gates					
		sDF$CFR[which(sDF$ID==siblingDF$ID[1])]<- (-1)			
## eliminate repair time (crt) values (set to -1) for the conditional feed EXCEPT repairable condition					
		if (sDF$Cond_Code[row]%%10 == 0) {			
			sDF$CRT[which(sDF$ID==siblingDF$ID[1])]<- (-1)		
		}			
					
	}				
					
	## INHIBIT gate calculation				
	if(sDF$Type[row]==12)  {				
	resultDF<-INHIBITcalc(siblingDF)				
	}				
					
	if(sDF$Type[row]>12 && sDF$Type[row]<15)  {				
## second feed must have demand for remaining combination gates					
		if(siblingDF$CFR[1]<=0)  {			
			stop(paste0("second feed must have demand at gate ", sDF$ID[row]))		
		}			
	}				
					
	## ALARM gate calculation				
	if(sDF$Type[row]==13)  {				
	resultDF<-ALARMcalc(siblingDF, sDF$P1[row])				
	}				
					
	## COND gate calculation				
	if(sDF$Type[row]==14)  {				
## reversible condition must have repair time					
		if(sDF$Cond_Code[row]%%10==1 && siblingDF$CRT[1]<=0)  {			
			stop(paste0("reversible condition at gate ", sDF$ID[row]), " must have repair time")		
		}			
## Test whether Latent condition has been misplaced					
		if(siblingDF$Type[1]==1 && siblingDF$Type[2]==2) {			
			stop(paste0("Active set as condition for Latent component at gate ", sDF$ID[row]))		
		}			
					
	resultDF<-PRIORITYcalc(siblingDF, sDF$Cond_Code[row]%%10)				
	}				
					
					
## Fill the sDF with results of calculations					
	sDF$CFR[row]<- resultDF$CFR[1]				
	sDF$PBF[row]<-resultDF$PBF[1]				
	sDF$CRT[row]<-resultDF$CRT[1]				
					
## The gate node is identified as sDF$ID[row]					
	if(use.bdd == TRUE)  {	
##if(sDF$Type[row] < 10) browser()	
	sDF$PBF[row]<-probability(DF, method="bdd", ft_node=sDF$ID[row])				
	}				
					
					
	}  ## close logic type check				
}  ## next row					
					
	## reorder by ID				
	NDX<-order(sDF$ID)				
	DF<-sDF[NDX,]				
					
					
	DF				
}					

ANDcalc<-function(df)  {
## start progressive calculations of FR and PF, identify latency
	pFR<-df$CFR[1]
	pPF<-df$PBF[1]
	pAct=FALSE
## identify active events
	if(df$PBF[1]>0 && df$CRT[1]>0)  {
		if(abs(df$CRT[1]-1/(df$CFR[1]*(1/df$PBF[1]-1))) < df$CRT[1]*10e-5)  {
			pAct=TRUE
		}
	}

## collect positive RT values
	RTvec<-NULL
	if(df$CRT[1]>0)  RTvec<-c(RTvec, df$CRT[1])

	for(sib in 2:dim(df)[1])  {
## single combination calculations
		cFR<-df$CFR[sib]
		cPF<-df$PBF[sib]
## collect positive RT values
	if(df$CRT[sib]>0)  RTvec<-c(RTvec, df$CRT[sib])
	cAct=FALSE
## identify active events
	if(df$PBF[sib]>0 && df$CRT[sib]>0)  {
		if(abs(df$CRT[sib]-1/(df$CFR[sib]*(1/df$PBF[sib]-1))) < df$CRT[sib]*10e-5)  {
			cAct=TRUE
		}
	}

		x1FR<-pPF*cFR
		if(x1FR<0)  {x1FR<-0}
		x2FR<-cPF*pFR
		if(x2FR<0)  {x2FR<-0}
		pFR<-x1FR+x2FR
		pPF<-pPF*cPF


	## return any non-entries to -1 indication
		if(pPF<0) {
			pPF<- (-1)
		}else{
	## second order fail rate adjustment often negligable
			if(x1FR>0&&x2FR>0)  {
			pFR<-pFR-(x2FR/cPF+cFR)*pPF
			}

		}
		if(!pFR>0)  {pFR<- (-1)}

		if(pAct && cAct ){
	## calculate progressive RT for combination of two actives
			pRT<- pPF/pFR
			RTvec<-c(RTvec, pRT)
		}else{
	## CRT will simply be minimum of any positive RT's
			pAct=FALSE
			if(pFR>0 && pPF>0) {
	## just in case an implied RT is the  minimum here (not really expected to ever be used)
	## on second thought, don't do this

				pRT<-min(RTvec)
				if(pAct || cAct) {
					warning("Active component combined with Latent in an AND gate")
				}

			}else{
				pRT<--1
			}
		}


	}  ## next child, if any






## prepare results as output
	outDF<-data.frame(
		CFR=pFR,
		PBF=pPF,
		CRT=pRT
		)

	outDF
}

ORcalc<-function(df)  {
	pFR<-df$CFR[1]
	pPF<-df$PBF[1]
	pRT<-df$CRT[1]
	FRaa<-NULL
	AVaa<-NULL
## get active analog availability for first component
	if(pFR>0 && pRT>0) {
		AVaa<-c(AVaa, (1-pRT/(1/pFR+pRT)))
		FRaa<-c(FRaa, pFR)
	}else{
		AVaa<-c(AVaa, 1)
		FRaa<-c(FRaa, 0)
	}
## exclude special case where OR has single child
	if(dim(df)[1]==1)  {
		outDF<-data.frame(
			CFR=pFR,
			PBF=pPF,
			CRT=pRT
		)
	}else{
		for(sib in 2:dim(df)[1])  {
			cFR<-df$CFR[sib]
			cPF<-df$PBF[sib]
			cRT<-df$CRT[sib]

## get active analog availability vector for rest of components
		if(cFR>0 && cRT>0) {
			AVaa<-c(AVaa, (1-cRT/(1/cFR+cRT)))
			FRaa<-c(FRaa, cFR)
		}else{
			AVaa<-c(AVaa, 1)
		}
## zero out any non-entries prior to calculations
		if(pFR<0) {pFR<-0}
		if(pPF<0) {pPF<-0}
		if(cFR<0) {cFR<-0}
		if(cPF<0) {cPF<-0}
## the progressing OR calculation 2x2 (non-entries are zero thus have no effect)
		pFR<-pFR+cFR
		pPF<-1-(1-pPF)*(1-cPF)
## return any non-entries to -1 indication for final or next iteration
		if(pFR==0) {pFR<- (-1)}
		if(pPF==0)  {pPF<- (-1)}
		}


## prepare the CRT based on active analog values for an operating line
		TAVaa<-prod(AVaa)
		TFRaa<-sum(FRaa)
		if(TAVaa<1)  {
			crt<-1/(TFRaa*(1/(1-TAVaa) -1))
		}else{
			crt<- -1
		}


## prepare results as output
	outDF<-data.frame(
		CFR=pFR,
		PBF=pPF,
		CRT=crt
		)
	}

	outDF
}

ALARMcalc<-function(df, humanPBF)  {
## already have validated that df has two rows
## must have validated the siblingDF for first feed probability before this call
## secondary feed must have CFR

	outDF<-data.frame(
	## this was first test that tested close to APTree, but it violates proper sum of probabilities
		##CFR=df$PBF[1]*df$CFR[2]+humanPBF*df$CFR[2],
		##PBF=(1-(df$PBF[1]*df$PBF[2]-1)*(humanPBF*df$PBF[2]-1)),
		CFR=(1-(df$PBF[1]-1)*(humanPBF-1))*df$CFR[2],
		PBF=(1-(df$PBF[1]-1)*(humanPBF-1))*df$PBF[2],
		CRT= -1
		)

	outDF
}

INHIBITcalc<-function(df)  {
## already have validated that df has two rows
## must have validated the siblingDF for first feed probability before this call
## it is possible to combine two probabilities as an AND here, but why would one do this?
	if(df$CFR[2]>0) {
		CFRout<-df$PBF[1]*df$CFR[2]
	}else{
		CFRout<- -1
	}

	if( df$PBF[2]>0) {
		PBFout<-df$PBF[1]*df$PBF[2]
	}else{
		PBFout<- -1
	}


	outDF<-data.frame(
		CFR=CFRout,
		PBF=PBFout,
		CRT=df$CRT[2]
		)

	outDF
}

PRIORITYcalc<-function(df, reversible)  {
## already have validated that df has two rows
## must have validated the siblingDF for first feed probability before this call
## sequential event must have demand
## must validate that reversible condition has CRT>0
	CFRout<-df$PBF[1]*df$CFR[2]
## special case of irreversible condition
	if(reversible==0) {
		PBFout= -1
		CRTout= -1
	}else{
## identify latency
		if(df$PBF[1]>df$CRT[1]/(df$CRT[1]+1/df$CFR[1]))  {
			validCRT<-NULL
			for(n in 1:2) {
				if(df$CRT[n]>0) validCRT<-c(validCRT,df$CRT[n])
			}
			if(length(validCRT)>0) {
				CRTout<-min(validCRT)
				PBFout<- CRTout/(CRTout+1/CFRout)
			}else{
				CRTout<- -1
				PBFout<- -1
			}
		}else{
## this is the convolution solution
			if(df$CRT[1]<=df$CRT[2]) {
				CRTout<-df$CRT[1]/2
			}else{
				CRTout<-df$CRT[2]-(1/2)*df$CRT[2]*df$CRT[2]/df$CRT[1]
			}
			PBFout<- CRTout/(CRTout+1/CFRout)
		}
	}
	
	outDF<-data.frame(
		CFR=CFRout,
		PBF=PBFout,
		CRT=CRTout
		)

	outDF
}

VOTEcalc<-function(df, comb)  {
## input is a single element
## input must have probability value to avoid div by zero error already checked but just to be sure. . .
	if(!df$PBF[1]>0) {
		stop("combination calculation attempted with non-positive probability")
		}
## if only df$PBF has pos value (df$CFR<0) then CFR and CRT will automatically be -1 at end of calculations
## if input has latency (not active), then CRT will be input CRT at end of calculations
## integer vector - comb[1] must be less than comb[2]

## solve for the Binomial Coefficient
	BC<-factorial(comb[2])/(factorial(comb[1])*factorial(comb[2]-comb[1]))


## start progressive calculations of FR only if input CFR>0
if(df$CFR[1]>0)  {
	pFR<-df$CFR[1]
	pPF<-df$PBF[1]

	if(comb[1] >1)  {
		for(k in 2:comb[1])  {
			cFR<-df$CFR[1]
			cPF<-df$PBF[1]

	## cross multiply LB1*P2 + LB2*P1 for first order fail rate
	##  minus second order fail rate adjustment  - (LB1+LB2) * P1*P2
	## - (LB1+LB2) * P1*P2
			pFR<-(pPF*cFR + cPF*pFR) - (pFR+cFR) * pPF*cPF
			}
		}
## finalize using Binomial Coefficient
	CFR<-pFR*BC


}else{
CFR<-(-1)
CRT<-(-1)
}

##  PB calc
	pPF<-df$PBF[1]^comb[1]
## finalize using Binomial Coefficient
	PBF<- 1-(1-pPF)^BC

## identify active events for CRT calculation, else just return input CRT
## note division by zeo prevented at start of function as input PBF cannot equal 0
	CRT=df$CRT[1]
	if(df$CRT[1]>0 && df$CFR[1]>0)  {
		if(abs(df$CRT[1]-1/(df$CFR[1]*(1/df$PBF[1]-1))) < df$CRT[1]*10e-5)  {
			pAct=TRUE
			CRT=1/(CFR*(1/PBF-1))
			}
		}
	outDF<-data.frame(
		CFR=CFR,
		PBF=PBF,
		CRT=CRT
		)

	outDF
}
