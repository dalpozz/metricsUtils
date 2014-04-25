library(utils)

## Code from Miguel Lopes - April 2014
## estimates binary classification performance against random
## input:
## "scores" -> scores of belonging to positive class
## "y" -> positive class different than 0, negative class 0
## "params" (optional) -> mean and variance of null distribution (much faster if provided)
#*********************
# example
# y <- c(1,1,1,0,0,0,1,0,1,0)
# phat.1 <- c(0.9,0.8,0.4,0.5,0.3,0.2,0.8,0.3,0.8,0.3)
# pranker(phat.1, y)
#*********************
pranker <- function(scores, y, params, conf.level = 0.95){
  if (missing(params)){
    estimate.p=TRUE}
  ranking=as.numeric(scores)
  y=as.numeric(y)
  #cat("computing auprc (average precision) \n")##
  auprc=auprc.ap( scores, y)
  if (estimate.p==TRUE){
    #cat("computing parameters of null distribution:\n") ## 
    P=length(which(y!=0))
    N=length(y)
    params=null.params(N,P)
  }
  nullMEAN=params[1]
  nullVAR=params[2] 
  k=1:P
  min.possible=sum(k/(N-P+k)*(1/P))
  a=min.possible
  b=1	
  mean.beta=(nullMEAN-a)/(b-a)
  var.beta=nullVAR/(b-a)^2
  par1=mean.beta^2*((1-mean.beta)/var.beta)-mean.beta
  par2=par1*(1/mean.beta)-par1
  auprc=(auprc-a)/(b-a) 
  pv=pbeta(auprc, par1, par2, lower.tail=FALSE)
  p1 <- (1-conf.level)/2
  p2 <- (1+conf.level)/2
  quantiles <- qbeta(c(p1, p2), par1, par2)
  names(quantiles) <- paste(c(p1, p2) * 100,"%")
  
  out <- list(auprc = auprc*(b-a)+a, pvalue = pv, nullparams = params, instances = c(total = N, positives = P), conf.int = quantiles)
  return(out)
}


## null distribution parameters
null.params <- function(N, P){
  nullMEAN=null.auprc(N,P)
  nullCOV=covMAT(N,P)
  ### put symmetric
  nullCOV=nullCOV+t(nullCOV)
  ## improve - do a C function for this ##
  for (k1 in 1:P){
    nullCOV[k1,k1]=var_k1(k1,N,P) 
  }
  nullVAR=sum(nullCOV) ## 
  out=c(mean = nullMEAN, var = nullVAR)
  return(out)
}

## expected null aupcr for maximum precision approach
null.auprc <- function(N, P){		
  prec.all.k=NULL
  prec.all=0	
  cat("compute expected null aupcr:\n")
  pb   <- txtProgressBar(1, P, style=3)
  for(k in 1:P){
    prec.k=0
    for(n in k:N){  
      hyp=dhyper( (k-1),P,(N-P),(n-1))
      psel=hyp*(P-(k-1))/(N-(n-1))
      prec.k=prec.k+psel*(k/n)
    }
    prec.all=prec.all+prec.k
    prec.all.k=c(prec.all.k,prec.k)
    setTxtProgressBar(pb, k)
  }
  cat("\n")
  p.max=prec.all/P
  out=p.max
  return(out)
}

## hypergeometric modification 
prob_function<-function(N,P,k,n){	
  out=dhyper(k-1,P-1,N-P,n-1)*(P/N)
  return(out)
}
### variance for recall k)
var_k1<-function(k,N,P){
  ## calculate expected value
  total=0
  for (n in k:N){
    add=prob_function(N,P,k,n)
    add=add*(k/n)
    total=total+add
  }
  total=total/(P)		
  expect=total
  ## calculate expected value for k*k
  total=0
  for (n1 in k:N){
    add=prob_function(N,P,k,n1)
    add2=(k/(P*n1))^2
    add=add*add2
    total=total+add
  }
  expect2=total
  var.out=expect2-expect^2
  return(var.out)
}

## covariance 
library(Rcpp)
library(inline)
code<-'
Rcpp::IntegerVector Ni(Nii);
Rcpp::IntegerVector Pi(Pii);

int N=as<int>(Ni);
int P=as<int>(Pi);

std::vector<double> ekMULT(N, 0);
std::vector<double> out(P, 0);
std::vector<double> ek(P, 0);

Rcpp::NumericMatrix ekjALL(P,P);
Rcpp::NumericMatrix covMAT(P,P);

double ekSUM;

for (int k = 1; k<P+1; k++ ) {	
	ekSUM=0;
	for (int nk=k; nk<(N-P+k+1); nk++) {	
		ekMULT[nk-1]=R::dhyper(k-1,P-1,N-P,nk-1,0)*(k)/(nk*P*N);
		ekSUM=ekSUM+(ekMULT[nk-1]*P);
		}
	ek[k-1]=ekSUM;	
	
	if (k<P){
	#pragma omp parallel for private(nk)
	for (int j = k+1; j<(P+1); j++){
		double ekj=0;
		double ekjMULT;
		double ekjMULT2;		
		int nk=k;
		int nj=nk+j-k;
		ekjMULT=R::dhyper(j-k-1,P-k-1,N-nk-P+k,nj-nk-1,0)*(P-k)*j/(nj*(N-nk));
		
		for (nk=k; nk<(N-P+k+1); nk++) {	
			if (nk>k){
				nj=nk+j-k;
				int MULT1=(N-nj-P+j+1)*(nj-(nk-1)-1)*(nj-1)*(nj-nk-j+k+1)*(N-nk+1);
				int MULT2=(N-nj+1)*(nj-(nk-1)-j+k)*nj*(nj-nk)*(N-nk+1-P+k);
				ekjMULT=ekjMULT*MULT1/MULT2;										
				}
			ekjMULT2=ekjMULT;
			
			for (nj=nk+j-k; nj<(N-P+j+1); nj++) {
				if (nj>nk+j-k){
					int MULT1=(N-nj-P+j+1)*(nj-nk-1)*(nj-1);
					int MULT2=(N-nj+1)*(nj-nk-j+k)*nj;
					ekjMULT2=ekjMULT2*MULT1/MULT2;				
					}
				ekj=ekj+(ekjMULT2*ekMULT[nk-1]);
				}
			}
		ekjALL(k-1,j-1)=ekj;
		}}
	}
for (int k = 1; k<P; k++ ) {	
	for (int j = k+1; j<(P+1); j++){
		covMAT(k-1,j-1)=ekjALL(k-1,j-1)-(ek[k-1]*ek[j-1]);
		}
	}	
return(covMAT);
'

inc <- '
#include <iostream>
using namespace std;
#include <omp.h>
'

settings <- getPlugin("Rcpp")
settings$env$PKG_CXXFLAGS <- paste('-fopenmp', settings$env$PKG_CXXFLAGS)
settings$env$PKG_LIBS <- paste('-fopenmp -lgomp', settings$env$PKG_LIBS)
covMAT <- cxxfunction(signature(Nii="numeric", Pii="numeric"),
                      plugin="Rcpp", inc=inc, body=code, settings=settings)	


## average precision auprc ##
auprc.ap<- function(prediction, truthmat) { 
  # rank
  ranked=sort(as.numeric(prediction),decreasing=TRUE,index.return=TRUE)			
  P=length(which(truthmat!=0))
  Total=length(truthmat)
  precisions=NULL
  tp_count=0
  for (i in 1:Total){
    ## is a true positive?
    if (truthmat[ranked[[2]][i]]!=0){
      tp_count=tp_count+1						
      precisions=c(precisions, (tp_count/i))
    }
  }
  out=mean(precisions)
  return(out)
}
