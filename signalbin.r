signalbin <- function(aa)
{
if (all(aa>=0)) ## Check no zero or negative values are in the input
{
nrow1 = nrow(aa)
ncol1 = ncol(aa)
xx = matrix(0,nrow1,ncol1)
for (j in 1:nrow1) {xx[j,1]=aa[j,1]; for (i in 2:ncol1){xx[j,i]=if((xx[j,i-1]+aa[j,i])<=100){xx[j,i-1]+aa[j,i]} else {101}}}
rm(i)
rm(j)
if (all(xx[,ncol1]==101)) # Check the resultant xx has column number >100
{
yy = matrix(0,nrow1,100)
for (j in 1:nrow1) 
{
i=1;
while (xx[j,i]<=99) 
{
yy[j,ceiling(xx[j,i])]=1-yy[j,ceiling(xx[j,i])]; 
for (n in (ceiling(xx[j,i])):99)
{
yy[j,n+1]=yy[j,n]
}
i=i+1
}
while (xx[j,i]<=100) 
{
yy[j,ceiling(xx[j,i])]=1-yy[j,ceiling(xx[j,i])]; 
i=i+1
}
}
yymean=colMeans(yy)
rm(i)
rm(j)
yymeanodd=yymean[seq(1,99,2)]
yymeaneven=yymean[seq(2,100,2)]
oddmean=mean(yymeanodd)
evenmean=mean(yymeaneven)
resultlist=list(ResponseArray100=yymean, OddMean=oddmean, EvenMean=evenmean)
return(resultlist)
}
else
{
cat("Warning:", "\n", "Please try another input matrix.", "\n", "A column number >130 is recommended.", "\n")
}
rm(nrow1)
rm(ncol1)
}
else
{
cat("Warning:", "\n", "Negative values exist and are not suitable for this simulation.", "\n", "Please try another input matrix.", "\n")
}
}
