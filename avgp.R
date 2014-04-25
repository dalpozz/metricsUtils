avgp <- function(y, phat, ties=FALSE)
{
# This version written by Mu Zhu, March 2005.
# Now takes care of ties ...
# Inputs:
# y    = true label; MUST be zero or one.
# phat = stuff used to rank the items; MUST be the same length as y.
# Assumes if phat[i] > phat[j], then item i should come ahead of item j.
# Can think of phat[i] as the predicted probability that item i is a one.

y<-data.matrix(y)
phat<-data.matrix(phat)
n<-length(phat)     # count total number of items
m<-sum(y)           # count total number of ones
sort.order <- order(phat, decreasing=TRUE)
pp <- phat[sort.order]
yy <- y[sort.order]

# this part takes care of ties
if (ties) {
anchor <- unique(pp); N <- length(anchor);
for (i in 1:N) {
 this <- which(pp==anchor[i])
 hits <- sum(yy[this])
 size <- length(this)
 yy[this] <- hits/size
}}

dr <- yy/m
p <- cumsum(yy)/(1:n)
return(sum(p*dr))

}
