
# This library is free software, you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation, either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library, if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA


################################################################################
# FUNCTION:
#  .corDist
#  .kendallDist
#  .spearmanDist
#  .mutinfoDist
# FUNCTION:
#  .euclideanDist
#  .maximumDist
#  .manhattanDist 
#  .canberraDist
#  .binaryDist
#  .minkowskiDist 
# FUNCTION:
#  .braycurtisDist
#  .mahalanobisDist
#  .jaccardDist
#  .differenceDist
#  .sorensenDist
################################################################################


.corDist <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = as.dist(1-cor(x))
    
    # Return Value:
    dist
}


.kendallDist <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = as.dist(1-cor(x, method = "kendall"))
    
    # Return Value:
    dist
}


.spearmanDist <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = as.dist(1-cor(x, method = "spearman"))
    
    # Return Value:
    dist
}


.mutinfoDist <-
function(x, nbin=10)
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # borrowed from R package bioDist
    
    # Distance:
    x <- as.matrix(x)
    nc <- ncol(x)
    nr <- nrow(x)
    clist <- vector("list", length=nr)
    for(i in 1:nr) clist[[i]] <- cut(x[i,], breaks=nbin)
    ppfun <- function(pp) {pp<-pp[pp>0]; -sum(pp*log(pp ))}
    appfun <- function(x,y) {
        ppfun(table(x)/nc)+ppfun(table(y)/nc) - ppfun(c(table(x, y)/nc))}
    rvec<-rep(NA, nr*(nr-1)/2)
    ct <- 1
    for(i in 1:(nr-1)) {
        for(j in (i+1):nr) {
            rvec[ct] <- appfun(clist[[i]], clist[[j]])
            ct <- ct+1
        }
    }
    dist = 1 - sqrt(1 - exp(-2*rvec)) 
        
    # Return Value:
    dist
}
    
    
################################################################################
# from base R:
# "euclidean", "maximum", "manhattan", 
# "canberra", "binary", "minkowski" 
  
    
.euclideanDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = dist(x, "euclidean")
    
    # Return Value:
    dist
}


.maximumDist <-
function(x)
{   
    # FUNCTION:
    
    # A function implemented by Diethelm Wuertz
    
    # Distance:
    dist = dist(x, "maximum")
    
    # Return Value:
    dist  
}


.manhattanDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = dist(x, "manhattan")
    
    # Return Value:
    dist
}


.canberraDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = dist(x, "canberra")
    
    # Return Value:
    dist
}


.binaryDist <-
function(x)
{   
    # Distance:
    dist = dist(x, "binary")
    
    # Return Value:
    dist
}


.minkowskiDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = dist(x, "minkowski")
    
    # Return Value:
    dist
}


################################################################################


# from ecodist:
# "euclidean",   "bray-curtis", "manhattan", 
# "mahalanobis", "jaccard",     "difference" 
# "sorensen" 


.braycurtisDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "bray-curtis")
    
    # Return Value:
    dist
}


.mahalanobisDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "mahalanobis")
    
    # Return Value:
    dist
}


.jaccardDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "jaccard")
    
    # Return Value:
    dist
}


.differenceDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "difference")
    
    # Return Value:
    dist 
}


.mahalanobisDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "mahalanobis")
    
    # Return Value:
    dist
}


.sorensenDist <-
function(x)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Distance:
    dist = .ecodist(x, "sorensen")
    
    # Return Value:
    dist 
}


################################################################################


