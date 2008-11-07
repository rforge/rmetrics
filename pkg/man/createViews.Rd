\name{Construct views}
\alias{createBLViews}
\alias{updateBLViews}
\alias{createCOPViews}
\title{Create or add to a view object using a graphical interface}
\description{
    These helper functions allow one to easily create or add to an object of class BLViews or COPViews through the use 
    of R's built-in data editor.  
}

\usage{
createBLViews(allAssets, numAssetViews = 1, assetSubset = NULL, mode = c("editor", "Window"))
updateBLViews(views, includeNullViews = FALSE, numNewViews = 0, assets = NULL)
createCOPViews (allAssets, numAssetViews = 1, assetSubset = NULL, mode = c("editor", "Window")) 

}

\arguments{
  \item{allAssets}{A character vector holding the names of all of the assets in one's "universe"}
  \item{numAssetViews}{The number of views to form.  Should be less than or equal to the total number of assets}
  \item{assetSubset}{A character vector of assets that is a subset of \code{allAssets}.  Views will be formed only on this subset. By default, \code{assetSubset = allAssets}}.
  \item{mode}{Mode of GUI.  Currently unused}
  \item{views}{Object of class BLViews}
  \item{assets}{Set of assets to form or modify views on.  If NULL, will use the full set of assets}
  \item{includeNullViews}{When updating views, should the 0 columns of the pick matrix be included?}
  \item{numNewViews}{In \code{updateViews}, this is the number of new views to add}
}
\details{
	\code{createCOPViews} does not allow one to specify the distributions of the views at the moment.  Such a feature
	may be added later through another GUI.  At the moment the object returned by this function has its distribution
	set to a default.  \code{updateViews} allows one to modify pre-existing views
}
\value{
    An object of class \code{BLViews} or \code{COPViews} that holds all of the views created.
}
\author{Francisco Gochez <fgochez@mango-solutions.com> }

\seealso{\code{\link{addBLViews}}, \code{\link{addCOPViews}}, \code{\link{COPViews}}, \code{\link{BLViews}}}
\examples{
    
    \dontrun{
        views <- createBLViews(colnames(monthlyReturns), 2)
    }
}
\keyword{IO}