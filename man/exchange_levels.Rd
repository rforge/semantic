\name{exchange_levels}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{exchange_levels}
\title{exchange_levels: Character encoding in whole data.frame}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Exchanges factor encoding for a whole data.frame.}
%
\usage{
exchange_levels(dfr, oldlevels, newlevels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dfr}{data.frame: The object in which encoding is exchanged.
        All columns in this data.frame must be of type character.}
    %
    \item{oldlevels}{Set of initial factor levels. These levels are used because
        the assumed factor string sets in the input data.frame only 
        represent a subset of the enconding which is needed afterwards.}
    %
    \item{newlevels}{Set of new factor levels which replace the old levels.}
}
\details{Empty entries (nchar(x)==0) remain unchanged.}
\value{data.frame where dimension is identical to input object.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
#
}
\keyword{data.frame}
\keyword{factor}