\name{parseUri}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{parseUri}
\title{parseUri: Prepares a PREFIX uri represenation for import into 
Namespace object.}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{Parses URI core out of a HTTP address: ' <http://uri> ' will be
converted to 'uri'. This is a preparatory step for importing whole
PREFIX sections into a Namespace object.}
%
\usage{parseUri(text)}
\arguments{
    \item{text}{character.}
}
\details{The function acts in three steps: 
1) Removes leading and trailing white spaces.
2) Removes '<http://' prefix.
3) Removes '>' suffix.}
\value{character.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
parseUri(" <http://xmlns.com/foaf/0.1/>")
}
\keyword{parseUri}
\keyword{Namespace}
