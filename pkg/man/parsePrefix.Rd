\name{parsePrefix}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{parsePrefix}
\title{parsePrefix: Extracts complete namespace data from single
textual PREFIX representation}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{parsePrefix is the basic function for parsing textual PREFIX
collections into a abbreviated identifier and a full URI.}
\usage{
parsePrefix(text)
}
\arguments{
    \item{text}{character. Each vector elements is parsed as a single PREFIX
        (=Namespace element).}
}
\details{The function proceeds in multiple steps.
The first step divides lines (delimited by '\\n') into separate vector elements.
The second step removes the 'PREFIX' suffix.
The third step divides the remnant at the ":" into two constituents.
The prefix is used as is. the Suffix is passed to the 'parseUri' function
(which removes "<..>" and  'http//:' prefix. )
}
\value{data.frame with two columns.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\examples{
parsePrefix("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>")
}
\keyword{Namespace}
\keyword{Prefix}
