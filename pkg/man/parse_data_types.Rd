\name{parse_data_types}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{parse_data_types}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{parse_data_types: Parses Text Containing A RDF Typed Value}
\description{Parses typed strings and returns a data.frame.}
\usage{parse_data_types(text)}
\arguments{
    \item{text}{character. Text elements which will be parsed.}
}
\details{The text is processed in multiple steps.
The first step separates at "^^" into value and type identifier.
The second step removes leading and trailing "<>" from type identifier.
The third step removes "http://www.w3.org/2001/XMLSchema#" prefix
from identifier.}
\value{data.frame with two columns.}
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\examples{
parse_data_types("2142^^<http://www.w3.org/2001/XMLSchema#int>")
}
\keyword{Typed values}
