\name{typedLiteral}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{typedLiteral}
\alias{typedLiteral-methods}
\alias{typedLiteral,character-method}
\alias{typedLiteral,integer-method}
\alias{typedLiteral,logical-method}
\alias{typedLiteral,numeric-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{typedLiteral: Construction Of RDF Typed Literal Representations}
\description{Constructs an RdtTerm object containing a typed literal
from R vectors.}
\usage{typedLiteral(object)}
\arguments{
    \item{object}{Object from which typed literal is created.}
}
%\details{}
\value{RdfTerm}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Reference
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{\code{\linkS4class{RdfTerm}}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
tl <- typedLiteral(42L)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
