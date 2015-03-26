\name{QueryVariables-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{QueryVariables-class}
\alias{initialize,QueryVariables-method}
%
\alias{queryVariables}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{QueryVariables: RDF Terms representing Query Variables In SPARQL}
\description{Represents query variables in SPARQL syntax. Query variables
are identified by a leading question mark. A SELECT Sparql Query 
produces data bindings for query variables.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{queryVariables}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
    \describe{
        \item{\code{ev}:}{Object of class \code{"environment"}.
            Inherited from RdfTerm.}
    }
}
\section{Extends}{
Class \code{"\linkS4class{RdfTerm}"}, directly.
}
\section{Methods}{
    \describe{
        \item{initialize}{\code{signature(.Object = "QueryVariables")}:
            Generic copy constructor.}
}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
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
\seealso{
\code{\linkS4class{SparqlQuery}}
\code{\linkS4class{RdfTerm}}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
queryVariables(c("s", "p", "o"))
}
\keyword{classes}
