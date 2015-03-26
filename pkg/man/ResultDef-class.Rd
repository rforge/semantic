\name{ResultDef-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{ResultDef-class}
% Construction methods
\alias{initialize,ResultDef-method}
\alias{resultDef}
\alias{resultDef-methods}
\alias{resultDef,character-method}
\alias{resultDef,missing-method}
\alias{resultDef,RdfTerm-method}
% Slot accessors
\alias{addRdf,ResultDef,character-method}
\alias{addRdf,ResultDef,RdfTerm-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{ResultDef: Definition Of Variable Bindings Returned
By A SPARQL Query}
\description{The class contains a set of Rdf terms (e.g. variable bindings)
which will be returned from a SPARQL query.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{resultDef}.}
%
\section{Slots}{
    \describe{
        \item{\code{ev}:}{Object of class \code{"environment"}
            Environment containing a text (character) vector.
    }
}}
\section{Extends}{
Class \code{"\linkS4class{RdfTerm}"}, directly.
}
\section{Methods}{
    \describe{
        \item{addRdf}{\code{signature(object = "ResultDef", rdf = "character")}:
            Adds RDF terms to the result definition.}
        %
        \item{addRdf}{\code{signature(object = "ResultDef", rdf = "RdfTerm")}: 
            Adds a set of RDF terms to result definition.}
        %
        \item{initialize}{\code{signature(.Object = "ResultDef")}: 
            Generic copy constructor.}
}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
%%\note{}
%%\section{Warning }{....}
\seealso{
\code{\linkS4class{RdfTerm}}
%% \code{\link{~~fun~~}}
%% \code{\linkS4class{CLASSNAME}}
}
\examples{
# Create RdfTerm's
bns <- blankNodes()
ba <- getNodes(bns, 2)
qv <- queryVariable(c("q", "s"))
# Create object
rdef <- resultDef(c(ba, qv[1]))
rdef
# Use object inside SPARQL query
spq <- sparqlQuery()
setRdf(spq, rdef)
}
\keyword{classes}
