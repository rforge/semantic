\name{RdfData-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{RdfData-class}
%
\alias{initialize,RdfData-method}
\alias{rdfData}
%
\alias{setRdf,RdfData,RdfGraph-method}
\alias{setRdf,RdfData,RdfTriple-method}
\alias{setRdf,RdfData,SparqlNamespace-method}
%
\alias{show,RdfData-method}
%
\alias{qryText,RdfData-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{RdfData: Container Class For An RDF Graph.}
%
\description{Container class for an RDF graph.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{rdfData()}.
}
\section{Slots}{
    \describe{
        \item{\code{ptr}:}{Object of class \code{"externalptr"}:
            Pointer to external C++ object which keeps the RDF Data
            graph.}
    }
}
\section{Methods}{
    \describe{
        \item{initialize}{\code{signature(.Object = "RdfData")}:
            Generic copy constructor. }
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "RdfGraph")}:
            Replace contained RdfData member. }
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "RdfTriple")}: 
            Replace contained RdfData member. The provided RdfTriple object
            is internally wrapped inside an RdfGraph object.}
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "SparqlNamespace")}:
            Sets or replaces Namespace object (which keeps PREFIX encodings).}
        %
        \item{setRdf}{\code{signature(object = "RdfData", rdf = "RdfGraph")}: 
            Sets or replaces RdfData member in object.}
        %
        \item{show}{\code{signature(object = "RdfData")}: 
            Produces sensible textual output on Terminal. }
        %
        \item{qryText}{\code{signature(object = "RdfData")}: 
            Returns textual representation of content. }
}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% References
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
\seealso{
\code{\linkS4class{RdfGraph}}
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
rt <- rdfTerm(c("s", "p", "o"))
rg <- rdfGraph(rdfTriple(rt[1], rt[2], rt[3]))
rdfd <- rdfData()
setRdf(rdfd, rg)
rdfd
qryText(rdfd)
}
\keyword{classes}
