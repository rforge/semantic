\name{RdfTerm-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{RdfTerm-class}
% Construction methods
\alias{initialize,RdfTerm-method}
\alias{show,RdfTerm-method}
%
\alias{rdfTerm}
\alias{rdfTerm-methods}
\alias{rdfTerm,ANY-method}
\alias{rdfTerm,character-method}
\alias{rdfTerm,IriTerm-method}
%
\alias{length,RdfTerm-method}
%
\alias{addRdf}
\alias{addRdf-methods}
\alias{addRdf,RdfTerm,IriTerm-method}
\alias{addRdf,RdfTerm,character-method}
%
\alias{setRdf}
\alias{setRdf,RdfTerm,character-method}
%
\alias{c,RdfTerm-method}
%
\alias{[-methods}
\alias{[,RdfTerm-method}
%
\alias{qryText,RdfTerm-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{RdfTerm: Base Class for RDF Terms}
%
\description{Basic type representing RdfTerms.
Various classes derive from RdfTerm: IriTerm, ResultDef, QueryVariable}
\section{Objects from the Class}{Can be created using the \code{rdfTerm}
    function.}
%
\section{Slots}{
    \describe{
        \item{\code{ev}:}{Object of class \code{"environment"}.
            Environment containing a \code{text} vector (character).}
    }
}
%
\section{Methods}{
    \describe{
        \item{initialize}{\code{signature(.Object = "RdfData")}:
            Copy constructor. }
        %
        \item{addRdf}{\code{signature(object = "RdfData", 
            rdf = "character")}:
            Add new RDF terms to object. }
        %
        \item{setRdf}{\code{signature(object = "RdfData", 
            rdf = "RdfTriple")}: 
            Replaces RDF terms in objects.}
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
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{
\code{\linkS4class{IriTerm}}
\code{\linkS4class{ResultDef}}
\code{\linkS4class{QueryVariable}}
\code{\linkS4class{PrefixedTerm}}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
rt <- rdfTerm(c("SPARQL", "RDF", "Term"))
addRdf(rt, c("a", "b", "c"))
rt[3:5]
}
\keyword{classes}
