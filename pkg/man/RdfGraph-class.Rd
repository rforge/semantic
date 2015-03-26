\name{RdfGraph-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{RdfGraph-class}
% Basic generics
\alias{show,RdfGraph-method}
% Construction methods
\alias{initialize,RdfGraph-method}
%
\alias{rdfGraph}
\alias{rdfGraph-methods}
%
\alias{qryText,RdfGraph-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{RdfGraph: Represents A List Of RDF Triples}
\description{An RdfGraph is the data structure of a RdfData object.
Beside the RdfGraph, an RdfData object keeps a namespace.}
%
\section{Objects from the Class}{
Objects can be created by calls of the form \code{rdfGraph()}.
}
\section{Slots}{
    \describe{
        \item{\code{ptr}:}{Object of class \code{"externalptr"}:
            Points to a C++ object of class \code{rdf\_graph}.}
    }
}
\section{Methods}{
    \describe{
        \item{addRdf}{\code{signature(object = "RdfGraph", rdf = "RdfTriple")}:
            Adds RDF Triple to data graph. }
        %
        \item{initialize}{\code{signature(.Object = "RdfGraph")}: 
            Generic copy constructor.}
        %
        \item{qryText}{\code{signature(object = "RdfData", rdf = "RdfGraph")}: 
            Returns textual representation of content.}
        %
        \item{show}{\code{signature(object = "RdfGraph")}:
            Prints sensible textual representation to terminal.}
    }
}
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
\code{\linkS4class{RdfTriple}}
\code{\linkS4class{RdfTerm}}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
# Create RdfTriple
qv <- queryVariables(c("s", "p", "o"))
rt <- rdfTriple(qv[1], qv[2], qv[3])
# Create RdfGraph
rdg <- rdfGraph(rt)
#addRdf(rdg, rt)
rdg
}
\keyword{classes}
