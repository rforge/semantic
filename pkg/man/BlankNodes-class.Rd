\name{BlankNodes-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{BlankNodes-class}
\alias{initialize,BlankNodes-method}
\alias{show,BlankNodes-method}
%
\alias{blankNodes}
%
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,BlankNodes-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{BlankNodes: Creates Blank Nodes (RdfTerms)}
\description{The function creates unique blank node labels (e.g. _:a).
A single BlankNodes object creates consequtive labels which do not recur.
}
\usage{
blankNodes()
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Objects from the Class}{
Objects can be created by calls of the form \code{blankNodes()}.
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Structure
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Slots}{
    \describe{
        \item{\code{ptr}:}{Object of class \code{"externalptr"}
            Points to a C++ class which manages the creation of unique
            blank nodes.}
    }
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Methods}{
    \describe{
        \item{getNodes}{\code{signature(object = "BlankNodes")}: 
            Returns an RdfTerm object containing blank nodes.}
        %
        \item{initialize}{\code{signature(.Object = "BlankNodes")}: 
            Generic constructor}
        %
        \item{show}{\code{signature(object = "BlankNodes")}: 
            Produces sensible printout on terminal.}
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
\seealso{
\code{\linkS4class{RdfTerm}}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
# Blank node creator object
bns <- blankNodes()
# Create two blank nodes (RdfTerm's)
ba <- getNodes(bns, 2)
}
\keyword{classes}
