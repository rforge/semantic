\name{GroupPattern-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{GroupPattern-class}
\alias{initialize,GroupPattern-method}
\alias{length,GroupPattern-method}
\alias{show,GroupPattern-method}
\alias{sparqlQuery,GroupPattern-method}
%
\alias{groupPattern}
\alias{groupPattern-methods}
\alias{groupPattern,missing-method}
%
\alias{addRdf,GroupPattern,BasicGroupPattern-method}
\alias{setRdf,SparqlQuery,GroupPattern-method}
%
\alias{gpText,GroupPattern-method}
\alias{qryText,GroupPattern-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{GoupPattern: Container for BasicGroupPattern objects}
\description{GroupPattern is a container for BasicGroupPattern objects.
In SPARQL group graph patterns are delimited by braces: \{\}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{groupPattern()}.
}
\section{Slots}{
    \describe{
        \item{\code{ev}:}{Object of class \code{"environment"} ~~ }
    }
}
\section{Extends}{
Class \code{"\linkS4class{BasicGroupPattern}"}, directly.
}
\section{Methods}{
    \describe{
        \item{addRdf}{\code{signature(object = "GroupPattern", 
            rdf = "BasicGroupPattern")}:
            Adds BasicGroupPattern to existing BasicGroupPattern list.
        }
        %
        \item{gpText}{\code{signature(object = "GroupPattern")}:
            Returns textual representation used inside GroupPattern objects.
        }
        %
        \item{qryText}{\code{signature(object = "GroupPattern")}:
            Returns textual representation used inside SparqlQuery
            object.
        }
        %
        \item{initialize}{\code{signature(.Object = "GroupPattern")}:
            Generic copy constructor.
        }
        %
        \item{length}{\code{signature(x = "GroupPattern")}: 
            Returns number of stord BasicGroupPattern objects.
        }
        %
        \item{setRdf}{\code{signature(object = "SparqlQuery", 
            rdf = "GroupPattern")}: 
            Sets GroupPattern.
        }
        %
        \item{show}{\code{signature(object = "GroupPattern")}:
            Produces sensible printout on terminal.
        }
        %
        \item{sparqlQuery}{\code{signature(object = "GroupPattern")}: 
            Creates a SparqlQuery object from a GroupPattern.
        }
    }
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Reference
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
Section 5.2
}
\author{Kaisers, W.}
%\note{}
%\section{New section}{....}
%\subsection{Sub section}{}
%\seealso{\code{\link{~~fun~~}} \code{\linkS4class{CLASSNAME}} }
\seealso{
\code{\linkS4class{RdfTerm}}
\code{\linkS4class{RdfTriple}}
}
\examples{
qv <- queryVariables(c("s", "p", "o"))
rt <- rdfTriple(qv[1], qv[2], qv[3])
gp <- groupPattern(rt)
gp
qryText(gp)
}
\keyword{classes}
