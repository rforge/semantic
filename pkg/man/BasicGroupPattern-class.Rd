\name{BasicGroupPattern-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{BasicGroupPattern-class}
\alias{groupPattern,BasicGroupPattern-method}
\alias{initialize,BasicGroupPattern-method}
\alias{show,BasicGroupPattern-method}
%
\alias{gpText}
\alias{gpText-methods}
\alias{gpText,BasicGroupPattern-method}
%
\alias{qryText,BasicGroupPattern-method}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{Class \code{"BasicGroupPattern"}}
\description{Abstract base class for class \code{GroupPattern}.
This base class is needed because a GroupPattern object shall contain 
a list of BasicGroupPattern objects.
The list shall be converted into a string during construction of
a SparqlQuery or RdfData class.
}
\section{Objects from the Class}{BasicGrouppattern is a virtual class: 
    No objects may be created from it.}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Structure
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\section{Slots}{
  \describe{
    \item{\code{ev}:}{Object of class \code{"environment"}.}
  }
}
\section{Environment content}{
\tabular{lll}{
    Name    \tab    Type        \tab Content\cr
    text    \tab    character   \tab Group pattern text\cr
}}
\section{Methods}{
    \describe{
        \item{addRdf}{\code{signature(object = "GroupPattern",
            rdf = "BasicGroupPattern")}:
            Adds BasicGroupPattern to GroupPattern object.
            Details are specified in GroupPattern object.
        }
        %
        \item{gpText}{\code{signature(object = "BasicGroupPattern")}:
            Unexported function. Produces specialized textual
            output for usage inside GroupPattern object.}
        %
        \item{qryText}{\code{signature(object = "BasicGroupPattern")}:
            Returns textual representation of data.}
        %
        \item{qryText}{\code{signature(object = "BasicGroupPattern")}:
            Produces textual output.}
        %
        \item{groupPattern}{\code{signature(object = "BasicGroupPattern")}: 
            Generates a GroupPattern object containing calling
            object packed inside BasicGroupPattern list.
        }
        %
        \item{initialize}{\code{signature(.Object = "BasicGroupPattern")}: 
            Generic copy constructor.
        }
        %
        \item{show}{\code{signature(object = "BasicGroupPattern")}:
            Produces sensible textual output on terminal.
        }
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
%\section{Warning }{....}
\seealso{
\code{\linkS4class{GroupPattern}}
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
qv <- queryVariables(c("s", "p", "o"))
rt <- rdfTriple(qv[1], qv[2], qv[3])
gp <- groupPattern(rt)
gp
qryText(gp)
}
\keyword{GroupPattern}
