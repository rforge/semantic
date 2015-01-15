\name{QueryVariable-class}
\Rdversion{1.1}
\docType{class}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{QueryVariable-class}
%
\alias{[,QueryVariable-method}
%
\alias{initialize,QueryVariable-method}
\alias{show,QueryVariable-method}
%
\title{Class \code{"QueryVariable"}}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{QueryVariable.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("QueryVariable", v)}.
}
\section{Slots}{
    \describe{
        \item{\code{v}:}{Object of class \code{"character"}. }
        %
        \item{\code{prefix}:}{Object of class \code{"character"}. }
    }
}
\section{Extends}{
Class \code{"\linkS4class{RdfTerm}"}, directly.
}
\section{Methods}{
    \describe{
        \item{[}{\code{signature(x = "QueryVariable")}: ... }
        \item{initialize}{\code{signature(.Object = "QueryVariable")}: ... }
        \item{show}{\code{signature(object = "QueryVariable")}: ... }
    }
}
\author{Wolfgang Kaisers}
\seealso{
  \code{\link{RdfTerm-class}}.
}
\examples{
showClass("QueryVariable")
}
\keyword{classes}
