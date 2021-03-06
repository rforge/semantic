\name{curl}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Alias
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\alias{curl}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Description
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\title{Curl: Performs a HTTP Request And Returns The Message Body}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\description{The function uses curlPerform to send a Query via HTTP 
using curlPerform.}
\usage{
curl(url, httpheader = NULL, display = TRUE, header = FALSE)
}
\arguments{
    \item{url}{character. The URL.}
    %
    \item{httpheader}{The message header which will be included into the
        message header (e.g. 'Accept="text/html"').
    }
    %
    \item{display}{logical. When TRUE, a status message from the returned header 
        will be displayed.
    }
    %
    \item{header}{logical. When TRUE, the message header will be included
        into the returned value.
    }
}
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\details{Uses curlPerform and displays a message from the returned HTTP header.}
%
\value{"character"" (or "list" when header is TRUE)}
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
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
% Examples
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %
\examples{
\dontrun{res <- htmlParse(curl("https://www.google.de"))}
}
\keyword{curl}
\keyword{curlPerform}
