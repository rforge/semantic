\name{curl}
\alias{curl}
\title{curl: Performs a HTTP request and returns the message body.}
\description{The function uses curlPerform to send a Query via HTTP 
using curlPerform.}
\usage{
curl(url, httpheader = NULL, display = TRUE, header = FALSE)
}
\arguments{
    \item{url}{character. The URL.}
    %
    \item{httpheader}{The message header which will be included into the
        message header (e.g. 'c(Accept="text/html')}
    %
    \item{display}{logical. When TRUE, a status message from the returned header 
        will be displayed.}
    %
    \item{header}{logical. When TRUE, the message header will be included
        into the returned value.}
}
\details{Uses curlPerform and displays a message from the returned HTTP header.}
%
\value{"character"" (or "list" when header is TRUE)}
%
\references{
SPARQL 1.1 Query Language W3C Recommendation
\url{http://www.w3.org/TR/sparql11-query/}
}
\author{Wolfgang Kaisers}
%
\examples{
\dontrun{
res <- htmlParse(curl("https://www.google.de"))
}
}
\keyword{curl}
\keyword{curlPerform}
