

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Load prerequisites
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

require(rdf)
ns <- namespace("PREFIX owl: <http://www.w3.org/2002/07/owl#>")
ns <- append(ns, c("rdfs", "www.w3.org/2000/01/rdf-schema#"))

ns2 <- namespace("PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>")
ns <- c(ns, ns2)

ns3 <- append(ns, c("rdfs", "www.w3.org/2000/01/rdf-schema#"))

ns3 <- c(ns, ns2)


sq <- selectQuery(ns)
addDataset(sq) <- "?x"
addModifier(sq) <- "bla"
