\name{filterGO}
\alias{filterGO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Filter GO. }
\description{
  Filter out genes from a list not mapping to the actual ontology. Genes
  not mapping to the currently set ontology ("BP","MF","CC") and not
  having one of the predefined evidence codes (default is to use all
  evidence codes) are removed. 
}
\usage{
filterGO(genelist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genelist}{ character vector of Entrez gene IDs }
}
\value{
   List with items
   \item{"genename"}{gene ID}
   \item{"annotation"}{character vector of GO terms mapping to the gene within the actual ontology}
}
\author{ Holger Froehlich }
\note{  The result depends on the currently set ontology. IMPORTANT: The result refers to the GO library that was used to precompute the information contence of GO terms.
}
\seealso{ \code{\link{setOntology}}, \code{\link{setEvidenceLevel}}, \code{\link{getGOInfo}}, \code{\link{calcICs}}}
\examples{
\donttest{
	filterGO(c("12345","4559"))	
}
}
\keyword{ file }% at least one, from doc/KEYWORDS
