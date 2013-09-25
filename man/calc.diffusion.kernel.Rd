\name{calc.diffusion.kernel}
\alias{calc.diffusion.kernel}
\alias{load.diffusion.kernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation and loading of diffusion kernel matrices}
\description{
  Manifold embeddings of gene ontology terms via diffusion kernel techniques. Diffusion kernels are positive semidefinite similarity measures calculated from the graph Laplacian. They are interpreted as the result of a local heat diffusion process along the graph structure. 
}
\usage{
calc.diffusion.kernel(method="diffKernelLapl", m=7, normalization.method="sqrt", DIR=".")

load.diffusion.kernel(method="diffKernelLapl", DIR=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{one of "diffKernelLapl", "diffKernelpower", "diffKernelLLE", "diffKernelexpm"}
  \item{m}{(1) Half the power of the transition probability matrix (an integer > 0). (2) an arbitrary positive time constant for the exponential diffusion kernel}
  \item{normalization.method}{method to normalize the kernel}
  \item{DIR}{directory, where to write ready calculated kernel matrices to and read them from, respectively. If DIR=NULL in function load.diffusion.kernel, the method assumes the kernel matrix to be present in the data directory of GOSim.}
}
\details{
The methods argument has to take on one of the following values:
  \describe{
\item{"diffKernelLapl"}{pseudo inverse of the (unnormalized) graph Laplacian: Takes into account all powers of diffusion and incorporates all paths from one node to another one.}
\item{"diffKernelpower"}{even power of the transition probability matrix: Takes into account local transitions of path length m}
\item{"diffKernelLLE"}{local linear embedding into an Euclidean space: The focus is to preserve local distances to nearest neighbors. The LLE kernel emphasizes short-range interactions between GO terms.}
\item{"diffKernelexpm"}{expm(-mL), where t is a positive constant, L is the (unnormalized) graph Laplacian and expm denotes the matrix exponential. This kernel takes into account all positive integer powers of diffusion, but with an exponential decay of the influence of long-range interactions.}
}
}
\value{
  \code{calc.diffusion.kernel} puts a kernel matrix / similarity matrix named "<method><ontology><organism><evidence levels>.rda" in the defined directoy. It can be used afterwards by calling \code{\link{load.diffusion.kernel}}. 
}
\seealso{\code{\link{load.diffusion.kernel}}}
\references{
Lerman G. & Shaknovich B., Defining Functional Distance using Manifold Embeddings of Gene Ontology Annotations, PNAS, 104(27): 11334 - 11339, 2007
}
\keyword{ file }% at least one, from doc/KEYWORDS
