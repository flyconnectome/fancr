% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ids.R
\name{fanc_xyz2id}
\alias{fanc_xyz2id}
\title{Convert xyz locations to root or supervoxel ids}
\usage{
fanc_xyz2id(
  xyz,
  rawcoords = FALSE,
  voxdims = c(4.3, 4.3, 45),
  root = TRUE,
  ...
)
}
\arguments{
\item{xyz}{One or more xyz locations as an Nx3 matrix or in any form
compatible with \code{\link[fafbseg]{xyzmatrix}} including \code{neuron} or
\code{mesh3d} surface objects.}

\item{rawcoords}{whether the input values are raw voxel indices or in nm}

\item{voxdims}{The voxel dimensions (in nm). See details.}

\item{root}{Whether to return the root id of the whole segment rather than
the supervoxel id.}

\item{...}{additional arguments passed to \code{pbapply} when looking up
multiple positions.}
}
\value{
A character vector of segment ids, NA when lookup fails.
}
\description{
Convert xyz locations to root or supervoxel ids
}
\details{
This used to be very slow because we do not have a supervoxel
field on spine.

I am somewhat puzzled by the voxel dimensions for FANC. Neuroglancer
clearly shows voxel coordinates of 4.3x4.3x45. But in this function, the
voxel coordinates must be set to 4.25 in x-y to give the correct answers.
}
\examples{
# a point from neuroglancer, should map to 648518346498932033
fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE)
}
\seealso{
\code{\link{flywire_xyz2id}}

Other fanc-ids: 
\code{\link{fanc_cellid_from_segid}()},
\code{\link{fanc_ids}()},
\code{\link{fanc_islatest}()},
\code{\link{fanc_latestid}()},
\code{\link{fanc_leaves}()},
\code{\link{fanc_rootid}()}
}
\concept{fanc-ids}
