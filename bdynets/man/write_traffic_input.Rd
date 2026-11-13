\name{write_traffic_input}
\alias{write_traffic_input}
\title{Publish prepared traffic data to a new directory}
\usage{
write_traffic_input(input, output_dir, source_files = character(),
  native = NULL, dictionary = NULL, validation = NULL)
}
\arguments{
\item{input}{A list produced by \code{traffic_input()}. Evaluated after destination validation.}
\item{output_dir}{A new directory. Existing files, directories and source aliases are rejected.}
\item{source_files}{Existing source paths to protect and identify. Directory paths protect their descendants. File contents are identified by SHA-256.}
\item{native}{Optional native source snapshot, saved as \code{source.rds}.}
\item{dictionary}{Optional data frame with \code{table}, \code{field} and \code{meaning}; existing fields only.}
\item{validation}{Optional data frame with \code{check}, \code{observed}, \code{expected}, \code{passed} and \code{detail}. A failed check prevents publication.}
}
\value{Invisibly, the published \code{input.rds} path.}
\description{Writes the authoritative RDS, readable prepared tables, ordered road vertices, metadata, field dictionary, validation results, source identities and a checksum manifest. Source paths are read only. The destination has no overwrite mode.}
\details{Preparation and export finish in a temporary sibling directory before publication. Failure removes the temporary files. A per-destination lock prevents two cooperating preparations from publishing simultaneously. CSV identifiers remain character, double values use 17 significant digits, missing values use the token declared in metadata, and POSIXct timestamps use Unix seconds. RDS retains original types and nested provenance.}
\examples{
nodes <- data.frame(node_id=c("001","002"), longitude=c(1,2), latitude=c(1,2))
od <- data.frame(origin_id="001", destination_id="002", flow=1L)
x <- traffic_input(nodes=nodes, od=od)
out <- tempfile("prepared-")
path <- write_traffic_input(x, out)
stopifnot(identical(readRDS(path)$od, x$od))
unlink(out, recursive=TRUE)
}
