\name{bdynets_atomic_write}
\alias{bdynets_atomic_write}
\title{Publish a complete result file}
\usage{bdynets_atomic_write(path, writer)}
\arguments{
\item{path}{Destination file in an existing directory.}
\item{writer}{Function that writes one complete file to the supplied temporary path.}
}
\description{Write in the destination directory, then rename the file into place. A writer error leaves the previous result intact. Callers coordinate simultaneous writers when updates to one file must be serialized.}
\value{The destination path, invisibly.}
