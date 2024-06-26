% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expected_time.R
\name{event_diff}
\alias{event_diff}
\title{Considering the enrollment rate, failure rate, and randomization ratio,
calculate the difference between the targeted number of events and the
accumulated events at time \code{x}}
\usage{
event_diff(x, enroll_rate, fail_rate, ratio, target_event)
}
\arguments{
\item{x}{Duration}

\item{enroll_rate}{An \code{enroll_rate} data frame with or without stratum
created by \code{\link[=define_enroll_rate]{define_enroll_rate()}}.}

\item{fail_rate}{A \code{fail_rate} data frame with or without stratum
created by \code{\link[=define_fail_rate]{define_fail_rate()}}.}

\item{ratio}{Experimental:Control randomization ratio.}

\item{target_event}{The targeted number of events to be achieved.}
}
\value{
A single numeric value that represents the difference between the
expected number of events for the provided duration (\code{x}) and the targeted
number of events (\code{target_event})
}
\description{
A helper function passed to \code{uniroot()}
}
\keyword{internal}
