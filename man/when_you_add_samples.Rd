% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/when_you_add_samples.R
\name{when_you_add_samples}
\alias{when_you_add_samples}
\title{Calculate When Your LC-MS Analysis Will End}
\usage{
when_you_add_samples(
  sample_count,
  minutes_per_sample,
  sample_detected = 0,
  start_time = NULL
)
}
\arguments{
\item{sample_count}{Numeric. The total number of samples to be analyzed.}

\item{minutes_per_sample}{Numeric. The time needed for each sample analysis in minutes.}

\item{sample_detected}{Numeric. The number of samples already analyzed (default = 0).}

\item{start_time}{Character or NULL. The start time for analysis. Can be in format
"YYYY-MM-DD HH:MM" or "HH:MM". If NULL, current system time will be used.}
}
\value{
A list containing:
\itemize{
\item start_time: POSIXct object of the analysis start time
\item end_time: POSIXct object of the analysis end time
\item total_hours: Numeric value of total hours needed
\item sample_count: Number of total samples
\item sample_detected: Number of samples already analyzed
\item remaining_samples: Number of remaining samples
\item minutes_per_sample: Minutes per sample
}
}
\description{
This function helps you calculate when your LC-MS analysis will finish based on
the number of samples and time per sample. It supports both automatic (current system time)
and manual time input.
}
\examples{
# Using current system time
when_you_add_samples(sample_count = 75, minutes_per_sample = 13)

# Using specific date and time
when_you_add_samples(
  sample_count = 75,
  minutes_per_sample = 13,
  start_time = "2024-01-20 09:00"
)

# Using only time (will use today's date)
when_you_add_samples(
  sample_count = 75,
  minutes_per_sample = 13,
  start_time = "09:00"
)

# With already detected samples and current system time
when_you_add_samples(
  sample_count = 75,
  minutes_per_sample = 13,
  sample_detected = 25
)

# With already detected samples and specific start time
when_you_add_samples(
  sample_count = 75,
  minutes_per_sample = 13,
  sample_detected = 25,
  start_time = "2024-01-20 09:00"
)

}
