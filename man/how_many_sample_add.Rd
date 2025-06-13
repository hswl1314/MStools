% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/how_many_sample_add.R
\name{how_many_sample_add}
\alias{how_many_sample_add}
\title{Calculate Maximum Number of Samples That Can Be Added}
\usage{
how_many_sample_add(end_time, minutes_per_sample, start_time = NULL)
}
\arguments{
\item{end_time}{Character. The deadline time in either:
- "HH:MM" format (e.g., "17:00", will use system date)
- "YYYY-MM-DD HH:MM" format (e.g., "2024-01-21 17:00")}

\item{minutes_per_sample}{Numeric. The time needed for each sample analysis in minutes.}

\item{start_time}{Character, POSIXct, or NULL. The start time in either:
- "HH:MM" format (e.g., "09:00", will use system date)
- "YYYY-MM-DD HH:MM" format (e.g., "2024-01-20 09:00")
- Sys.time() for current system time
- NULL (will use current system time)}
}
\value{
A list containing:
\itemize{
\item start_time: POSIXct object of the analysis start time
\item end_time: POSIXct object of the deadline
\item available_hours: Numeric value of available hours
\item max_samples: Integer number of maximum samples possible
\item minutes_per_sample: Minutes per sample
}
}
\description{
This function calculates how many samples can be added before a deadline based on
the time needed per sample. It supports flexible time input formats and automatically
handles date calculations.
}
\examples{
# Example 1: Using current system time as start time
how_many_sample_add(
  end_time = "17:00",
  minutes_per_sample = 13
)

# Example 2: Using time-only format (will use system date)
how_many_sample_add(
  end_time = "17:00",
  minutes_per_sample = 13,
  start_time = "09:00"
)

# Example 3: Using full datetime format
how_many_sample_add(
  end_time = "2024-01-21 17:00",
  minutes_per_sample = 13,
  start_time = "2024-01-20 09:00"
)

# Example 4: Using system time explicitly
how_many_sample_add(
  end_time = "17:00",
  minutes_per_sample = 13,
  start_time = Sys.time()
)

}
