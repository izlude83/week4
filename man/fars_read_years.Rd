% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{fars_read_years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A list of year values.}
}
\value{
This function returns a list of data frames each containing 2 columns MONTH and
year, or NULL (if an error is encountered trying to read the file of the corresponding
year.)
}
\description{
This function takes in an argument "years" containing a list of year values, then
creates a vector file containing the names of the accident files for these year values.
It then calls on the fars_read function to read each of these files into a data frame,
create a new column called year using the mutate function from dplyr which would
be populated with the appropriate year value, and return the month and year columns
from each of these data frames as a list object. If there is no file corresponding to
any of the year values, a NULL is returned instead of a data frame comprising the
month and year columns.
}
\examples{
dat_list <- fars_read_years(c(2013, 2014))

}
