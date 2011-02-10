\name{readSeries}

\title{timeSeries Input}

\alias{readSeries}

\description{
  Reads a file in table format and creates a \code{timeSeries} object
  from it. The first column of the table must hold the
  timestamps. Format of the stimestamps can be either specified in the
  header of the first column or by the \code{format} argument.
}

\usage{
readSeries(file, header = TRUE, sep = ";", zone = "", 
    FinCenter = "",  format, \dots)
}

\arguments{
  \item{file}{
    the filename of a spreadsheet data set from which
    to import the data records.
  }
  \item{FinCenter}{
    a character with the the location of the  financial center named 
    as "continent/city". 
  }
  \item{header}{
    a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the
    value is determined from the file format: 'header' is set to
    'TRUE' if and only if the first row contains one fewer field
    than the number of columns.
  }
  \item{format}{
    a character string with the format in POSIX notation specifying
    the timestamp format. Note the function assumes that the first
    column in the csv file has the timestamps.
  }  
  \item{sep}{
    the field seperator used in the spreadsheet file to separate 
    columns.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }  
  \item{\dots}{
    Additional arguments passed to \code{read.table()} function which is
    used to read the file.
  }
}

\value{
  Return a S4 object of class \code{timeSeries}.
}

