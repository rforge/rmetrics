\name{readSeries}


\title{Reads a 'timeSeries' from a File}


\alias{readSeries}


\description{

    Reads a file in table format and creates a \code{timeSeries} 
    object from it. 
    
}


\details{

    The first column of the table must hold the timestamps. Format 
    of the stimestamps can be either specified in the header of the 
    first column or by the \code{format} argument.
    
}

\usage{
readSeries(file, header = TRUE, sep = ";", zone = "", 
    FinCenter = "", format, \dots)
}


\arguments{

    \item{file}{
        the filename of a spreadsheet data set from which to import 
        the data records.
        }
    \item{FinCenter}{
        a character with the the location of the  financial center 
        named as "continent/city". 
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
        the timestamp format. Note, the format has not to be specified
        if the first column in the file has the timestamp format
        specifyer, e.g. "\%Y-\%m-\%d" for the short ISO 8601 format. 
        }  
    \item{sep}{
        the field seperator used in the spreadsheet file to separate 
        columns. By default ";". Note, if \code{sep=";"} is specified, 
        and reading the series fails, then the reading is automatically 
        repeated with \code{sep=","}.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        By default \code{zone=""} which is short for GMT.
        }  
    \item{\dots}{
        Additional arguments passed to \code{read.table()} function which 
        is used to read the file.
        }
}


\value{

  Returns a S4 object of class \code{timeSeries}.
  
}


