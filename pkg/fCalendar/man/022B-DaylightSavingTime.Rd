\name{DaylightSavingTime}

\alias{DaylightSavingTime}

\alias{Adelaide}
\alias{Algiers}
\alias{Amsterdam}
\alias{Anchorage}
\alias{Andorra}
\alias{Athens}
\alias{Auckland}
\alias{Bahrain}
\alias{Bangkok}
\alias{Beirut}
\alias{Belfast}
\alias{Belgrade}
\alias{Berlin}
\alias{Bogota}
\alias{Bratislava}
\alias{Brisbane}
\alias{Brussels}
\alias{Bucharest}
\alias{Budapest}
\alias{BuenosAires}
\alias{Cairo}
\alias{Calcutta}
\alias{Caracas}
\alias{Casablanca}
\alias{Cayman}
\alias{Chicago}
\alias{Copenhagen}
\alias{Darwin}
\alias{Denver}
\alias{Detroit}
\alias{Dubai}
\alias{Dublin}
\alias{Eastern}
\alias{Edmonton}
\alias{Frankfurt}
\alias{Helsinki}
\alias{HongKong}
\alias{Honolulu}
\alias{Indianapolis}
\alias{Istanbul}
\alias{Jakarta}
\alias{Jerusalem}
\alias{Johannesburg}
\alias{Kiev}
\alias{KualaLumpur}
\alias{Kuwait}
\alias{Lagos}
\alias{Lisbon}
\alias{Ljubljana}
\alias{London}
\alias{LosAngeles}
\alias{Luxembourg}
\alias{Madrid}
\alias{Manila}
\alias{Melbourne}
\alias{MexicoCity}
\alias{Monaco}
\alias{Montreal}
\alias{Moscow}
\alias{Nairobi}
\alias{Nassau}
\alias{NewYork}
\alias{Nicosia}
\alias{Oslo}
\alias{Pacific}
\alias{Paris}
\alias{Perth}
\alias{Prague}
\alias{Riga}
\alias{Riyadh}
\alias{Rome}
\alias{Seoul}
\alias{Shanghai}
\alias{Singapore}
\alias{Sofia}
\alias{Stockholm}
\alias{Sydney}
\alias{Taipei}
\alias{Tallinn}
\alias{Tehran}
\alias{Tokyo}
\alias{Tunis}
\alias{Vaduz}
\alias{Vancouver}
\alias{Vienna}
\alias{Vilnius}
\alias{Warsaw}
\alias{Winnipeg}
\alias{Zagreb}
\alias{Zurich}


\title{Daylight Saving Time Rules}


\description{

    A collection and description of functions for about 
    100 cities and regions, which return the daylight 
    saving time tables. 
    \cr
    
    The functions are:
    
    Adelaide Algiers Amsterdam Anchorage Andorra Athens Auckland Bahrain
    Bangkok Beirut Belfast Belgrade Berlin Bogota Bratislava Brisbane
    Brussels Bucharest Budapest BuenosAires Cairo Calcutta Caracas
    Casablanca Cayman Chicago Copenhagen Darwin Denver Detroit Dubai
    Dublin Eastern Edmonton Frankfurt Helsinki HongKong Honolulu
    Indianapolis Istanbul Jakarta Jerusalem Johannesburg Kiev
    KualaLumpur Kuwait Lagos Lisbon Ljubljana London LosAngeles
    Luxembourg Madrid Manila Melbourne MexicoCity Monaco Montreal Moscow
    Nairobi Nassau NewYork Nicosia Oslo Pacific Paris Perth Prague Riga
    Riyadh Rome Seoul Shanghai Singapore Sofia Stockholm Sydney Taipei
    Tallinn Tehran Tokyo Tunis Vaduz Vancouver Vienna Vilnius Warsaw
    Winnipeg Zagreb Zurich.
    
}


\note{

    There are currently two synonymes available "Pacific" for Los Angeles
    and "Eastern" for New York. 
    
    Note that we leave the space in all double named cities like New York 
    or Hong Kong and don't use an underscore for it.
    
    All the entries are retrieved from the Ical library which is 
    available under GNU's GPL licence. 

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fCalendar.023B-DaylightSavingTime")

## data - DST Rules for Zurich:
   Zurich()
}


\keyword{data}
