
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# DATASET:             DESCRIPTION:
#  RS.txt               Monthly 91 day Treasury Bill rate  
#  R20.txt              Monthly Yield on 20 Year UK Gilts  
#  RSQ.txt              Quarterly 91 day Treasury Bill rate  
#  R20Q.txt             Quarterly Yield on 20 Year UK Gilts  
#  RSQREAL.txt          Quarterly real 91 day Treasury Bill rate  
#  FTAPRICE.txt         FTA All Share Price Index  
#  FTADIV.txt           FTA All Share Dividend Index  
#  FTARET.txt           FTA All Share Nominal Returns  
#  RPI.txt              UK Retail Price Index  
#  EXCHD.txt            Dollar/Sterling Exchange Rate  
#  EXCHQ.txt            Dollar/Sterling Exchange Rate  
#  SP500.txt            SP 500 Annual Data Index  
#  SP500R.txt           SP 500 Real Returns  
#  SP500D.txt           SP 500 Daily Data Index  
#  FT30.txt             FT 30 Index  
#  FTSE100.txt          FTSE 100 Index  
#  CTLD.txt             Courtaulds Share Price  
#  LGEN.txt             Legal and General Share Price  
#  PRU.txt              Prudential Share Price.   
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(MillsData); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.MillsData = 
function()
{  
    # Data Sets:
    data(RS);         head(RS)       
    data(R20);        head(R20)      
    data(RSQ);        head(RSQ)      
    data(R20Q);       head(R20Q)     
    data(RSQREAL);    head(RSQREAL)   
    data(FTAPRICE);   head(FTAPRICE) 
    data(FTADIV);     head(FTADIV)   
    data(FTARET);     head(FTARET)   
    data(RPI);        head(RPI)      
    data(EXCHD);      head(EXCHD)    
    data(EXCHQ);      head(EXCHQ)    
    data(SP500);      head(SP500)    
    data(SP500R);     head(SP500R)   
    data(SP500D);     head(SP500D)   
    data(FT30);       head(FT30)     
    data(FTSE100);    head(FTSE100)   
    data(CTLD);       head(CTLD)     
    data(LGEN);       head(LGEN)     
    data(PRU);        head(PRU)      

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fSeries/test/runit036B.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
