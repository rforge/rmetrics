
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
# You should have received A copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################


if (FALSE) {
    
    # Settings:
    require(fSeries)
    require(RUnit)
    testIndex = c(
        "1A", # ArmaInterface
        "1B", # ArmaStatistics
        "1C", # ArfimaOxInterface
        "2A", # UnitrootTests
        "2B", # UnitRootUrcaInterface
        "3A", # LrdModelling
        "3B", # LrdStatistics
        "4A", # GarchDistributions
      # "4C", # GarchModelling 
        "4D", # GarchOxInterface
        "5A", # NonLinModelling
        "5B", # NonLinStatistics
        "5C") # NonLinTests
    File = "C:/Rmetrics/SVN/trunk/fSeries/tests/runit"
    Protocol = "runitfSeries.txt"
    
    # Perform and Save all Unit Tests:
    write("fSeries:", file = Protocol)
    for (Index in testIndex) {
        file = paste(File, Index, ".R", sep = "")
        write("", file = Protocol, append = TRUE)
        testResult <- runTestFile(file)
        textProtocol = capture.output(printTextProtocol(testResult))
        write(textProtocol[-c(2, 6:14, 18:20)], 
            file = Protocol, append = TRUE)
    } 
     
    # Show Protocol:
    TXT = scan(Protocol, character(), blank.lines.skip = FALSE, sep = "\n")
    cat(TXT, sep = "\n")
    
}


# ------------------------------------------------------------------------------

