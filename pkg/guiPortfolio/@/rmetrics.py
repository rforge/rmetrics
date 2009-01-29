# -*- coding: utf-8 -*-
 #!/usr/bin/env python


#import r
import rpy2.robjects as robjects
import pygtk
pygtk.require( '2.0')
import gtk, gtk.glade

#import os
import os

# Define the main window

class MainWindow:

#---------------------------------------------------------------------------------#
#--- init gui --------------------------------------------------------------------#
#---------------------------------------------------------------------------------#

	def __init__(self):

		global r
		r = robjects.r
 
		r("source('rmetrics.R',echo=T)")

	def main(self):
		gtk.main()

if __name__ == "__main__":
	base = MainWindow()
	base.main()


