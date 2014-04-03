library(XMLRPC)
library(XML)

doc = xmlParse('xmlrpcTypes.xml')
top = xmlRoot(doc)

XMLRPC:::xmlRPCToR(top[["array"]])
XMLRPC:::xmlRPCToR(top[["struct"]])

XMLRPC:::xmlRPCToR(top[["base64"]])

xmlApply(top, XMLRPC:::xmlRPCToR)


##############

# Round trip the data.
x = XMLRPC:::rpc.serialize(1:10)
XMLRPC:::xmlRPCToR(x)


