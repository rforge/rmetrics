
xml.rpc =
function(url, method, ..., .args = list(...),
          .opts = list(),
          .defaultOpts = list(httpheader = c('Content-Type' = "text/xml")),
          .convert = TRUE, .curl = getCurlHandle())
{
    # Turn the method and arguments to an RPC body.
  body = createBody(method,  .args)

    # merge the .defaultOpts and the .opts into one list.
  .defaultOpts[["postfields"]] = saveXML(body)
  if(length(.opts))
     .defaultOpts[names(.opts)] = .opts
  
  ans = postForm(url, .opts = .defaultOpts, style = "POST", curl = .curl)

   # Now either convert using the default converter fnction (convertToR)
   # or return as is or allow the caller to specify a function to use for conversion.
  if(is.logical(.convert)) {
    if(.convert)
      convertToR(ans)
    else
      ans
  } else if(is.function(.convert))
          .convert(ans)
  else
      ans
}

createBody =
function(method, args)
{
  top = newXMLNode("methodCall", newXMLNode("methodName", method))
  params = newXMLNode("params", parent = top)
  sapply(args, function(x) newXMLNode("param", rpc.serialize(x), parent = params))
  top
}

setGeneric("rpc.serialize", function(x, ...) standardGeneric("rpc.serialize"))

basicTypeMap =
  c("integer" = "i4",
    "double" = "double",
    "character" = "string",
    "logical" = "boolean",
    "raw" = "base64")

cast <- function(x) {
  if (is.logical(x))
    as.integer(x)
  else
    x
}

setMethod("rpc.serialize", "raw",
           function(x, ...) {
              x = gsub("\\n", "", x)
              val = base64Encode(x)
              newXMLNode("value", newXMLNode("base64", val))
           })

setMethod("rpc.serialize", "vector",
           function(x, ...) {
              type = basicTypeMap[typeof(x)]
              x = cast(x)
              
              if(length(names(x))) {
warning("Skipping named vector!")
              } else {
                if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x))
                else {
                  top = newXMLNode("value")
                  a = newXMLNode("array", parent = top)
                  data = newXMLNode("data", parent = a)
                  sapply(x, function(x) newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(x) else x), parent = data))
                  top
                }
              }
           })


setMethod("rpc.serialize", "list",
           function(x, ...) {
              
              if(length(names(x))) {
                  a = newXMLNode("struct")
                  sapply(names(x), function(id) {
                                     type = basicTypeMap[typeof(x[[id]])]
                                     newXMLNode("member", newXMLNode("name", id),
                                                          newXMLNode("value", rpc.serialize(x[[id]])),
                                                parent = a)
                                   })
                  a
              } else {
                  a = newXMLNode("array")
                  data = newXMLNode("data", parent = a)
                  sapply(x, function(x) {
                               elName = basicTypeMap[typeof(x)]
                               newXMLNode("value", newXMLNode(elName, if(elName == "string") newXMLCDataNode(x) else x,
                                                               parent = data))
                             })
              a
              }
           })


setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
          fault = xmlRPCToR(fault[[1]])
          stop("faultCode: ",  fault$faultCode, " faultString: ", fault$faultString)
    }
    a = xpathApply(node, "//param/value", xmlRPCToR)
    if(length(a) == 1)
      a[[1]]
    else
      a
})

setMethod('convertToR', 'XMLInternalNode',
function(node)
{
   if(length(getNodeSet(node, "./param/value"))) {
     ans = xpathApply(node, "./param/value", xmlRPCToR, simplify = FALSE)
   } else
      xmlToList(node)
})

setMethod('convertToR', 'character',
function(node)
{
  convertToR(xmlParse(node, asText = TRUE))
})

xmlRPCToR =
function(node, ...)
{
  if(is.null(node))
    return(NULL)
  
  if(xmlName(node) == "value")
    node = node[[1]]

  if(is(node, "XMLInternalTextNode"))
    return(xmlValue(node))
  
  type = xmlName(node)
  switch(type,
         'array' = xmlRPCToR.array(node, ...),
         'struct' = xmlRPCToR.struct(node, ...),
         'i4' = as.integer(xmlValue(node)),
         'int' = as.integer(xmlValue(node)),
         'boolean' = if(xmlValue(node) == "1") TRUE else FALSE,                  
         'double' = as.numeric(xmlValue(node)),
         'string' = xmlValue(node),
         'dateTime.iso8601' = as.POSIXct(strptime(xmlValue(node), "%Y%m%dT%H:%M:%S")),
         'base64' = base64(xmlValue(node), encode = FALSE),
         xmlValue(node)
        )

}

xmlRPCToR.struct =
function(node, ...)
{
  ans = xmlApply(node, function(x) xmlRPCToR(x[["value"]][[1]], ...))
  names(ans) = xmlSApply(node, function(x) xmlValue(x[["name"]]))
  ans
}


xmlRPCToR.array =
function(node, ...)
{
  ans = xmlApply(node[["data"]], function(x) xmlRPCToR(x[[1]]))

  if(!is.list(ans[[1]]) && all(sapply(ans, typeof) == typeof(ans[[1]])))
    structure(unlist(ans), names = NULL)
  else
    ans
}
