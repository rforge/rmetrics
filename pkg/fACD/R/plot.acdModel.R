`plot.acdModel` <-
function(modelOut)
{
    plot(modelOut@x,xlab='Event',ylab='Duration',type='l')
    lines(modelOut@condDur,col='red')
    title('Duration and Conditional Duration')  


}

