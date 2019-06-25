
library(ggplot2)

#generalized model with parameters K,B,M
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}


inverse = function(params, y) {
  -(1/params[2])*log((params[1]-y)/(y*exp(params[2] *params[3]) ))
}

# x values are concentration values and y values are mortality rates
# for instance for concentration 3mg/L there is 81.82% mortality of 
#zebrafish embryos

xseq=seq(0,15,by=0.1) # x axis
x=c(0,3,5,10,15)
y=c(0,0.8182,0.8868,1,1)


# fitting model
fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=.5,c=1))

# extracting coefficients
params=coef(fitmodel)

#Values that are resulting from sigmoid
predicted <- sigmoid(params,xseq)

# I insert my data into dataframes so that I can easily make the plot
df1=data.frame(x,y)   # df1 contains the experinment's datapoints 
df2=data.frame(xseq,predicted)  # df2 contains sigmoid's data  

#I now plot the data
ggplot(data = df2, aes(x = xseq, y = predicted)) +geom_line(aes(col=I("blue")))+
  geom_point(data=df1, aes(x=x,y=y))

# for a more detailed plot...
#ggplot(data = df2, aes(x = xseq, y = predicted)) +geom_line(aes(col=I("blue")))
#+ geom_point(data=df1, aes(x=x,y=y)) + scale_x_continuous(breaks =
#round(seq(min(df2$xseq), max(df2$xseq), by = 0.5),1)) + scale_y_continuous
#(breaks = round(seq(min(df2$predicted), max(df2$predicted), by = 0.5),1))

#inverse at 0.5 - To find the LC50
lethal.dose.athalf=inverse(params,0.5)
