# ===================
# GSH (after 1h)

data_GSH = read.table("data_GSH.txt", sep="\t", header=TRUE)
head(data_GSH)

# plot the data to get a sense of it
plot(data_GSH$KBrO3_mM, data_GSH$pct_control)

# you have plenty of options, the most useful are:
# nicer
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, las=1)
# professional
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, las=1,
     xlab="KBrO3 (mM)", ylab="GSH decrease (%)")
# excellent
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, las=1,
     xlab="KBrO3 (mM)", ylab="GSH decrease (%)", main="My Beautiful Data")
# clever
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, las=1,
     xlab="KBrO3 (mM)", ylab="GSH decrease (%)", main="My Beautiful Data",
     log="y") # you can use log="x" or "y" or "xy" or "" 
# genius level
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, las=1,
     xlab="KBrO3 (mM)", ylab="GSH decrease (%)", main="My Beautiful Data",
     log="y", pch=10, cex= 1.3, col="red") # set the type of mark, size, color

# etc. see help(par) or ?par for all the parameters you can set

# Let's get serious:
# define the Hill dose-response model as a function
Hill = function (x, EC50, n) {
  return(x^n / (x^n + EC50^n))
}

# let's try it
Hill(x=10, EC50=1, n=1)

# plot it...
x = seq(0, 10, 0.1)
y = Hill(x=x, EC50=1, n=3)
plot(x, y, type="b")

# problem: it goes up from zero to one and we want it to go down from 100 to 0
# so we change it a bit

# define an extended Hill dose-response model as a function
Hill = function (x, EC50, n, from, diff) {
  return(from + diff * (x^n / (x^n + EC50^n)))
}

# try it
Hill(x=10, EC50=1, n=1, from=1, diff=10)

# plot it...
x = seq(0, 10, 0.1)
y = Hill(x=x, EC50=1, n=3, from=1, diff=10)
plot(x, y, type="b")

# can you find values of from and diff that would match the data range?
# first: what is the data range?
# assign the values found to variables A and B
A =
B =
        
# now replot the data and overlay with the model
# make sure to define the axes limits before doing overlays...
xlims = c(0, 6) # a vector of min and max
ylims = c(0, 120)
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, xlim=xlims, ylim=ylims, col="red")
par(new=T) # will overlay the next plot
x = seq(0, 6, 0.1)
y = Hill(x=x, EC50=1, n=3, from=A, diff=B)
plot(x, y, type="l", xlim=xlims, ylim=ylims, xlab="", ylab="") # turn off labels

# now play with EC50 and N to fit the data better

# there are many tools in R that can be used to fit automatically your model
# one of them is PROAST (from the RIVM)
# but we can also access PROAST online (more fun)
