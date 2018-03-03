# ===================
# We will fist model the in vitro kinetics of KBrO3 in RPTEC cells

# read the data from a file
kinetic.data = read.table("KBrO3_kinetics.txt")

# how big?
dim(kinetic.data)

# small: look at them all
kinetic.data

# plot them on two panels
par(mfrow=c(1,2))
plot(kinetic.data[,1], kinetic.data[,2], las=1, type="b",
     xlab="Time (min)", ylab="KBrO3 (mM)",
     log="", pch=16, cex=1.3, cex.lab=1.2, col="orange")
plot(kinetic.data[,1], kinetic.data[,3], las=1, type="b",
     xlab="Time (min)", ylab="",
     log="", pch=16, cex=1.3, cex.lab=1.2, col="red")

# now we need a model for those data
# they look like decreasing exponential (quite usual)
# It is customary to look at them on a log y-scale 
par(mfrow=c(1,2))
plot(kinetic.data[,1], kinetic.data[,2], las=1, type="b",
     xlab="Time (min)", ylab="KBrO3 (mM)",
     log="y", pch=16, cex=1.3, cex.lab=1.2, col="orange")
plot(kinetic.data[,1], kinetic.data[,3], las=1, type="b",
     xlab="Time (min)", ylab="",
     log="y", pch=16, cex=1.3, cex.lab=1.2, col="red")

# it seems that there is only one line (not two or more lines joined
# with different slopes. In this case a mono-exponential decrease model will
# probably do. That model would be:
# KBrO3 = C0 * exp(-k * time)

# what values should we give to parameters Co and k? Co is obvious
C0 = 0.375

# for k, try different values till you match the low dose data
k = 0

KBrO3 = C0 * exp(-k * kinetic.data[,1])

par(mfrow=c(1,2))
plot(kinetic.data[,1], kinetic.data[,2], las=1, type="b",
     xlab="Time (min)", ylab="KBrO3 (mM)",
     log="y", pch=16, cex=1.3, cex.lab=1.2, col="orange")
lines(kinetic.data[,1], KBrO3, col="blue")

# repeat the above lines, changing k value, till you find a decent fit
# report the value found:
k = ?

# does it work at high dose? (k might depend on the dose...)
# how would you check it?

# ==================================
# OK, so now we should have the KBrO3 kinetics about OK.
# let's reanalyze the KBrO3 - GSH data, taking the kinetics into account

# re-read the GSH data
data_GSH = read.table("data_GSH.txt", sep="\t", header=TRUE)
data_GSH

# replot the data
plot(data_GSH$KBrO3_mM, data_GSH$pct_control)

# remember that we fitted a Hill dose-response model to that data
Hill = function (x, EC50, n, from, diff) {
  return(from + diff * (x^n / (x^n + EC50^n)))
}

# x was the nominal (initial) concentration of KBrO3 in the medium,
# but now we know that KBrO3 disappears progressively (probably by
# reduction to bromide). So, what should be x? The GSH measurements were
# made after 1 hour. Should we take the concentration of KBrO3 at 1 hour, or
# or the average concentration over the 1st hour? <Discussion>

# if we take the concentration at 1h we should use
KBrO3 = C0 * exp(-k * 1 * 60) # 1 hour is 60 minutes...

# if we take the average we should use
KBrO3 = (-C0*exp(-k * 60)/k + C0*exp(0)/k) / (60 - 0)

# does it make a difference? why?

# let's try it
Hill(x=KBrO3, EC50=1, n=1)

# plot it...
# define a sequence of nominal concentrations
C0 = seq(0, 10, 0.1)
# the actual bromate concentration seen by the cells was:
? KBrO3 = C0 * exp(-k * 1 * 60)
? KBrO3 = (-C0*exp(-k * 60)/k + C0*exp(0)/k) / (60 - 0)
y = Hill(x=KBrO3, EC50=10, n=1, from=100, diff=-100)
plot(C0, y, type="b")

# now replot the data and overlay with the model
# make sure to define the axes limits before doing overlays...
xlims = c(0, 6) # a vector of min and max
ylims = c(0, 120)
plot(data_GSH$KBrO3_mM, data_GSH$pct_control, xlim=xlims, ylim=ylims, col="red")
par(new=T) # will overlay the next plot
plot(C0, y, type="l", xlim=xlims, ylim=ylims, xlab="", ylab="") # turn off labels

# now play with EC50 and N to fit the data better
## we have coupled PK and PD (dose-response)
