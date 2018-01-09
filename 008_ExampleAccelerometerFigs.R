

require(data.table)

fle <- "/Users/cswitzer/Dropbox/SonicationBehavior/SonBehData/ExampleAccTrace.txt"
system.time(DT <- fread(fle))


e2 <- t(DT)
head(e2)
plot(e2, type = 'l', xlab = "Time (s)", ylab = "Differential Voltage (V)", bty = 'n')

spectrum(e2[, 2])


spec.pgram(e2[,2] )

