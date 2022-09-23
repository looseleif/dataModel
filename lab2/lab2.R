# Before running these functions, make sure the file "all-data.csv" is
# in the local directory.
# Also, within the R environment, change the working directory to the directory
# that contains the data file using the toolbar menu:
# File -> Change dir
#

# Read the data from the csv file.
processors <- read.csv("all-data.csv")


################################################################
#
# This function returns the data from the desired column.
# Example:  clock<-get_column("Fp2000","Processor.Clock..MHz.")

get_column <- function(x,y) {

# x = string with the name of the desired benchmark
# y = desired column
#
# Find the indices of all rows that have an entry for the  
# indicated benchmark
benchmark <- paste(paste("Spec",x,sep=""),"..average.base.",
	sep="")
ix <- !is.na(processors[,benchmark])
return(processors[ix,y])
}
################################################################




################################################################
# This function extracts the interesting data columns for the given benchmark
# program and returns a dataframe with these columns.

extract_data <- function(benchmark) {

temp <- paste(paste("Spec",benchmark,sep=""),"..average.base.", sep="")

# perf = the performance reported in the database
perf <- get_column(benchmark,temp)

#nperf = performance normalized to the overall range
max_perf <- max(perf)
min_perf <- min(perf)
range <- max_perf - min_perf
nperf <- 100 * (perf - min_perf) / range

clock <- get_column(benchmark,"Processor.Clock..MHz.")
threads <- get_column(benchmark,"Threads.core")
cores <- get_column(benchmark,"Cores")
TDP <- get_column(benchmark,"TDP")
transistors <- get_column(benchmark,"Transistors..millions.")
dieSize <- get_column(benchmark,"Die.size..mm.2.")
voltage <- get_column(benchmark,"Voltage..low.")
featureSize <- get_column(benchmark,"Feature.Size..microns.")
channel <- get_column(benchmark,"Channel.length..microns.")
FO4delay <- get_column(benchmark,"FO4.Delay..ps.")
L1icache <- get_column(benchmark,"L1..instruction...on.chip.")
L1dcache <- get_column(benchmark,"L1..data...on.chip.")
L2cache <- get_column(benchmark,"L2..on.chip.")
L3cache <- get_column(benchmark,"L3..on.chip.")

return(data.frame(nperf,perf,clock,threads,cores,TDP,transistors,dieSize,voltage,featureSize,channel,FO4delay,L1icache,L1dcache,L2cache,L3cache))

}
################################################################


# Extract a new data frame for each of the benchmark programs available in the data set.

int92.dat <- extract_data("Int1992")
fp92.dat <- extract_data("Fp1992")
int95.dat <- extract_data("Int1995")
fp95.dat <- extract_data("Fp1995")
int00.dat <- extract_data("Int2000")
fp00.dat <- extract_data("Fp2000")
int06.dat <- extract_data("Int2006")
fp06.dat <- extract_data("Fp2006")

# TESTING INTEL92 ENVIRONMENT
print("### ### ###")
print("### INTEL92")
print("### ### ###")


head(int92.dat$clock)
table(int92.dat$clock)
summary(int92.dat$clock)
length(int92.dat$clock)
sum(is.na(int92.dat$clock))
print('---')

head(int92.dat$voltage)
table(int92.dat$voltage)
summary(int92.dat$voltage)
length(int92.dat$voltage)
sum(is.na(int92.dat$voltage))
print('---')

head(int92.dat$L1icache)
table(int92.dat$L1icache)
summary(int92.dat$L1icache)
length(int92.dat$L1icache)
sum(is.na(int92.dat$L1icache))
print('---')

int92clockMean <- mean(int92.dat$clock, na.rm=TRUE)
int92voltageMean <- mean(int92.dat$voltage, na.rm=TRUE)
int92L1icacheMean <- mean(int92.dat$L1icache, na.rm=TRUE)

print('MEAN')
print(int92clockMean)
print(int92voltageMean)
print(int92L1icacheMean)

# --- --- --- --- ---

int92clockSD <- sd(int92.dat$clock, na.rm=TRUE)
int92voltageSD <- sd(int92.dat$voltage, na.rm=TRUE)
int92L1icacheSD <- sd(int92.dat$L1icache, na.rm=TRUE)

print('SD')
print(int92clockSD)
print(int92voltageSD)
print(int92L1icacheSD)

# --- --- --- --- ---

int92clockMin <- min(int92.dat$clock, na.rm=TRUE)
int92voltageMin <- min(int92.dat$voltage, na.rm=TRUE)
int92L1icacheMin <- min(int92.dat$L1icache, na.rm=TRUE)

print('MIN')
print(int92clockMin)
print(int92voltageMin)
print(int92L1icacheMin)

# --- --- --- --- ---

int92clockMax <- max(int92.dat$clock, na.rm=TRUE)
int92voltageMax <- max(int92.dat$voltage, na.rm=TRUE)
int92L1icacheMax <- max(int92.dat$L1icache, na.rm=TRUE)

print('MAX')
print(int92clockMax)
print(int92voltageMax)
print(int92L1icacheMax)

# TESTING INTEL00 ENVIRONMENT
print("### ### ###")
print("### INTEL00")
print("### ### ###")

head(int00.dat$clock)
table(int00.dat$clock)
summary(int00.dat$clock)
length(int00.dat$clock)
sum(is.na(int00.dat$clock))
print('---')

head(int00.dat$voltage)
table(int00.dat$voltage)
summary(int00.dat$voltage)
length(int00.dat$voltage)
sum(is.na(int00.dat$voltage))
print('---')

head(int00.dat$L1icache)
table(int00.dat$L1icache)
summary(int00.dat$L1icache)
length(int00.dat$L1icache)
sum(is.na(int00.dat$L1icache))
print('---')

int00clockMean <- mean(int00.dat$clock, na.rm=TRUE)
int00voltageMean <- mean(int00.dat$voltage, na.rm=TRUE)
int00L1icacheMean <- mean(int00.dat$L1icache, na.rm=TRUE)

print('MEAN')
print(int00clockMean)
print(int00voltageMean)
print(int00L1icacheMean)

# --- --- --- --- ---

int00clockSD <- sd(int00.dat$clock, na.rm=TRUE)
int00voltageSD <- sd(int00.dat$voltage, na.rm=TRUE)
int00L1icacheSD <- sd(int00.dat$L1icache, na.rm=TRUE)

print('SD')
print(int00clockSD)
print(int00voltageSD)
print(int00L1icacheSD)

# --- --- --- --- ---

int00clockMin <- min(int00.dat$clock, na.rm=TRUE)
int00voltageMin <- min(int00.dat$voltage, na.rm=TRUE)
int00L1icacheMin <- min(int00.dat$L1icache, na.rm=TRUE)

print('MIN')
print(int00clockMin)
print(int00voltageMin)
print(int00L1icacheMin)

# --- --- --- --- ---

int00clockMax <- max(int00.dat$clock, na.rm=TRUE)
int00voltageMax <- max(int00.dat$voltage, na.rm=TRUE)
int00L1icacheMax <- max(int00.dat$L1icache, na.rm=TRUE)

print('MAX')
print(int00clockMax)
print(int00voltageMax)
print(int00L1icacheMax)

# TESTING INTEL00 ENVIRONMENT
print("### ### ###")
print("### INTEL06")
print("### ### ###")

head(int06.dat$clock)
table(int06.dat$clock)
summary(int06.dat$clock)
length(int06.dat$clock)
sum(is.na(int06.dat$clock))
print('---')

head(int06.dat$voltage)
table(int06.dat$voltage)
summary(int06.dat$voltage)
length(int06.dat$voltage)
sum(is.na(int06.dat$voltage))
print('---')

head(int06.dat$L1icache)
table(int06.dat$L1icache)
summary(int06.dat$L1icache)
length(int06.dat$L1icache)
sum(is.na(int06.dat$L1icache))
print('---')

int06clockMean <- mean(int06.dat$clock, na.rm=TRUE)
int06voltageMean <- mean(int06.dat$voltage, na.rm=TRUE)
int06L1icacheMean <- mean(int06.dat$L1icache, na.rm=TRUE)

print('MEAN')
print(int06clockMean)
print(int06voltageMean)
print(int06L1icacheMean)

# --- --- --- --- ---

int06clockSD <- sd(int06.dat$clock, na.rm=TRUE)
int06voltageSD <- sd(int06.dat$voltage, na.rm=TRUE)
int06L1icacheSD <- sd(int06.dat$L1icache, na.rm=TRUE)

print('SD')
print(int06clockSD)
print(int06voltageSD)
print(int06L1icacheSD)

# --- --- --- --- ---

int06clockMin <- min(int06.dat$clock, na.rm=TRUE)
int06voltageMin <- min(int06.dat$voltage, na.rm=TRUE)
int06L1icacheMin <- min(int06.dat$L1icache, na.rm=TRUE)

print('MIN')
print(int06clockMin)
print(int06voltageMin)
print(int06L1icacheMin)

# --- --- --- --- ---

int06clockMax <- max(int06.dat$clock, na.rm=TRUE)
int06voltageMax <- max(int06.dat$voltage, na.rm=TRUE)
int06L1icacheMax <- max(int06.dat$L1icache, na.rm=TRUE)

print('MAX')
print(int06clockMax)
print(int06voltageMax)
print(int06L1icacheMax)





