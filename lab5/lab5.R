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

###
clock
threads
cores
TDP
transistors
dieSize
voltage
featureSize
channel
FO4delay
L1icache
L1dcache
L2cache
L3cache
###

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

# starting lab 5

# PART 1

x <- list(.1,.2,.3,.4,.5,.6,.7,.8,.9)

count <- 1

for (val in x) {

deltaConcat <- 0
  
for (y in 1:100){

rows <- nrow(int95.dat) 
f <- count*0.1
upper_bound <- floor(f * rows)
permuted_int95.dat <- int95.dat[sample(rows) , ]
train.dat <- permuted_int95.dat[1:upper_bound, ]
test.dat <- permuted_int95.dat[(upper_bound+1): rows, ]

int95_new.lm <- lm(int95.dat$nperf ~ int95.dat$clock + 
                    								 int95.dat$cores, 
                   data = train.dat)
predicted.dat <- predict(int95_new.lm, newdata=test.dat)
delta <- predicted.dat - test.dat$nperf

deltaConcat <- c(deltaConcat, delta)

}

# Find the mean and confidence interval for this vector of example data.
val = t.test(deltaConcat, conf.level=0.95)
# Extract the mean of this vector from the confidence interval calculation
y = val$estimate
# Show the two confidence intervals
val$conf.int
# Extract the lower bound of the confidence interval
ylow = val$conf.int[1]
# Extract the upper bound of the confidence interval
yhigh = val$conf.int[2]

if(count==1){
  
  int95 <- plot(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
  title(main="int95")
  
}

points(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
arrows(f, ylow, f, yhigh, length=0.05, angle=90, code=3)

count <- count + 1

}

# int06

count <- 1

for (val in x) {
  
  deltaConcat <- 0
  
  for (y in 1:100){
    
    rows <- nrow(int06.dat) 
    f <- count*0.1
    upper_bound <- floor(f * rows)
    permuted_int06.dat <- int06.dat[sample(rows) , ]
    train.dat <- permuted_int06.dat[1:upper_bound, ]
    test.dat <- permuted_int06.dat[(upper_bound+1): rows, ]
    
    int06_new.lm <- lm(int06.dat$nperf ~ int06.dat$clock + 
                         								 int06.dat$threads +
                         								 int06.dat$cores +
                         								 int06.dat$TDP +
                         								 int06.dat$channel +
                         								 int06.dat$FO4delay +
                         								 int06.dat$L1icache +
                         								 int06.dat$L2cache,
                       data = train.dat)
    predicted.dat <- predict(int06_new.lm, newdata=test.dat)
    delta <- predicted.dat - test.dat$nperf
    
    deltaConcat <- c(deltaConcat, delta)
    
  }
  
  # Find the mean and confidence interval for this vector of example data.
  val = t.test(deltaConcat, conf.level=0.95)
  # Extract the mean of this vector from the confidence interval calculation
  y = val$estimate
  # Show the two confidence intervals
  val$conf.int
  # Extract the lower bound of the confidence interval
  ylow = val$conf.int[1]
  # Extract the upper bound of the confidence interval
  yhigh = val$conf.int[2]
  
  if(count==1){
    
    int06 <- plot(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
    title(main="int06")
    
  }
  
  points(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
  arrows(f, ylow, f, yhigh, length=0.05, angle=90, code=3)
  
  count <- count + 1
  
}

# fp95

count <- 1

for (val in x) {
  
  deltaConcat <- 0
  
  for (y in 1:100){
    
    rows <- nrow(fp95.dat) 
    f <- count*0.1
    upper_bound <- floor(f * rows)
    permuted_fp95.dat <- fp95.dat[sample(rows) , ]
    train.dat <- permuted_fp95.dat[1:upper_bound, ]
    test.dat <- permuted_fp95.dat[(upper_bound+1): rows, ]
    
    fp95_new.lm <- lm(fp95.dat$nperf ~ fp95.dat$clock + 
                        fp95.dat$cores, 
                      data = train.dat)
    predicted.dat <- predict(fp95_new.lm, newdata=test.dat)
    delta <- predicted.dat - test.dat$nperf
    
    deltaConcat <- c(deltaConcat, delta)
    
  }
  
  # Find the mean and confidence interval for this vector of example data.
  val = t.test(deltaConcat, conf.level=0.95)
  # Extract the mean of this vector from the confidence interval calculation
  y = val$estimate
  # Show the two confidence intervals
  val$conf.int
  # Extract the lower bound of the confidence interval
  ylow = val$conf.int[1]
  # Extract the upper bound of the confidence interval
  yhigh = val$conf.int[2]
  
  if(count==1){
    
    fp95 <- plot(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
    title(main="fp95")
    
  }
  
  points(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
  arrows(f, ylow, f, yhigh, length=0.05, angle=90, code=3)
  
  count <- count + 1
  
}

# fp06

count <- 1

for (val in x) {
  
  deltaConcat <- 0
  
  for (y in 1:100){
    
    rows <- nrow(fp06.dat) 
    f <- count*0.1
    upper_bound <- floor(f * rows)
    permuted_fp06.dat <- fp06.dat[sample(rows) , ]
    train.dat <- permuted_fp06.dat[1:upper_bound, ]
    test.dat <- permuted_fp06.dat[(upper_bound+1): rows, ]
    
    fp06_new.lm <- lm(nperf ~ clock + cores, data = train.dat)
    predicted.dat <- predict(fp06_new.lm, newdata=test.dat)
    delta <- predicted.dat - test.dat$nperf
    
    deltaConcat <- c(deltaConcat, delta)
    
  }
  
  # Find the mean and confidence interval for this vector of example data.
  val = t.test(deltaConcat, conf.level=0.95)
  # Extract the mean of this vector from the confidence interval calculation
  y = val$estimate
  # Show the two confidence intervals
  val$conf.int
  # Extract the lower bound of the confidence interval
  ylow = val$conf.int[1]
  # Extract the upper bound of the confidence interval
  yhigh = val$conf.int[2]
  
  if(count==1){
    
    fp06 <- plot(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
    title(main="fp06")
    
  }
  
  points(f, y, xlim = range(c(0, 1)), ylim = range(c(-1, 1)))
  arrows(f, ylow, f, yhigh, length=0.05, angle=90, code=3)
  
  count <- count + 1
  
}

# PART 2

#int95 into fp95

count <- 1

  deltaConcat <- 0
  
  for (y in 1:100){
    
    int95_new.lm <- lm(nperf ~ clock + cores, data = int95.dat)
    predicted.dat <- predict(int95_new.lm, newdata=fp95.dat)
    delta <- predicted.dat - test.dat$nperf
    
    deltaConcat <- c(deltaConcat, delta)
    
  }
  
  # Find the mean and confidence interval for this vector of example data.
  val = t.test(deltaConcat, conf.level=0.95)
  # Extract the mean of this vector from the confidence interval calculation
  y = val$estimate
  # Show the two confidence intervals
  val$conf.int
  # Extract the lower bound of the confidence interval
  ylow = val$conf.int[1]
  # Extract the upper bound of the confidence interval
  yhigh = val$conf.int[2]
  
  print("LOL")
  
  print(val)
  

#int06 into fp06
  
  deltaConcat <- 0
  
  for (y in 1:100){
    
    int06_new.lm <- lm(int06.dat$nperf ~ int06.dat$clock + 
                         								 int06.dat$threads +
                         								 int06.dat$cores +
                         								 int06.dat$TDP +
                         								 int06.dat$channel +
                         								 int06.dat$FO4delay +
                         								 int06.dat$L1icache +
                         								 int06.dat$L2cache,
                       data = int06.dat)
    predicted.dat <- predict(int06_new.lm, newdata=fp06.dat)
    delta <- predicted.dat - test.dat$nperf
    
    deltaConcat <- c(deltaConcat, delta)
    
  }
  
  # Find the mean and confidence interval for this vector of example data.
  val = t.test(deltaConcat, conf.level=0.95)
  # Extract the mean of this vector from the confidence interval calculation
  y = val$estimate
  # Show the two confidence intervals
  val$conf.int
  # Extract the lower bound of the confidence interval
  ylow = val$conf.int[1]
  # Extract the upper bound of the confidence interval
  yhigh = val$conf.int[2]
  
  print("LOL")
  
  print(val)
  


