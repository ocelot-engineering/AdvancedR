
# 2.  Names and values ----------------------------------------------------

source("set_up.R", local = TRUE)


# Binding basics ----------------------------------------------------------

# 1. create and object of c(1,2,3)
# 2. bind it to the variable x
x <- c(1, 2, 3)
# Name is a reference to a value

# binds the value of x to y (does not copy, just creates another reference)
y <- x
obj_addr(x)
obj_addr(y)
obj_addr(x) == obj_addr(y)

?Reserved # shows all reserved terms in R

# Exercises

# all point to the same underlying function object
obj_addr(mean)
obj_addr(base::mean)
obj_addr(get("mean"))
obj_addr(evalq(mean))
obj_addr(match.fun("mean"))

?make.names


# Copy on modify ----------------------------------------------------------

# x and y reference the same object
x <- c(1, 2, 3)
y <- x

# the object y is referencing changes, so R created a new object instead of 
# modifying the original. Hence x and y now have a different memory address.
y[[3]] <- 4
x
obj_addr(x)
obj_addr(y)

# R objects are immutable.

# The location where a reference points to can be traced using base::tracemem)
x <- c(1, 2, 3)
cat(tracemem(x), "\n")
# <0x7fcbda78eaa8> 

y <- x
y[[3]] <- 4L
# prints that memory has changed: tracemem[0x7fcbda78eaa8 -> 0x7f80c4427f40]: 

y[[3]] <- 5L
untracemem(x)

# Same rules apply for functions
f <- function(a) {
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")
#> <0x7fe1121693a8>

z <- f(x)
# there's no copy here! z points to the same value as x because there was no modification

untracemem(x)


# Lists

l1 <- list(1, 2, 3)
l2 <- l1
l2[[3]] <- 4
ref(l1, l2)
# █ [1:0x7fcbdb0a7e28] <list> 
# ├─[2:0x7fcbdd130980] <double> 
# ├─[3:0x7fcbdd130948] <double> 
# └─[4:0x7fcbdd130910] <double> 
#   
# █ [5:0x7fcbdafe96a8] <list>  # l2 ref has different address to l1 ref 
# ├─[2:0x7fcbdd130980] 
# ├─[3:0x7fcbdd130948] 
# └─[6:0x7fcbdd1e8ce0] <double> 

# Can see that the l2 does a shallow copy of l1 (i.e. contents references the 
#  same values). But then when one element is modified it creates a new object
#  and references that location. But we can see that the first 2 references
#  in l2 are the same as l1.


# Data frames

# Data frames are lists of vectors
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d2 <- d1
d2[, 2] <- d2[, 2] * 2
ref(d1, d2) # d1 and d2 column 1 point to the same location

# if you modify a column, that column is the only thing that needs to change.
# but if you modify a row, every column needs to change

d3 <- d1
d3[1, ] <- d3[1, ] * 3
ref(d1, d3) # all column points to different locations

# Character vectors
x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)
# R uses a global string pool, which stores all unique strings. 
# A character vector contains pointers to the unique strings in the pool.

# Exercises
# 1.
tracemem(1:10)

# 2.
x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4.0
untracemem(x)
# copies twice since the type of the vector changes from int to float, then updates
#  the ref

# 3.
a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)
obj_addr(a)
ref(b)
ref(c)

# 4.
x <- list(1:10)
ref(x)
x[[2]] <- x
ref(x)
# x points to a list containing a vector. Then x has a second element added which
#  is a reference to itself so the x reference is copied to a new location
#  and the 2nd element points to the original list


# Object size -------------------------------------------------------------

obj_size(letters)
#> 1,712 B

x <- runif(1e6)
as.numeric(obj_size(x))
#> 8,000,048 B

y <- list(x, x, x)
as.numeric(obj_size(y))
#> 8,000,128 B

obj_size(list(NULL, NULL, NULL))
#> 80 B


banana <- "bananas bananas bananas"
obj_size(banana)
#> 136 B
obj_size(rep(banana, 100))
#> 928 B
# keeps referencing the same string from the global string pool, which is why
#  it's not 100 times larger

obj_size(1:3)
#> 680 B
obj_size(1:1e3)
#> 680 B
obj_size(1:1e6)
#> 680 B
obj_size(1:1e9)
#> 680 B
# all the same size because R is just storing the first and last numbers for :

# Exercises

# 1. 
y <- rep(list(runif(1e4)), 100) # 100 lists of 10,000 elements

format(object.size(y), units = "b")
#> 8,005,648 bytes
obj_size(y)
#> 80,896 B

as.numeric(obj_size(runif(1e4)) * 100)
obj_size(rep(list(NULL), 100))

# object.size looks to estimate the memory usage by assuming that the elements
#  are not repeated references but instead completely different values in memory.

# 2.
funs <- list(mean, sd, var)
obj_size(funs)
#> 17,608 B
ref(funs)
obj_size(mean, sd, var, list(NULL, NULL, NULL))

# 3.
a <- runif(1e6)
obj_size(a) # 8mb

b <- list(a, a)
obj_size(b) # 8mb
obj_size(a, b) # still 8mb given the references point to the same memory locations

b[[1]][[1]] <- 10
obj_size(b) # 16mb
obj_size(a, b) # 16mb

b[[2]][[1]] <- 10
obj_size(b) # 16mb
obj_size(a, b) # 24mb


# Modify in place ---------------------------------------------------------

# R will usually always copy on modification. But there are two exceptions to this. 
#   1. If an object has a single name bound to it, R will modify in place
#   Note there are complications of when this occurs. Refer to text book for 
#   deeper details about how bindings are tracked.
#   2. Environments are always modified in place

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

cat(tracemem(x), "\n")

obj_addr(x)
ref(x) # why does ref copy it?
obj_addr(x)
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}
untracemem(x)
# many copies


y <- as.list(x)
cat(tracemem(y), "\n")
#> <0x7f80c5c3de20>

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}
#> tracemem[0x7f80c5c3de20 -> 0x7f80c48de210]: 
# Modifying a list uses internal C code, so the references are not 
#  incremented and only a single copy is made

# Environments are always modified in place
# When you modify an environment all existing bindings continue to have the 
#  same references.
e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1

e1$c <- 4
e2$c 
#> [1] 4
# e2 is also modified

# environments can contain themselves
e <- rlang::env()
e$self <- e

ref(e)
#> █ [1:0x7fe114214cd8] <env> 
#> └─self = [1:0x7fe114214cd8]

# Exercises
x <- list()
tracemem(x)
x[[1]] <- x # note the change of address for x
ref(x)

tracemem(e1) # expect error: not useful for modify in place objects


# Unbinding and the garbage collector -------------------------------------

x <- 1:3
x <- 2:3
rm(x)

# garbage collector deletes unused objects that are no longer used, like those 
#  formally reference to by x (see above).
#  This frees up memory.

# very hard to predict when gc will run, but this command will print to the 
# console every time it does
gcinfo(TRUE) 

# there is no need to ever call gc yourself. R will do it when it needs it.
#  one exception to this is to call gc to return memory to the operating system.

mem_used() # prints memory used
# This number wont be accurate for 3 reasons
#  1. R and the OS are lazy and only reclaim memory when they need it
#  2. R counts memory occupied by objects but there may be gaps, this is called
#     memory fragmentation
#  3. Includes objects created by R but not the R interpreter.
