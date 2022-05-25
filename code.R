#1.
twosum <- function(nums, target){
  n <- length(nums)
  for(i in 1:n){
    for (j in (i+1):n){
      if (nums[i]+nums[j]==target){
        return(c(i-1, j-1)) # use return to break out of every loop in the function
      }
    }
  }
}
twosum(c(2, 7, 11, 15), 9)
twosum(c(3, 2, 4), 6)
twosum(c(3, 3), 6)


#2.
addtwonumbers1 <- function(l1, l2){
  n1 <- length(l1)
  n2 <- length(l2)
  x1 <- 0
  for(i in 1:n1){
    x <- l1[i]*10^(i-1)
    x1 <- x1+x
  }
  x2 <- 0
  for(j in 1:n2){
    y <- l2[j]*10^(j-1)
    x2 <- x2+y
  }
  sum <- x1+x2
  output <- rep(NA, nchar(sum))
  for(e in 1:nchar(sum)){
    output[e] <- as.numeric(substr(sum, nchar(sum)+1-e, nchar(sum)+1-e))
  }
  return(output)
}
addtwonumbers1(c(2, 4, 3), c(5, 6, 4))
addtwonumbers1(c(0), c(0))
addtwonumbers1(c(9, 9, 9, 9, 9, 9, 9), c(9, 9, 9, 9))


#3. longest substring without repeating characters
without <- function(string){
  combi <- c()
  for (i in 1:nchar(string)){
    for (j in i:(nchar(string))){
      combi <- append(combi, substr(string, i, j))
    }
  }
  l <- c()
  for (i in 1:length(combi)){
    length <- length(unique(strsplit(combi[i], "")[[1]]))
    if (nchar(combi[i]) == length){
      l <- append(l, length) 
    }
  }
  return(max(l))
}
without("abcabcbb")
without("bbbbb")
without("pwwkew")
