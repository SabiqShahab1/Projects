# set size of grid
size <- 8
#initial value of alive cells
AC <- 0
#creating grid of 0-1 values
matrix <- matrix(sample(c(TRUE:FALSE), size^2, replace = TRUE), nrow = size, ncol = size)


countNeighbors <- function(matrix, x, y) {
  
  # Go through the neighbors centered on (x, y)
  for (a in -1:1) {
    for (b in -1:1) {
      # Skip (x, y) itself
      if (a == 0 & b == 0) next
      
      # Find the neighbor cell coordinates
      X <- x + a
      Y <- y + b
      
      # Check if the cell is within the matrix
      if (X >= 1 & X <= nrow(matrix) & Y >= 1 & Y <= ncol(matrix)) {
        # If cell is alive, increase the alive cell count
        if (matrix[X, Y]) {
          AC <- AC + 1
        }
      }
    }
  }
  #give the total number of alive cells
  return(AC)
}

updateCells <- function(matrix) {
  #create a new updated matrix
  Next <- matrix
  #go through each cell of the matrix
  for (x in 1:nrow(matrix)) {
    for (y in 1:ncol(matrix)) {
      #calculate the alive neighbors of the cells the function goes through     
      AN <- countNeighbors(matrix, x, y)
      #if the value of the cell checked is true
      if (matrix[x, y]) {
        #and the alive neighbor value is less than 2 or more than 3
        if (AN < 2 | AN > 3) {
          #set the cell value to false for the new updated matrix
          Next[x,y] <- FALSE
        }
      }
      #else if the value of the cell checked is false
      else {
        #and the value of alive neighbors is = to 3
        if (AN == 3) {
          #set the cell value to true for the new updated matrix
          Next[x, y] <- TRUE
        }
      }
    }
  }
  #give the new updated matrix
  return(Next)
}

#checking if function works
matrix <- updateCells(matrix)

#creating GOL function with matrix input, and R = number of runs
runGOL <- function(matrix, R) {
#setting i to 0
  i <- 0
#repeating the updateCells function while updating the matrix each time until i reaches R
  repeat {
    matrix = updateCells(matrix)
    print(matrix)
    i <- i + 1
    if (i == R) {
      break
    }
  }
}

#creating the glider shape
matrix <- matrix(0, nrow = 6, ncol = 6)
matrix[4,2:4] <- 1
matrix[3,4] <- 1
matrix[2,3] <- 1
#testing if the GOL runs correctly
runGOL(matrix, 12)
