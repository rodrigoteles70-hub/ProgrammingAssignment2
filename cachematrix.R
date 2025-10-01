## makeCacheMatrix: cria um objeto especial que armazena uma matriz e sua inversa
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # inicializa o cache da inversa
  
  # função para definir uma nova matriz
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # função para obter a matriz
  get <- function() x
  
  # função para armazenar a inversa
  setInverse <- function(inverse) inv <<- inverse
  
  # função para obter a inversa
  getInverse <- function() inv
  
  # retorna a lista de funções
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: calcula a inversa da matriz armazenada em makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # se a inversa já foi calculada, retorna do cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # se não, calcula
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
