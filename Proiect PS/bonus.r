# create a row or column of the matrix
# with random values such that sum of vector is limit
# usage : limit = 1 to generate the last line or column
#       : limit = pi v qj to generate a a row or column of the joint distribution 

create_line <- function(limit,end)
{
  sum <- 0
  x <- vector()
  for (i in 1:(end-1))
  {
    gen <- runif(1,0,limit - sum)
  
    x <- c(x,gen)
    sum <- sum + gen
    
  }
  x <- c(x,limit-sum)
  
  return (x)
  
}
# create names for row and col , pi with i=1,n and qi with i= 1,m

create_rc_names <- function(chr1,chr2,n){
  names <- vector()
  for (i in 1:n){
    names <- c(names,paste(chr1,i,sep=""))
  }
  names <- c(names,paste(chr2,"i",sep=""))
  
  return (names)
}

# punctul a

frepcomgen <- function(n,m){
  
  X <- sort(sample(0:100,n))
  Y <- sort(sample(0:100,m)) 
  
  # generate names
  row_names <-  create_rc_names(chr1="x",chr2="p",n)
  
  col_names <- create_rc_names(chr1="y",chr2="q",m)
  
  #create matrix
  mat <- matrix(,nrow= n+1, ncol=m+1,dimnames= list(row_names,col_names))
  temp <- create_line(1,m)
  mat[n+1,] <- c(temp,1)
  
  
  #generate columns values
  for ( i in 1:m){
    
    new_col <- create_line(mat[n+1,i],n)
    unknown <- sample(1:n,1)
    new_col[unknown] <- 0
    mat[,i] <- c(new_col,mat[n+1,i])
    
    
  }
  
  results <- list(mat,X,Y)
  return (results)
  
  
  
}

print(mat)

# punctul b


fcomplrepcom <- function(n,m){
  
  results <- frepcomgen(n,m)
  mat <- results[[1]]
  X <-results[[2]]
  Y <- results[[3]]
  
  px <- vector()
  py <- vector()
  
  print('Inainte de completare')
  print(' ')
  print('Variabila aleatore X')
  print(' ')
  
  
  for( i in 1:length(X)){
    print(paste("x",i,"=",X[i],sep=""))
  }
  
  
  print(' ')
  print('Variabila aleatore Y\n')
  print(' ')
 
  for(i in 1:length(Y))
    
  {
    print(paste("y",i,"=",Y[i],sep=""))
    
    
  }
  print("Repartitia comuna inainte de completare(val necompletate sunt =0 sau NA")
  print(mat)
  for(i in 1:n){
    
    for(j in 1:m){
      
      if (all.equal(mat[i,j],0) == TRUE ){
        
        temp <- mat[,j]
        mat[i,j]<- temp[length(temp)] - sum(temp[-(n+1)])
      
        
        
      }
    }
  }
  
  for(i in 1:n){
    
      
      temp <- mat[i,]
      mat[i,m+1] <- sum(temp[-(m+1)])
    
  }
  
  
  print('Repartitia comuna dupa completarea tabelului')
  print(mat)
  res <-list(mat,X,Y)
  return (res)
  
  
  
  
}



# punctul c

results <- fcomplrepcom(4,7)
mat <- results[[1]]
X <-results[[2]]
Y <- results[[3]]

# E(Y)
Ey <- 0
for(i in 1:length(Y)){
  
  Ey <- Ey + Y[i]*mat[length(X)+1,i]
}

#E(Y^2)

Ey2 <- 0
for(i in 1:length(Y)){
  
  Ey2 <- Ey2 + Y[i]*mat[length(X)+1,i]*mat[length(X)+1,i]
}


#E(X)

Ex <- 0
for(i in 1:length(X)){
  
  Ex <- Ex + X[i]*mat[i,length(Y)+1]
}

#E(X^2)

Ex2 <- 0
for(i in 1:length(X)){
  
  Ex2 <- Ex2 + X[i]*mat[i,length(Y)+1]*mat[i,length(Y)+1]
}


#E(XY)
Exy <- 0
for( i in 1:length(X)){
  
  for( j in 1:length(Y)){
    
    Exy <- Exy + mat[i,j]*X[i]*Y[j]
    
    
  }
}

# Folosim Cov(aX,bY) = abCov(X,Y)
Covar <- -15 * (Exy - Ex*Ey)



#functie care calc. repartitia var aleatoare V F(V<=x)
# flag =0 , pt V=X
# flag = 1, pt V=Y
repartitie <- function(x,flag)
{
  i <- 1
  value <- 0
  
  probY <- mat[nrow(mat),]
  probX <- mat[,ncol(mat)]
  if(flag == 0){
    
    if( x > X[length(X)])
          value <- 1
    else if ( x < X[1])
          value <- 0
    else{
      
      
       while( x >= X[i])
       {
         value <- value + probX[i]
         i <- i+1
       }
      
    }
    
    
  }
  else
  {
    if( x > Y[length(Y)])
      value <- 1
    else if ( x < Y[1])
      value <- 0
    else{
      
      
      while( x >= Y[i])
      {
        value <- value + probY[i]
        i <- i+1
      }
      
    }
    
    
    
  }
  
  return (value)
  
  
}

#functie ajutatoare pentru a calcula Fxy(x,y)

find_breakpoint <-function(x,y){
  
  i <- 1
  j <- 1

  if (x > X[length(X)]){
    
    i <- length(X) + 1
  }
  
  else
    {
    while(x >= X[i])
      i <-  i +1
    
    }
  
  
  if ( y > Y[length(Y)]){
    
    j <- length(Y) + 1
  }
  else{
    
    while(y >= Y[j])
      j <- j+1
    
  }
  

  
  res <-list(i-1,j-1)
  return (res)
}

Fxy <- function(x,y)
{
  res <- find_breakpoint(x,y)
  value <- 0
  endx <- res[[1]]
  endy <- res[[2]]
  # x < X[1], Y < Y[1]
  if( endx == 0 && endy == 0)
    return (value)
  else{
    for( i in 1:endx){
    
    for(j in 1:endy){
      
      value <-value + mat[i,j]
      
    }
    }
  }
  
  return (value)
  
}


#P(0<X<3\Y>2) = P(0<X<3,Y>2) / P(Y>2)
# P(0<X<3,2<Y) = Fx(3) - Fx(0) - Fxy(3,2) + Fxy(0,2)
# P(Y>2) = 1 - P(Y <=2) =  1 - Fy(2)


numitor <- 1 - repartitie(2,flag = 1)
numarator <- repartitie(3,flag = 0) - repartitie(0,flag = 0) -Fxy(3,2) + Fxy(0,2)

P1 <- numarator / numitor


# P(X>6,Y<7) = Fy(7)-Fxy(6,7)
P2 <- repartitie(7,flag =1) - Fxy(6,7)



# punctul d
#  X,Y independente ddaca PI_ij = pi*qj pt orice i,j
fverind <- function()
{
  for(i in 1:length(X)){
    
    for( j in 1:length(Y))
    {
      
      if( all.equal(mat[nrow(mat),j]*mat[i,ncol(mat)],mat[i,j]) != TRUE)
        return (FALSE)
    }
  }
  return (TRUE)
}

# coef corelatie = 0 => X,Y sunt necorelate

fvernecor <- function(){
    
  numarator <-  Exy-Ex*Ey
  VarX <- Ex2 - Ex*Ex
  VarY <- Ey2 - Ey*Ey
  numitor <- sqrt(VarX*VarY)
  coefCor <- numarator/ numitor
  print(coefCor)

  if( coefCor == 0)
    return (TRUE)
  
  return(FALSE)
}






