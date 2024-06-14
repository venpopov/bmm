  // precompute chebyshev points
  matrix[200,200] COSN;
  for (m in 1:200) {
    for (i in 1:m) {
      COSN[i,m] = cos((2*i-1)*pi()/(2*m))-1;
    }
  }
