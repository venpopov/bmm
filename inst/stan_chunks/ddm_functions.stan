// Likelihood function for the 7-parameter ddm
real ddm_lpdf (real rt, real mu, real drift, real bound, real ndt, real zr, real sdrift, real sndt, real szr, int dec) {
  if (dec == 1) {
    return wiener_lpdf (rt | bound, ndt, zr, drift, sdrift, szr, sndt);
  } else {
    return wiener_lpdf (rt | bound, ndt, 1 - zr, -drift, sdrift, szr, sndt);
  }
}
