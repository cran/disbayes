functions {
  
  // Annual transition probabilities between states
  // in terms of intensities that are constant over transition interval [ e.g. year of age ] 
  // From solution to the Kolmogorov forward equation given in DISMOD2 paper 
  // 1: healthy;  2: disease;  3: dead from disease
  // i: incidence, f: case fatality, r: remission 
  
  matrix trans_probs_rem(real i, real f, real r)  {
    real l = i + r + f;
    real q = sqrt(i*i + 2*i*r -  2*i*f  + r*r + 2*f*r + f*f);
    real w = exp(-(l + q) / 2);
    real v = exp(-(l - q) / 2);
    matrix[3,3] P;
    P[1,1] = (2*(v-w)*(f+r) + v*(q-l) + w*(q+l)) / (2*q);
    P[2,1] = (v-w)*r/q;
    P[3,1] = 0; 
    
    P[1,2] = i*(v - w)/q;
    P[2,2] = -((2*(f+r) - l)*(v-w) - q*(v+w)) / (2*q);
    P[3,2] = 0;
    
    P[1,3] = (-l*(v-w) - q*(v+w))/(2*q) + 1;
    P[2,3] = ((v-w)*(2*f - l) - q*(v+w))/(2*q) + 1;
    P[3,3] = 1;
    return P;
  }
  
  matrix trans_probs_norem_if(real i, real f)  {
    real l = i + f;
    real q = sqrt(i*i - 2*i*f  + f*f);
    real w = exp(-(l + q) / 2);
    real v = exp(-(l - q) / 2);
    matrix[3,3] P;
    P[1,1] = (2*(v-w)*f + v*(q-l) + w*(q+l)) / (2*q);
    P[2,1] = 0; 
    P[3,1] = 0; 
    
    P[1,2] = i*(v - w)/q;
    P[2,2] = -((2*f - l)*(v-w) - q*(v+w)) / (2*q);
    P[3,2] = 0;
    
    P[1,3] = (-l*(v-w) - q*(v+w))/(2*q) + 1;
    P[2,3] = ((v-w)*(2*f - l) - q*(v+w))/(2*q) + 1;
    P[3,3] = 1;
    return P;
  }
  
  matrix trans_probs_norem_i(real i)  {
    matrix[3,3] P;
    P[1,1] = exp(-i);
    P[1,2] = i*exp(-i);
    P[1,3] = -exp(-i) + 1 - i*exp(-i);
    
    P[2,1] = 0;
    P[2,2] = exp(-i);
    P[2,3] =  1- exp(-i);
    
    P[3,1] = 0;
    P[3,2] = 0;
    P[3,3] = 1;
    return P;
  }

    matrix defuzz_P(matrix P){
      matrix[3,3] Pr;
      for (r in 1:3) {
	for (s in 1:3) {
	  Pr[r,s] = P[r,s];
	  if (P[r,s] < 0) Pr[r,s] = 0;
	  if (P[r,s] > 1) Pr[r,s] = 1;
	}
      }
      return Pr;
    }
  
  matrix trans_probs(real i, real f, real r) {
    matrix[3,3] P;
    if (r != 0)
      P = trans_probs_rem(i, f, r);
    else { 
      if (i == f)
	P = trans_probs_norem_i(i);
      else
	P = trans_probs_norem_if(i, f);
    }
    return defuzz_P(P);
  }
  
  real bound_prob(real x){
    real ret;
    if (x >= 1) {
      ret = 1 - machine_precision();
    } else if (x <= 0) {
      ret = machine_precision();
    } else { ret = x; } 
    return x;
  }

}
