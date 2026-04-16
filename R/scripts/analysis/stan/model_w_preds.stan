// predicting coconut choices from all features: distance, pv, ta, clst
// Lucas Watson 2026
// adapted and refactored from Dominik Deffner 2023

// functions block: Here we define the partial sum function for parallel computation of the likelihood.
functions {

real partial_sum(array[] int y_slice, // important! uses only the slice from start to end for that collection number
                   int start, int end, // indices of start/end
                   int N_feat,
                   array[] int C, // number of available coconuts
                   array[] int subject_id, // subject id
                   array[] int level, // level id
                   array[] int collection_number, // collection id
                   array[,,] real Feature_matrix, matrix weights, matrix v_subject , matrix v_level) {

      real logp=0;
      int counter=0;

// loop through collections 
for(i in start:end){
      counter += 1;

  // Vector for choice probabilities
  vector[C[i]] p;

  // slice out relevant coconut infos
  matrix[C[i] , N_feat] features = to_matrix(Feature_matrix[i,1:C[i],]) ;
  
  // compute choice probabilities using softmax function
  p = softmax(features * (weights[ , 1] + v_subject[subject_id[i],]' + v_level[level[i],]' ) );

  // add log probability of observed block choice to target
  logp += categorical_lpmf(y_slice[counter] | p);

}
   return logp;
}

}

// data block: define and name the size of each observed variable
data{
   int N;              // number of observations (coconut choices)
   int N_subjects;           // number of individuals
   int N_levels;       // number of levels
   int N_coconut;      // number of coconuts
   int N_feat;         // number of features
   array[N] int C;           // number of available blocks for each choice
   array[N] int subject_id;  // unique individual identification
   array[N] int level;       // level id
   array[N] int collection_number; // collection number in level
   array[N] int choice;        // chosen coconut
   array[N,N_coconut,N_feat] real Feature_matrix; // feature design matrix for each of N choices, 1 distance, 2 turning angle, 3 clustering, 4 point value
}

// parameter block: define and name the size of each unobserved variable.
parameters{
   matrix[N_feat,1] weights;

   // Varying effects clustered on individual
    matrix[N_feat,N_subjects] z_subject;
    vector<lower=0>[N_feat] sigma_subject;
    cholesky_factor_corr[N_feat] Rho_subject;

    // Varying effects clustered on groups
     matrix[N_feat,N_levels] z_level;
     vector<lower=0>[N_feat] sigma_level;
     cholesky_factor_corr[N_feat] Rho_level;
}

// transformed parameters block: here we multiply z-scores with variances and 
// Cholesky factors to get varying effects back to right scale
transformed parameters{
      matrix[N_subjects,N_feat] v_subject;
      matrix[N_levels,N_feat] v_level;

      v_subject = ( diag_pre_multiply( sigma_subject , Rho_subject ) * z_subject )';
      v_level = ( diag_pre_multiply( sigma_level , Rho_level ) * z_level )';
}

// model block: here compute the log posterior
model{
int grainsize = 1;

  // priors
  
  // weights
  to_vector(weights) ~ normal(0,3); // weakly informed

  // varying effects priors
  to_vector(z_subject) ~ normal(0,1); 
  sigma_subject ~ exponential(1); // regularizing variance prior
  Rho_subject ~ lkj_corr_cholesky(4);

  to_vector(z_level) ~ normal(0,1);
  sigma_level ~ exponential(1); // regularizing variance prior
  Rho_level ~ lkj_corr_cholesky(4);

 target += reduce_sum(partial_sum, choice, grainsize,
                     N_feat, C, subject_id, level, collection_number,
                     Feature_matrix, weights, v_subject, v_level);

}// end model


// we use generated quantities block to compute log pointwise predictive densities for model comparison
generated quantities {

  vector[N] log_lik;
  
  for (i in 1:N) {


  // vector for choice probabilities
  vector[C[i]] p;

  // Slice out relevant coconut infos
  matrix[C[i] , N_feat] features = to_matrix(Feature_matrix[i,1:C[i],]) ;
  
  // compute choice probabilities using softmax function
  p = softmax(features * (weights[ , 1] + v_subject[subject_id[i],]' + v_level[level[i],]' ) );
  
  //Compute log likelihood of observed choice
  log_lik[i] = categorical_lpmf(choice[i] | p);

}

}//end generated quantities
