DATA_SECTION
  init_int nclust
  init_int maxlgth
  init_matrix pgammas_mat(1,nclust,1,maxlgth)
  init_vector probs_cum(1,maxlgth)

PARAMETER_SECTION
  init_bounded_vector a(1,nclust,0,1)
  number output
  objective_function_value f

PROCEDURE_SECTION

  dvar_vector Pred(1,maxlgth);

  Pred =  a(1)*extract_row(pgammas_mat,1);
  for(int x=2; x<=nclust; x++)
  {
  Pred +=  a(x)*extract_row(pgammas_mat,x);
  }

  f = norm2(Pred-probs_cum);
  output = f;

