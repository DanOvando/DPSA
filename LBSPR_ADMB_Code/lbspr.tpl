// TPL file for LBSPR estimation procedure
// GTG Model - Length Structured
DATA_SECTION
  init_number Mk;
  init_number Linf;
  init_number CVLinf;

  init_number NGTG;
  init_number MaxSD;

  init_number NLenMids;
  init_vector LenMids(1,NLenMids);
  init_vector ObsLength(1,NLenMids);
  init_vector LenBins(1,NLenMids+1);

  init_number L50;
  init_number L95;
  init_number FecB;

  init_number Mpow;

  init_number logSL50Min;
  init_number logSL50Max;
  init_number logDeltaMin;
  init_number logDeltaMax;

  init_number kslope;

// ----------------------------------------------------------------------------
 PARAMETER_SECTION
  init_bounded_number logSL50(logSL50Min,logSL50Max,1);           //
  init_bounded_number logDelta(logDeltaMin,logDeltaMax,1);
  init_number logFM(2);  //

  // Storage
  number SDLinf;
  number GTGLinf;
  number LinfdL;
  number Delta;
  number EP0;
  number EPf;
  number MatDelta;

  vector DiffLinfs(1,NGTG);
  vector RecProbs(1,NGTG);
  vector PredLenComp(1,NLenMids);
  vector PredUnfishedComp(1,NLenMids);
  vector PredFishedComp(1,NLenMids);
  vector Vul(1,NLenMids+1);
  vector MkL(1,NLenMids+1);
  vector FkL(1,NLenMids+1);
  vector Mat(1,NLenMids);
  vector Wght(1,NLenMids);
  vector Fec(1,NLenMids);
  vector EP0_gtg(1,NGTG);
  vector EPf_gtg(1,NGTG);
  vector PUnFished(1,NLenMids+1);
  vector PFished(1,NLenMids+1);
  vector NUnFished(1,NLenMids);
  vector NFished(1,NLenMids);
  vector currMkL(1,NLenMids+1);
  vector currZkL(1,NLenMids+1);

  matrix UnfishedMatrix(1,NGTG,1,NLenMids);
  matrix FishedMatrix(1,NGTG,1,NLenMids);
  matrix MKLMat(1,NGTG,1,NLenMids+1);
  matrix ZKLMat(1,NGTG,1,NLenMids+1);

  sdreport_number FMpar;
  sdreport_number SPR;
  sdreport_number SL50;
  sdreport_number SL95;

  vector temp(1,NLenMids+1); // delete when finished debugging
  number tempNum; // delete when finished debugging

  objective_function_value obj_fun;

// ----------------------------------------------------------------------------
PRELIMINARY_CALCS_SECTION
 int X;
 double pi = 3.14159265358979323844;
 MatDelta = L95 - L50;
 Mat = 1.0/(1+mfexp(-log(19)*(LenMids-L50)/MatDelta));
 Fec = elem_prod(Mat, pow(LenMids, FecB));
 Fec = Fec/max(Fec);

 SDLinf = CVLinf * Linf;
 LinfdL = ((Linf + MaxSD * SDLinf) - (Linf - MaxSD * SDLinf))/(NGTG-1);
 for (X=0;X<NGTG;X++) {
   DiffLinfs(X+1) = (Linf - MaxSD * SDLinf) + X * LinfdL;
 }

 RecProbs = 1/(sqrt(2*pi*SDLinf*SDLinf)) * mfexp(-(elem_prod((DiffLinfs-Linf),(DiffLinfs-Linf)))/(2*SDLinf*SDLinf));
 RecProbs = RecProbs/sum(RecProbs);



PROCEDURE_SECTION
  int Gtype, L;
  FMpar = mfexp(logFM); // estimated F/M
  SL50 = mfexp(logSL50);
  Delta = mfexp(logDelta);

  Vul = 1.0/(1+mfexp(-log(19)*(LenBins-SL50)/Delta));

  MkL = Mk * pow(Linf/LenBins, Mpow);
  FkL = FMpar * Mk * Vul;

  for (Gtype=1;Gtype<=NGTG;Gtype++) {
	MKLMat(Gtype) = MkL + kslope*(DiffLinfs(Gtype) - Linf);
    ZKLMat(Gtype) = MKLMat(Gtype) + FkL;
	currMkL = MKLMat(Gtype);
	currZkL = ZKLMat(Gtype);

    PUnFished = 0;
    PFished = 0;
    NUnFished = 0;
    NFished = 0 ;
    PUnFished(1) = RecProbs(Gtype);
    PFished(1) = RecProbs(Gtype);
    GTGLinf = DiffLinfs(Gtype);

    for (L=2;L<=NLenMids+1;L++) {
     if (LenBins(L) < GTGLinf) {
       PUnFished(L) = PUnFished(L-1) * pow(((GTGLinf-LenBins(L))/(GTGLinf-LenBins(L-1))),currMkL(L-1));
       PFished(L) = PFished(L-1) * pow(((GTGLinf-LenBins(L))/(GTGLinf-LenBins(L-1))),currZkL(L-1));
     }
     if (LenBins(L) >= GTGLinf) {
       PUnFished(L) = 0;
       PFished(L) = 0;
     }
   }

   for (L=1;L<=NLenMids;L++) {
 	NUnFished(L)  = (PUnFished(L) - PUnFished(L+1))/currMkL(L);
 	NFished(L) =  (PFished(L) - PFished(L+1))/currZkL(L);
   }

  UnfishedMatrix(Gtype) =  NUnFished;
  FishedMatrix(Gtype) = NFished;

  EP0_gtg(Gtype) = sum(elem_prod(NUnFished, Fec));
  EPf_gtg(Gtype) = sum(elem_prod(NFished, Fec));
  }

  EP0 = sum(EP0_gtg);
  EPf = sum(EPf_gtg);
  SPR =  EPf/EP0;

  PredUnfishedComp = colsum(UnfishedMatrix);
  PredUnfishedComp = PredUnfishedComp/sum(PredUnfishedComp);

  // cout<<"hello"<<endl;

  PredFishedComp = colsum(FishedMatrix);
  PredFishedComp = PredFishedComp/sum(PredFishedComp);

  PredLenComp = colsum(FishedMatrix);
  PredLenComp =  elem_prod(PredLenComp,  1.0/(1+mfexp(-log(19)*(LenMids-SL50)/Delta)));
  PredLenComp = PredLenComp/sum(PredLenComp);
  SL95 = SL50 + Delta;

  obj_fun = -sum(elem_prod(ObsLength, log(elem_div(PredLenComp+0.00000001,ObsLength+0.00000001)))); // AP from ADMB living doc

// ----------------------------------------------------------------------------
REPORT_SECTION

 report << NLenMids << endl; // number of length classes
 report << PredLenComp << endl; // model fit
 report << ObsLength << endl; // original length data
 report << LenMids << endl; // length bins
 report << PredUnfishedComp << endl; // Unfished length composition
 report << PredFishedComp << endl; // Fished length composition
 report << obj_fun << endl; // likelihood obj_fun
 report << objective_function_value::pobjfun->gmax; // maximum gradient value

// ----------------------------------------------------------------------------
TOP_OF_MAIN_SECTION
 arrmblsize = 500000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
 gradient_structure::set_MAX_NVAR_OFFSET(5000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
