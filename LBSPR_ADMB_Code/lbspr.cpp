#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <lbspr.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  Mk.allocate("Mk");
  Linf.allocate("Linf");
  CVLinf.allocate("CVLinf");
  NGTG.allocate("NGTG");
  MaxSD.allocate("MaxSD");
  NLenMids.allocate("NLenMids");
  LenMids.allocate(1,NLenMids,"LenMids");
  ObsLength.allocate(1,NLenMids,"ObsLength");
  LenBins.allocate(1,NLenMids+1,"LenBins");
  L50.allocate("L50");
  L95.allocate("L95");
  FecB.allocate("FecB");
  Mpow.allocate("Mpow");
  logSL50Min.allocate("logSL50Min");
  logSL50Max.allocate("logSL50Max");
  logDeltaMin.allocate("logDeltaMin");
  logDeltaMax.allocate("logDeltaMax");
  kslope.allocate("kslope");
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  logSL50.allocate(logSL50Min,logSL50Max,1,"logSL50");
  logDelta.allocate(logDeltaMin,logDeltaMax,1,"logDelta");
  logFM.allocate(2,"logFM");
  SDLinf.allocate("SDLinf");
  #ifndef NO_AD_INITIALIZE
  SDLinf.initialize();
  #endif
  GTGLinf.allocate("GTGLinf");
  #ifndef NO_AD_INITIALIZE
  GTGLinf.initialize();
  #endif
  LinfdL.allocate("LinfdL");
  #ifndef NO_AD_INITIALIZE
  LinfdL.initialize();
  #endif
  Delta.allocate("Delta");
  #ifndef NO_AD_INITIALIZE
  Delta.initialize();
  #endif
  EP0.allocate("EP0");
  #ifndef NO_AD_INITIALIZE
  EP0.initialize();
  #endif
  EPf.allocate("EPf");
  #ifndef NO_AD_INITIALIZE
  EPf.initialize();
  #endif
  MatDelta.allocate("MatDelta");
  #ifndef NO_AD_INITIALIZE
  MatDelta.initialize();
  #endif
  DiffLinfs.allocate(1,NGTG,"DiffLinfs");
  #ifndef NO_AD_INITIALIZE
    DiffLinfs.initialize();
  #endif
  RecProbs.allocate(1,NGTG,"RecProbs");
  #ifndef NO_AD_INITIALIZE
    RecProbs.initialize();
  #endif
  PredLenComp.allocate(1,NLenMids,"PredLenComp");
  #ifndef NO_AD_INITIALIZE
    PredLenComp.initialize();
  #endif
  PredUnfishedComp.allocate(1,NLenMids,"PredUnfishedComp");
  #ifndef NO_AD_INITIALIZE
    PredUnfishedComp.initialize();
  #endif
  PredFishedComp.allocate(1,NLenMids,"PredFishedComp");
  #ifndef NO_AD_INITIALIZE
    PredFishedComp.initialize();
  #endif
  Vul.allocate(1,NLenMids+1,"Vul");
  #ifndef NO_AD_INITIALIZE
    Vul.initialize();
  #endif
  MkL.allocate(1,NLenMids+1,"MkL");
  #ifndef NO_AD_INITIALIZE
    MkL.initialize();
  #endif
  FkL.allocate(1,NLenMids+1,"FkL");
  #ifndef NO_AD_INITIALIZE
    FkL.initialize();
  #endif
  Mat.allocate(1,NLenMids,"Mat");
  #ifndef NO_AD_INITIALIZE
    Mat.initialize();
  #endif
  Wght.allocate(1,NLenMids,"Wght");
  #ifndef NO_AD_INITIALIZE
    Wght.initialize();
  #endif
  Fec.allocate(1,NLenMids,"Fec");
  #ifndef NO_AD_INITIALIZE
    Fec.initialize();
  #endif
  EP0_gtg.allocate(1,NGTG,"EP0_gtg");
  #ifndef NO_AD_INITIALIZE
    EP0_gtg.initialize();
  #endif
  EPf_gtg.allocate(1,NGTG,"EPf_gtg");
  #ifndef NO_AD_INITIALIZE
    EPf_gtg.initialize();
  #endif
  PUnFished.allocate(1,NLenMids+1,"PUnFished");
  #ifndef NO_AD_INITIALIZE
    PUnFished.initialize();
  #endif
  PFished.allocate(1,NLenMids+1,"PFished");
  #ifndef NO_AD_INITIALIZE
    PFished.initialize();
  #endif
  NUnFished.allocate(1,NLenMids,"NUnFished");
  #ifndef NO_AD_INITIALIZE
    NUnFished.initialize();
  #endif
  NFished.allocate(1,NLenMids,"NFished");
  #ifndef NO_AD_INITIALIZE
    NFished.initialize();
  #endif
  currMkL.allocate(1,NLenMids+1,"currMkL");
  #ifndef NO_AD_INITIALIZE
    currMkL.initialize();
  #endif
  currZkL.allocate(1,NLenMids+1,"currZkL");
  #ifndef NO_AD_INITIALIZE
    currZkL.initialize();
  #endif
  UnfishedMatrix.allocate(1,NGTG,1,NLenMids,"UnfishedMatrix");
  #ifndef NO_AD_INITIALIZE
    UnfishedMatrix.initialize();
  #endif
  FishedMatrix.allocate(1,NGTG,1,NLenMids,"FishedMatrix");
  #ifndef NO_AD_INITIALIZE
    FishedMatrix.initialize();
  #endif
  MKLMat.allocate(1,NGTG,1,NLenMids+1,"MKLMat");
  #ifndef NO_AD_INITIALIZE
    MKLMat.initialize();
  #endif
  ZKLMat.allocate(1,NGTG,1,NLenMids+1,"ZKLMat");
  #ifndef NO_AD_INITIALIZE
    ZKLMat.initialize();
  #endif
  FMpar.allocate("FMpar");
  SPR.allocate("SPR");
  SL50.allocate("SL50");
  SL95.allocate("SL95");
  temp.allocate(1,NLenMids+1,"temp");
  #ifndef NO_AD_INITIALIZE
    temp.initialize();
  #endif
  tempNum.allocate("tempNum");
  #ifndef NO_AD_INITIALIZE
  tempNum.initialize();
  #endif
  obj_fun.allocate("obj_fun");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
}

void model_parameters::preliminary_calculations(void)
{

#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
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
}

void model_parameters::userfunction(void)
{
  obj_fun =0.0;
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
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
 report << NLenMids << endl; // number of length classes
 report << PredLenComp << endl; // model fit
 report << ObsLength << endl; // original length data
 report << LenMids << endl; // length bins
 report << PredUnfishedComp << endl; // Unfished length composition
 report << PredFishedComp << endl; // Fished length composition
 report << obj_fun << endl; // likelihood obj_fun
 report << objective_function_value::pobjfun->gmax; // maximum gradient value
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
 arrmblsize = 500000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
 gradient_structure::set_MAX_NVAR_OFFSET(5000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
