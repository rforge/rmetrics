#include <oxstd.h> 
#include <oxfloat.h>
#import <maximize>
#import <modelbase>
#include <oxdraw.h>
#import <packages/GARCH30/garch>



StartValues(const object)
{
	object.SetStartValue("m_clevel",0.066);
	object.SetStartValue("m_calpha0",0.2);
	object.SetStartValue("m_vbetav",<0.8>);
	object.SetStartValue("m_valphav",<0.1>);

	object.GetPara();  									

		// DO NOT REMOVE THIS LINE WHEN USING THIS FUNCTION
		
	object.Initialization(object.GetValue("m_vPar"));  	
	                         
		// DO NOT REMOVE THIS LINE WHEN USING THIS FUNCTION
}


class GARCH30 : Garch
{                                    					          
	// constructor
	
	GARCH30();
	cfunc_gt0(const avF, const vP);
};


GARCH30::GARCH30()
{
   this.Garch();
   m_iModelClass = MC_GARCH;
   Init_Globals();
}


GARCH30::cfunc_gt0(const avF, const vP)
{
	avF[0] = matrix(1.0001 - vP[2] - vP[3]);
	return 1;
}


// -----------------------------------------------------------------------------


main()
{
	
	
//*** PARAMETER INPUT ***//		
	decl garchobj; 
    decl 
    	i_csts1, i_csts2, 
    	i_distri,
    	i_arma_orders1, i_arma_orders2,
    	i_arfima,
    	i_garch_orders1, i_garch_orders2,
    	i_model,
    	i_arch_in_mean,
    	i_trunc,
    	i_nt;
  	decl file;    
    file = fopen("OxParameter.txt");   
    fscan(file, 
    	&i_csts1, &i_csts2, 
    	&i_distri, 
    	&i_arma_orders1, &i_arma_orders2,
    	&i_arfima,
    	&i_garch_orders1, &i_garch_orders2,
    	&i_model,
    	&i_arch_in_mean,
    	&i_trunc,
    	&i_nt);  
    fclose(file);
    
    
    garchobj = new GARCH30();
    
	
//*** DATA INPUT ***//

	garchobj.Load("OxSeries.csv");			
	// garchobj.Info();                                         
    garchobj.Select(Y_VAR, {"X", 0, 0} );					 
	garchobj.SetSelSample(-1, 1, i_nt, 1);  
	 		
	
//*** SPECIFICATIONS ***//

	garchobj.CSTS(i_csts1,i_csts2);			
	
		// cst in Mean (1 or 0), 
		// cst in Variance (1 or 0)	
		// DEFAULT: 1, 1
		
	garchobj.DISTRI(i_distri);				
	
		// 0 for Gauss, 
	  	// 1 for Student, 
		// 2 for GED, 
	 	// 3 for Skewed-Student
	   	// DEFAULT: 0
	   	
	garchobj.ARMA_ORDERS(i_arma_orders1, i_arma_orders2); 		
											
		// AR order (p), 
		// MA order (q).
		// DEFAULT: 0, 0
		
	garchobj.ARFIMA(i_arfima);				
		
		// 1 if Arfima wanted, 
		// 0 otherwise
		// DEFAULT: 0
		
	garchobj.GARCH_ORDERS(i_garch_orders1, i_garch_orders2);		
											
		// p order, 
		// q order.
		// DEFAULT: 1, 1
		
	garchobj.MODEL(i_model);				
	
		//  1 : GARCH 
		//  2 : EGARCH 
		//  3 : GJR 
		//  4 : APARCH	
		//  5 : IGARCH 
		//  6 : FIGARCH(BBM) 
		//  7 : FIGARCH(Chung)	
		//  8 : FIEGARCH(BM only) 
		//  9 : FIAPARCH(BBM)	
		// 10 : FIAPARCH(Chung) 
		// 11 : HYGARCH(BBM)
		// DEFAULT: 1
		
	garchobj.ARCH_in_mean(i_arch_in_mean);		
											
		// ARCH-in-mean: 
	 	// 1 add variance in cond. mean 
		// 2 add cond. std., 
	 	// 0 otherwise 	
	 	// DEFAULT: 0		
	 				
	garchobj.TRUNC(i_trunc);				
	
		// Truncation order 
	 	// (only F.I. models with BBM method)
		// DEFAULT: 100
	
//*** TESTS & FORECASTS ***//	

	garchobj.BOXPIERCE(<10; 15; 20>);			
	
		// Lags for the Box-Pierce Q-stats, 
		// <> otherwise
		
	garchobj.ARCHLAGS(<2; 5; 10>);			
	
		// Lags for Engle's LM ARCH test, 
		// <> otherwise
		
	garchobj.NYBLOM(1);						
	
		// 1 for Nyblom stability test, 
	    // 0 otherwise  
	    
	garchobj.PEARSON(<40;50;60>);		
	
		// Cells (<40;50;60>) 
	    // for adjusted Pearson Chi-square 
	   	//    Goodness-of-fit test, 
	   	// <> otherwise //G@RCH1.12
	   	
	// garchobj.FORECAST(0, 10, 1);     			
	
		// Arg.1 : 1 launch forecasting procedure, 
	    //         0 otherwise 
		// Arg.2 : Number of forecasts
		// Arg.3 : 1 to Print the forecasts, 
		//         0 otherwise 
									
//*** OUTPUT ***//	

	garchobj.MLE(1);				
	
		// 0 : both, 
	  	// 1 : MLE, 
	 	// 2 : QMLE
	 	
	garchobj.COVAR(1);			    
	
		// if 1, prints variance-covariance matrix 
	 	//   of the parameters.
		// DEFAULT: 0
		
	garchobj.ITER(5);				
	
		// Interval of iterations between printed 
		//   intermediary results (if no 
		//   intermediary results wanted, enter '0')
	 	//   DEFAULT: 10
	 	
	garchobj.TESTS(0, 0);			
	
		// Arg. 1 : 1 run tests PRIOR to estimation, 
	 	//          0 otherwise
		// Arg. 2 : 1 run tests AFTER estimation, 
		//          0 otherwise
	
//*** PARAMETERS ***//	

	garchobj.BOUNDS(1);				
	
		// 1 if bounded parameters wanted, 
	  	// 0 otherwise
	  	
	garchobj.FixParam(0, <1; 0; 0; 0>); 
	
		// Arg.1 : 1 to fix some parameters to 
		//         their starting values, 
		//         0 otherwise
		// Arg.2 : 1 to fix (see 
		//         garchobj.DoEstimation(<>)) and 
		//         0 to estimate the corresponding 
		//         parameter
									
//*** ESTIMATION ***//			
								
	garchobj.Initialization(<>) ;
	
	StartValues(garchobj);
	
	//garchobj.PrintStartValues(1) ;	
	
		// 1: Prints the S.V. in a table form
		// 2: Individually ;  
		// 3: in a Ox code to use in StartValues
		
	garchobj.DoEstimation() ;
       	
		// m_vPar = m_clevel | m_vbetam |  m_dARFI | 
     	//   m_vAR | m_vMA | m_calpha0 | m_vgammav | 
    	//   m_dD |  m_vbetav | m_valphav | 
       	//   m_vleverage | m_vtheta1 | m_vtheta2 | 
   		//   m_vpsy | m_ddelta | m_cA | m_cV | 
      	//   m_vHY | m_v_in_mean

	garchobj.Output();
		
	//garchobj.STORE(1, 0, 1, 1, 1, "Ox", 0); 
	                                
		//  Arg.1,2,3,4,5 : 
	    //          if 1 -> stored. Names are
	   	//          (Res-SqRes-CondV-MeanFor-VarFor)
		//  Arg.6 : Suffix. The name of the saved 
		//          series will be "Res_ARG6" (or 
		//          "MeanFor_ARG6", ...).	
		//  Arg.7 : if 0, saves as an Excel 
		//          spreadsheet (.xls). If 1, saves 
		//          as a GiveWin dataset (.in7)
		//  DEFAULT: 0, 0, 0, 1, 1, "02", 0
	
	
	//file = fopen("OxBoxPierce.csv", "w"); 
	//fprint(file, garchobj.BOXPIERCE(<10;15;20>) );	// Box-Pierce
	//fclose(file);
	
	file = fopen("OxParameters.csv", "w"); 
	fprint(file, garchobj.PAR() );					   // Parameters
	fclose(file);
	
	file = fopen("OxResiduals.csv", "w"); 
	fprint(file, garchobj.GetValue("m_vE") );		   // Residuals
	fclose(file);
	
	file = fopen("OxCondVars.csv", "w"); 
	fprint(file, garchobj.GetValue("m_vSigma2") );	   // Conditional Variances
	fclose(file);
	
	delete garchobj;
}
