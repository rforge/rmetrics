
//#include <oxstd.h>  
//#include <packages/gnudraw/gnudraw.h>


#import <packages/Garch40/garch>


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
    garchobj = new Garch();
    
	
//*** DATA INPUT ***//
	
	garchobj.Load("OxSeries.csv");			
	// garchobj.Info();                                         
    garchobj.Select(Y_VAR, {"X", 0, 0} );					 
	garchobj.SetSelSample(-1, 1, i_nt, 1);  
	        		

//*** SPECIFICATIONS ***//
	

	garchobj.CSTS(i_csts1, i_csts2); 			
	
		// 	cst in Mean (1 or 0), cst in Variance (1 or 0)	
	
	garchobj.DISTRI(i_distri);				
	
		// 	0 for Gauss, 1 for Student, 2 for GED, 3 for Skewed-Student
	
	garchobj.ARMA_ORDERS(i_arma_orders1, i_arma_orders2); 		
	
		// 	AR order (p), MA order (q).
	
	garchobj.ARFIMA(i_arfima);				
	
		// 	1 if Arfima wanted, 0 otherwise
	
	garchobj.GARCH_ORDERS(i_garch_orders1, i_garch_orders2);		
	
		// 	p order, q order
	
	garchobj.ARCH_IN_MEAN(i_arch_in_mean);		
	
		// 	ARCH-in-mean: 1 or 2 to add the variance 
		//		or std. dev in the  cond. mean						
	
	garchobj.MODEL(i_model);		
	
		//	0: RISKMETRICS  
		//	1:GARCH		
		//	2:EGARCH	
		//	3:GJR	
		//	4:APARCH	
		//	5:IGARCH
		//  6:FIGARCH-BBM	
		//	7:FIGARCH-CHUNG	  
		//	8:FIEGARCH
		//  9:FIAPARCH-BBM	
		//	10: FIAPARCH-CHUNG	
		//	11: HYGARCH
	
	garchobj.TRUNC(i_trunc);
				
		// Truncation order (only F.I. models with BBM method)

	
//*** TESTS & FORECASTS ***//	


	garchobj.BOXPIERCE(<10;15;20>);	
	
		// 	Lags for the Box-Pierce Q-statistics, <> otherwise
		
	garchobj.ARCHLAGS(<2;5;10>);	
	
		// 	Lags for Engle's LM ARCH test, <> otherwise
	
	garchobj.NYBLOM(1);				
	
		// 	1 to compute the Nyblom stability test, 0 otherwise  
	
	garchobj.SBT(1);				
	
		// 	1 to compute the Sign Bias test, 0 otherwise  
	
	garchobj.PEARSON(<40;50;60>);	
			
		// 	Cells (<40;50;60>) for the adjusted Pearson Chi-square 
		//	Goodness-of-fit test, <> otherwise //G@RCH1.12
	
	garchobj.RBD(<10;15;20>);		
	
		//  Lags for the Residual-Based Diagnostic test of Tse, <> otherwise

						
//*** FORECASTS ***//
	

	garchobj.FORECAST(1,15,1);     	
	
		// Arg.1 : 1 to launch the forecasting procedure, 0 otherwize 
		// Arg.2 : Number of forecasts
		// Arg.3 : 1 to Print the forecasts, 0 otherwise 

			
//*** OUTPUT ***//	


	garchobj.MLE(1);				
		
		// 	0 : MLE (Second derivatives), 
		//	1 : MLE (OPG Matrix), 
		//	2 : QMLE
		
	garchobj.COVAR(1);			    
	
		// if 1, prints variance-covariance matrix of the parameters.
		
	garchobj.ITER(5);				
	
		// 	Interval of iterations between printed intermediary 
		//		results (if no intermediary results wanted, enter '0')
	
	garchobj.TESTS(0,1);			
	
		// 	Arg. 1 : 1 to run tests PRIOR to estimation, 0 otherwise
		// 	Arg. 2 : 1 to run tests AFTER estimation, 0 otherwise
		

//*** PARAMETERS ***//	


	garchobj.BOUNDS(1);				
	
		// 	1 if bounded parameters wanted, 0 otherwise
	
	garchobj.FIXPARAM(0,<0;0;0;0;1;0>);
			
		// 	Arg.1 : 1 to fix some parameters to their starting 
		//			values, 0 otherwize
		// 	Arg.2 : 1 to fix (see garchobj.DoEstimation(<>)) 
		//			and 0 to estimate the corresponding parameter
		
		
//*** ESTIMATION ***//


	// garchobj.MAXSA(0,5,0.5,20,5,2,1);  
	
		// 	Arg.1 : 1 to use the MaxSA algorithm of Goffe, 
		//			Ferrier and Rogers (1994) 
		//         	and implemented in Ox by Charles Bos 
		//  Arg.2 : dT=initial temperature 
		//	Arg.3 : dRt=temperature reduction factor 
		// 	Arg.4 : iNS=number of cycles 
		// 	Arg.5 : iNT=Number of iterations before temperature reduction 
		// 	Arg.6 : vC=step length adjustment	 
		// 	Arg.7 : vM=step length vector used in initial step 

	garchobj.Initialization(<>);
	
		// m_vPar = m_clevel | m_vbetam |  m_dARFI | m_vAR | m_vMA | 
		//		m_calpha0 | m_vgammav | m_dD |  m_vbetav |
		//		m_valphav | m_vleverage | m_vtheta1 | m_vtheta2 | 
		//		m_vpsy | m_ddelta | m_cA | m_cV | m_vHY | m_v_in_mean

	garchobj.PrintStartValues(1);	
	  
		// 	1: Prints the S.V. in a table form; 
		//	2: Individually;  
		//	3: in a Ox code to use in StartValues
		
	garchobj.PrintBounds(1);
	
	garchobj.DoEstimation();
	
	garchobj.Output();
			
	// garchobj.STORE(0,0,0,1,1,"01",0);  
	
		//  Arg.1,2,3,4,5 : 
		//			if 1 -> stored. (Res-SqRes-CondV-MeanFor-VarFor)
		//  Arg.6 : Suffix. The name of the saved series will be "Res_ARG6" 
		//			(or "MeanFor_ARG6", ...).	
		//  Arg.7 : if 0, saves as an Excel spreadsheet (.xls). If 1, saves	
		//			as a GiveWin dataset (.in7)
	
	 	
  	decl estpar, m_vPar, m_vStdErrors; 
	m_vPar = garchobj.GetValue("m_vPar");
  	m_vStdErrors = garchobj.GetValue("m_vStdErrors");
  	estpar = (m_vPar)~(m_vStdErrors)'~(m_vPar./(m_vStdErrors)');  
  	
	file = fopen("OxParameters.csv", "w"); 					   
	fprint(file, estpar );		   						// Parameters
	fclose(file);
	
	file = fopen("OxResiduals.csv", "w"); 
	fprint(file, garchobj.GetValue("m_vE") );		   	// Residuals
	fclose(file);
	
	file = fopen("OxCondVars.csv", "w"); 
	fprint(file, garchobj.GetValue("m_vSigma2") );	   	// Conditional Variances
	fclose(file);
	
	delete garchobj;
}
