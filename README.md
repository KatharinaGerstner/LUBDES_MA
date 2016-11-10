LUBDES meta analysis - code and other resources

This github repository contains all R-code and data files created within the LUBDES project. 
The repository is accompanying a paper entitled "The complex trade-offs of land-use intensification for biodiversity and yield" which is authored by:

Michael Beckmann1, Katharina Gerstner1,2, Morodoluwa Akin-Fajiye3, Silvia Ceau??u2,4, Stephan Kambach4,5, Nicole L. Kinlock3, Helen R. P. Phillips6,7, Willem Verhagen8, Jessica Gurevitch3, Stefan Klotz2,5, Tim Newbold9,10, Peter H. Verburg8, Marten Winter2, Ralf Seppelt1,2

1  UFZ - Helmholtz Centre for Environmental Research, Department Computational Landscape Ecology, 04318 Leipzig, Germany
2 iDiv - German Centre for Integrative Biodiversity Research, 04103 Leipzig, Germany
3 Department of Ecology and Evolution, Stony Brook University, Stony Brook, NY 11794, USA 
4 Institute of Biology/Geobotany and Botanical Garden, Martin-Luther-University Halle-Wittenberg, 06099 Halle (Saale), Germany
5  UFZ - Helmholtz Centre for Environmental Research, Department Community Ecology, 06120 Halle (Saale), Germany
6 Department of Life Sciences, Imperial College London, Silwood Park, SL5 7PY, UK.
7 Department of Life Sciences, Natural History Museum, Cromwell Road, London SW7 5BD, UK.
8 Environmental Geography Group, Department of Earth Sciences, Vrije Universiteit Amsterdam, de Boelelaan 1087, 1081 HV Amsterdam, The Netherlands
9 United Nations Environment Programme World Conservation Monitoring Centre, 219 Huntingdon Road, Cambridge CB3 0DL, UK. 
10 Centre for Biodiversity and Environment Research, Department of Genetics, Evolution and Environment, University College London, Gower Street, London WC1E 6BT, UK.

Acknowledgements. This work was supported by the National Socio-Environmental Synthesis Center (SESYNC; NSF DBI-1052875), the Helmholtz Centre for Environmental Research - UFZ and sDiv, the Synthesis Centre of the German Centre for Integrative Biodiversity Research (iDiv) Halle-Jena-Leipzig (DFG FZT 118). M.B. and S.K. are funded by the Helmholtz Research School for Ecosystem Services under Changing Land Use and Climate (ESCALATE, VH-KO-613). T.N. acknowledges funding from the UK Natural Environment Research Council (NE/J011193/1) and a Leverhulme Trust Research Project Grant.  K.G. received funding with the project GLUES from the German Federal Ministry of Education and Research (01LL0901A).  W.V. and P.V. are supported by OPERAs, funded within the EU 7th Framework Program (308393).  J.G. acknowledges support from the U.S. NSF project 1119891. This research contributes to the Global Land Project (www.globallandproject.org). We thank Kristin Powell, Chase Mendenhall for input to the conceptual design of the study; Wolfgang Viechtbauer for help with the conducted meta-analysis; Byron C. Wallace for support with text analysis; Jeff Kaplan for providing land-use history data; Tomás Václavík, Simon Attwood and Josef Settele for comments; Rachel Lorraine Lamb, Anna-Katharina Steinmetz and Marketa Václavíkova for support in paper screening.

Author contributions. M.B., R.S., S.Kl., P.H.V., T.N., M.W. and K.G. designed the project and the meta-analysis; M.B., K.G., W.V., H.R.P.P., S.C. and T.N. designed the data-collection protocols and database; M.B., W.V., K.G. H.R.P.P., S.C., S.Ka., N.K. and M.A-F. collated the data; K.G., J.G, N.K., T.N. designed the statistical analysis; K.G., H.R.P.P. and M.B. performed the analyses; M.B., R.S., T.N. and K.G. wrote the manuscript with contributions from H.R.P.P., S.Kl., W.V., S.C., S.Ka., N.K., J.G., P.H.V., M.W. and M.A-F.

Overall structure of the modules is:  
00 00_initialize_directories.r  
00.1. set the working and temporary directories  
00.2. source all relevant R scripts  
  
01 01_load_libraries_and_functions.r  
01.1. load all libraries needed for subsequent analysis  
01.2. load all self-written functions needed for subsequent analysis  
  
02 02_load_table_directly_from_google.R  
02.1. load data directly from google docs  
02.2. adapt data structure  
  
03 03_ImputeMissingData_mice_mean.r  
03.1. impute missing data using the mice package and the mean of 50 imputation chains  
03.2. plot imputed missing data  
  
04 04_CompileESframe.R  
04.1. Compile ES frame  
04.2. Calculate response ratio effect sizes  
  
05 05_AddMapDataToESframe.R  
05.1. Intersect studies with main climate zones  
05.2. Intersect studies with coarse classes of land-use history  
  
06 06_DataPreparation4Analysis.R  
06.1. Remove cases with zero variances, pseudo-replicates, redundant LUI.range.level comparisons   
06.2. remove columns not needed for the analysis, unify names  
06.3. save rawdata as table in a word doc  
  
07 07_DescriptiveStatsOfESFrame.Rmd  
07.1. Protocol structure and summary of variables in the ES.frames for richness and yield  
07.2. Plot Histograms of all variables in the ES.frame   
07.3. Plot maps of added data  
07.4. Plot map of studies  
07.5. Forest Plots of study.cases per LUI.range.level  
  
08 08_DataAnalysis.R  
08.1. define list of moderators  
08.2. Fit models for richness and yield  
08.3. extract fit statistics  
  
09 09.1_Plot_model_coeffs.r  
09.1.1. plot raw data + grand mean  
09.1.2. plot LUI cross diagrams  
  
09 09.2_Plot_LUI.SGP.crossDiagrams.r  
09.2.1. Predictions for richness  
09.2.2. Predictions for yield  
09.2.3. Join and save predictions for richness and yield  
09.2.4. Map predictions facetted by Product and/or Species.Group  
  
09 09.3_Preds_Full.r  
09.3.1. Predictions for richness for model "Full"  
09.3.2. Predictions for yield for model "Full"  
09.3.3. join and save predictions for richness and yield for models "Full"  
  
