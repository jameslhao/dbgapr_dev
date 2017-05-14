## ---- eval=F, echo=T-----------------------------------------------------
#  ?dbgapr

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  prjConfig(c, prjDir="/home/foo/myprj")

## ---- eval=F, echo=T-----------------------------------------------------
#  prjConfig(c, prjDir="/home/foo/myprj")        # all platforms
#  prjConfig(c, prjDir="~/myprj")                # all platforms
#  prjConfig(c, prjDir="C:/Users/foo/myprj")     # Windows
#  prjConfig(c, prjDir="C:\\Users\\foo\\myprj")  # Windows

## ---- eval=F, echo=T-----------------------------------------------------
#  dir("~/myprj")

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  getPrjDir(c, showErr=TRUE)

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  prepareData(c, userDataDir='/home/foo/my_data')

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  prepareData(c, userDataDir='/home/foo/my_data', phsAcc='phs000001.v3.p1')

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  searchCopyPhenoFiles(c, userDataDir='~/my_data')
#  # or to only copy the files of a specific study
#  searchCopyPhenoFiles(c, userDataDir='~/my_data', phsAcc="phs000001.v3.p1")

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  searchCopyPhenoFiles(c, userDataDir='~/my_data', copyOrMove = 'move')

## ---- eval=F, echo=T, tidy=TRUE, tidy.opts=list(width.cutoff=60)---------
#  dir("~/myprj/gapwork/data/phs000001/phs000001.v3/original")

## ---- eval=F, echo=T, tidy=TRUE, tidy.opts=list(width.cutoff=60)---------
#  dir("~/myprj/gapwork/data")

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  ftpDownload(c)
#  # or to limit the download to a specific study
#  ftpDownload(c, phsAcc="phs000001.v3.p1")

## ---- eval=F, echo=T, tidy=TRUE, tidy.opts=list(width.cutoff=60)---------
#  dir("~/.dbgapr/supplemental_data")

## ---- eval=F, echo=T, tidy=TRUE, tidy.opts=list(width.cutoff=60)---------
#  dir("~/myprj/gapwork/data/phs000001/phs000001.v3/supplemental_data")

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  recordPrjFileInfo(c)
#  # or to process only specific study
#  recordPrjFileInfo(c, phsAcc="phs000001.v3.p1")

## ---- eval=F, echo=T-----------------------------------------------------
#  file.show("~/.dbgapr/pht_file_info_indiv.json")

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  mergeDatasetConsent(c)
#  # or to only merged the files of a specific study
#  mergeDatasetConsent(c, phsAcc='phs000001.v3.p1')

## ---- eval=F, echo=T-----------------------------------------------------
#  dir("~/myprj/gapwork/data/phs000001/phs000001.v3/original")

## ---- eval=F, echo=T-----------------------------------------------------
#  dir("~/myprj/gapwork/data/phs000001/phs000001.v3/combined")

## ---- eval=F, echo=T-----------------------------------------------------
#  dir("~/myprj/gapwork/data/phs000001/phs000001.v3/combined/log")
#  

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- accInfo(c, acc='phs000001.v3.p1')    # study

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- accInfo(c, acc='pht000370.v2')       # dataset

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- accInfo(c, acc='phv00000084.v2')    # variable of integer type

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- accInfo(c, acc='phv00000031.v2')    # varible of enum_integer type

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- getAllStudyInfo(c)
#  # or
#  # display result as a table
#  df <- getAllStudyInfo(c, showAs='table')
#  # or
#  # display result in json with notepad
#  df <- getAllStudyInfo(c, showAs='json', editor='notepad')

## ---- eval=F, echo=T-----------------------------------------------------
#  c <- Commons()
#  df <- getAllDatasetInfo(c)
#  # or
#  # display result as a table
#  df <-  getAllDatasetInfo(c, showAs='table')
#  # or
#  # display result in json format with notepad
#  df <- getAllDatasetInfo(c, showAs='json', editor='notepad')
#  # or
#  # display result in a text format
#  df <-  getAllDatasetInfo(c, showAs='text')

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  # all study variables
#  df <- getStudyVariableInfo(s)
#  # or
#  # display result as a table
#  df <- getStudyVariableInfo(s, showAs='table')             ·
#  # or
#  # all dataset variables
#  df <- getStudyVariableInfo(s, phtAcc='pht000370.v2.p1')
#  # or
#  # all dataset variables of numerical type
#  df <- getStudyVariableInfo(s, phtAcc='pht000370.v2.p1', dataType = 'num')
#  # or
#  # a list of variables ·
#  accList = c('phv00054119.v1.p1.c2', 'phv00054118.v1.p1')
#  df <- getStudyVariableInfo(s, phvAccList=accList)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  
#  ##### Search for matches in variable descriptions #####
#  # define search terms
#  t1 = c('Diabetes Treatment', 'Smoking Status')
#  t2 = c('Year 10')
#  t3 = c('6 Months')
#  # term match with 'Diabetes Treatment' OR 'Smoking Status'
#  df <- getPhvAccListByTerms(s, terms_1=t1)
#  # or
#  #  display matching variable in a table
#  df <- getPhvAccListByTerms(s, terms_1=t1, showTable=T)
#  # or
#  # term match with ('Diabetes Treatment' OR 'Smoking Status') AND 'Year 10'
#  df <- getPhvAccListByTerms(s, terms_1=t1, terms_2=t2, showTable=T)
#  # or
#  # term match with ('Diabetes Treatment' OR 'Smoking Status') AND 'Year 10' AND '6 Months'
#  df <- getPhvAccListByTerms(s, terms_1=t1, terms_2=t2, terms_3=t3, showTable=T)
#  
#  ##### Search for matches in variable names #####
#  t4 = 'SMK'
#  t5 = 'AGE'
#  # term match with 'SMK'
#  df <- getPhvAccListByTerms(s, terms_1=t4, searchIn='name', showTable=T)
#  # or
#  # term match with 'SMK' AND 'AGE'
#  df <- getPhvAccListByTerms(s, terms_1=t4, terms_2=t5, searchIn='name', showTable=T)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc = 'phs000001.v3.p1')
#  # get data of variables in a dataset
#  df <- getStudyVariableData(s, phtAcc='pht000370.v2.p1')
#  # or
#  # get data of a list of variables
#  # acc_list = c('phv00054119.v1.p1.c2', 'phv00053733.v2'))
#  df <- getStudyVariableData(s, phvAccList=acc_list)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc = 'phs000001.v3.p1')
#  # variable data of a set of specified subjects in a dataset
#  subj_ids = c(219, 220, 221)
#  df <- getDatasetDataByPhtAcc(s, phtAcc='pht000371.v2', subjIdsOrFile=subj_ids)
#  # or
#  # variable data of a set of specified subjects in a dataset
#  # The subject id file is a plain text file with one dbGaP_Subject_ID per line.
#  subj_file = "~/my_data/subj_ids.txt"
#  df <- getDatasetDataByPhtAcc(s, phtAcc='pht000371.v2', subjIdsOrFile=subj_file)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc = 'phs000001.v3.p1')
#  df <- getStudyVariableData(s, phtAcc='pht000370.v2.p1')
#  # return column names
#  colnames(df)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc = 'phs000001.v3.p1')
#  df <- getStudyVariableData(s, phtAcc='pht000370.v2.p1')
#  # return column values
#  head(subset(df, select=c('Submitted_Subject_ID', 'ID2')))

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  df <- getStudyVariableData(s, phtAcc='pht000370.v2.p1', colNameWithAcc=TRUE)
#  # return column names
#  colnames(df)

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  df <- variableSummary(s, phvAcc='phv00054119.v1.p1.c2')   # numerical

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  df <- variableSummary(s, phvAcc='phv00000035.v2')         # categorical

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  df <- variableBoxplot(s, numPhvAcc='phv00000027.v2', catPhvAcc='phv00000032.v2')

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/boxplot_phv00000027.v2_AGEPHOT_phv00000032.v2_MARITAL.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  num_1 = 'phv00000027.v2'
#  num_2 = 'phv00053747.v2'
#  cat_1 = 'phv00053757.v2'
#  df <- variableScatterplot(s, numPhvAcc_1=num_1, numPhvAcc_2=num_2, catPhvAcc=cat_1)

## ---- out.width = "600px", out.height = "500px", echo=FALSE--------------
knitr::include_graphics("figure/scatterplot_phv00000027.v2_AGEPHOT_phv00053747.v2_LNUCSCORE_phv00053757.v2_RNUC.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  # with density plot
#  df <- variableHistogram(s, phvAcc='phv00053747.v2')
#  # or
#  # without Density plot·
#  df <- variableHistogram(s, phvAcc='phv00053747.v2', withDensity=F)

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/histogram_phv00053747.v2_LNUCSCORE.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  # with density plot
#  df <- variableHistogram(s, phvAcc='phv00053733.v2')

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/barchart_phv00053733.v2_ADVSTAT.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  df <- variablePiechart(s, catPhvAcc='phv00053764.v2')

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/boxplot_phv00053764.v2_LPSC.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  acc_list = c('phv00054139.v1', 'phv00053796.v2', 'phv00000089.v2')
#  df <- variableVenndiagram(s, phvAccList=acc_list)

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/VennDiagram_AGECT_phv00054139.v1_DT_PROA_phv00053796.v2_SYST07_phv00000089.v2.png")

## ---- eval=F, echo=T-----------------------------------------------------
#  s <- Study(phsAcc='phs000001.v3.p1')
#  
#  phvAcc = 'phv00000031.v2'
#  cat_acc = 'phv00000035.v2'
#  # or
#  num_acc_list = c('phv00000007.v2', 'phv00053794.v2', 'phv00053786.v2', 'phv00000106.v2')
#  df <- variableCorrelHeatmap(s, catPhvAcc=catPhvAcc, numPhvAccList=numPhvAcc_list cc_list))

## ---- out.width = "600px", echo=FALSE------------------------------------
knitr::include_graphics("figure/correlmatrix_SCHOOL_phv00000035.v2_RCORBASE_phv00000007.v2_DT_LYC_phv00053794.v2_DT_VITE_phv00053786.v2_DIAS12_phv00000106.v2.png")

