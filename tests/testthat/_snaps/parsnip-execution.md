# parsnip model execution

    Code
      fit_reg
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2ORegressionModel: drf
      Model ID:  DRF_model_R_1654706510170_346 
      Model Summary: 
        number_of_trees number_of_internal_trees model_size_in_bytes min_depth
      1               5                        5                1293         6
        max_depth mean_depth min_leaves max_leaves mean_leaves
      1         7    6.40000         12         18    16.00000
      
      
      H2ORegressionMetrics: drf
      ** Reported on training data. **
      ** Metrics reported on Out-Of-Bag training samples **
      
      MSE:  7.319999
      RMSE:  2.70555
      MAE:  2.221337
      RMSLE:  0.1422219
      Mean Residual Deviance :  7.319999
      
      
      
      

---

    Code
      fit_class
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2OBinomialModel: drf
      Model ID:  DRF_model_R_1654706510170_347 
      Model Summary: 
        number_of_trees number_of_internal_trees model_size_in_bytes min_depth
      1              20                       20               32033        14
        max_depth mean_depth min_leaves max_leaves mean_leaves
      1        18   15.40000        107        134   122.95000
      
      
      H2OBinomialMetrics: drf
      ** Reported on training data. **
      ** Metrics reported on Out-Of-Bag training samples **
      
      MSE:  0.1807182
      RMSE:  0.4251096
      LogLoss:  3.004796
      Mean Per-Class Error:  0.2360438
      AUC:  0.8217172
      AUCPR:  0.7650262
      Gini:  0.6434343
      R^2:  0.2690795
      
      Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
             Class1 Class2    Error      Rate
      Class1    306    131 0.299771  =131/437
      Class2     61    293 0.172316   =61/354
      Totals    367    424 0.242731  =192/791
      
      Maximum Metrics: Maximum metrics at their respective thresholds
                              metric threshold      value idx
      1                       max f1  0.300000   0.753213  29
      2                       max f2  0.125000   0.823893  37
      3                 max f0point5  0.714286   0.749665  12
      4                 max accuracy  0.600000   0.766119  18
      5                max precision  1.000000   0.835165   0
      6                   max recall  0.000000   1.000000  43
      7              max specificity  1.000000   0.931350   0
      8             max absolute_mcc  0.375000   0.531626  27
      9   max min_per_class_accuracy  0.461538   0.759887  22
      10 max mean_per_class_accuracy  0.375000   0.767263  27
      11                     max tns  1.000000 407.000000   0
      12                     max fns  1.000000 202.000000   0
      13                     max fps  0.000000 437.000000  43
      14                     max tps  0.000000 354.000000  43
      15                     max tnr  1.000000   0.931350   0
      16                     max fnr  1.000000   0.570621   0
      17                     max fpr  0.000000   1.000000  43
      18                     max tpr  0.000000   1.000000  43
      
      Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
      
      

---

    Code
      fit_reg
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2ORegressionModel: glm
      Model ID:  GLM_model_R_1654706510170_368 
      GLM Model: summary
          family     link                              regularization
      1 gaussian identity Elastic Net (alpha = 0.5, lambda = 1.0132 )
        number_of_predictors_total number_of_active_predictors number_of_iterations
      1                         10                           9                    1
           training_frame
      1 object_gabwknrsau
      
      Coefficients: glm coefficients
             names coefficients standardized_coefficients
      1  Intercept    26.298144                 20.090625
      2        cyl    -0.447375                 -0.798977
      3       disp    -0.005674                 -0.703231
      4         hp    -0.011042                 -0.757065
      5       drat     0.859638                  0.459630
      6         wt    -1.185114                 -1.159584
      7       qsec     0.000000                  0.000000
      8         vs     0.655750                  0.330509
      9         am     1.116929                  0.557338
      10      gear     0.123540                  0.091148
      11      carb    -0.350465                 -0.566071
      
      H2ORegressionMetrics: glm
      ** Reported on training data. **
      
      MSE:  6.511253
      RMSE:  2.551716
      MAE:  2.00629
      RMSLE:  0.113459
      Mean Residual Deviance :  6.511253
      R^2 :  0.8149633
      Null Deviance :1126.047
      Null D.o.F. :31
      Residual Deviance :208.3601
      Residual D.o.F. :22
      AIC :172.7651
      
      
      
      

---

    Code
      fit_class
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2OBinomialModel: glm
      Model ID:  GLM_model_R_1654706510170_369 
      GLM Model: summary
          family  link                                regularization
      1 binomial logit Elastic Net (alpha = 0.5, lambda = 6.158E-4 )
        number_of_predictors_total number_of_active_predictors number_of_iterations
      1                          2                           2                    4
           training_frame
      1 object_gabwknrsau
      
      Coefficients: glm coefficients
            names coefficients standardized_coefficients
      1 Intercept    -3.718107                 -0.343888
      2         A    -1.188186                 -1.066341
      3         B     3.753274                  2.741622
      
      H2OBinomialMetrics: glm
      ** Reported on training data. **
      
      MSE:  0.1310579
      RMSE:  0.3620192
      LogLoss:  0.426057
      Mean Per-Class Error:  0.1741328
      AUC:  0.8881757
      AUCPR:  0.8512458
      Gini:  0.7763513
      R^2:  0.4699321
      Residual Deviance:  674.0222
      AIC:  680.0222
      
      Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
             Class1 Class2    Error      Rate
      Class1    349     88 0.201373   =88/437
      Class2     52    302 0.146893   =52/354
      Totals    401    390 0.176991  =140/791
      
      Maximum Metrics: Maximum metrics at their respective thresholds
                              metric threshold      value idx
      1                       max f1  0.408943   0.811828 218
      2                       max f2  0.233480   0.867855 280
      3                 max f0point5  0.571023   0.817728 162
      4                 max accuracy  0.507338   0.824273 187
      5                max precision  0.997350   1.000000   0
      6                   max recall  0.010114   1.000000 394
      7              max specificity  0.997350   1.000000   0
      8             max absolute_mcc  0.408943   0.648199 218
      9   max min_per_class_accuracy  0.456629   0.821510 204
      10 max mean_per_class_accuracy  0.408943   0.825867 218
      11                     max tns  0.997350 437.000000   0
      12                     max fns  0.997350 352.000000   0
      13                     max fps  0.001832 437.000000 399
      14                     max tps  0.010114 354.000000 394
      15                     max tnr  0.997350   1.000000   0
      16                     max fnr  0.997350   0.994350   0
      17                     max fpr  0.001832   1.000000 399
      18                     max tpr  0.010114   1.000000 394
      
      Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
      
      

---

    Code
      fit_class
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2OMultinomialModel: glm
      Model ID:  GLM_model_R_1654706510170_371 
      GLM Model: summary
             family        link                                regularization
      1 multinomial multinomial Elastic Net (alpha = 0.5, lambda = 8.671E-4 )
        number_of_predictors_total number_of_active_predictors number_of_iterations
      1                         15                          11                   26
           training_frame
      1 object_gabwknrsau
      
      Coefficients: glm multinomial coefficients
               names coefs_class_0 coefs_class_1 coefs_class_2 std_coefs_class_0
      1    Intercept      6.496784     -0.408705    -18.679065         -3.969984
      2 Sepal.Length     -1.522256      1.109945      0.000000         -1.260529
      3  Sepal.Width      3.757638     -0.684351     -3.575675          1.637828
      4 Petal.Length     -2.059447     -0.596759      3.326552         -3.635537
      5  Petal.Width     -4.436355     -1.466536      7.098211         -3.381557
        std_coefs_class_1 std_coefs_class_2
      1         -0.016699         -8.596793
      2          0.919108          0.000000
      3         -0.298285         -1.558516
      4         -1.053457          5.872356
      5         -1.117849          5.410524
      
      H2OMultinomialMetrics: glm
      ** Reported on training data. **
      
      Training Set Metrics: 
      =====================
      
      Extract training frame with `h2o.getFrame("object_gabwknrsau")`
      MSE: (Extract with `h2o.mse`) 0.01469843
      RMSE: (Extract with `h2o.rmse`) 0.1212371
      Logloss: (Extract with `h2o.logloss`) 0.05728744
      Mean Per-Class Error: 0.02
      AUC: (Extract with `h2o.auc`) NaN
      AUCPR: (Extract with `h2o.aucpr`) NaN
      Null Deviance: (Extract with `h2o.nulldeviance`) 329.5837
      Residual Deviance: (Extract with `h2o.residual_deviance`) 17.18623
      R^2: (Extract with `h2o.r2`) 0.9779524
      AIC: (Extract with `h2o.aic`) NaN
      Confusion Matrix: Extract with `h2o.confusionMatrix(<model>,train = TRUE)`)
      =========================================================================
      Confusion Matrix: Row labels: Actual class; Column labels: Predicted class
                 setosa versicolor virginica  Error      Rate
      setosa         50          0         0 0.0000 =  0 / 50
      versicolor      0         48         2 0.0400 =  2 / 50
      virginica       0          1        49 0.0200 =  1 / 50
      Totals         50         49        51 0.0200 = 3 / 150
      
      Hit Ratio Table: Extract with `h2o.hit_ratio_table(<model>,train = TRUE)`
      =======================================================================
      Top-3 Hit Ratios: 
        k hit_ratio
      1 1  0.980000
      2 2  1.000000
      3 3  1.000000
      
      
      
      
      
      

---

    Code
      fit_class
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2OBinomialModel: naivebayes
      Model ID:  NaiveBayes_model_R_1654706510170_372 
      Model Summary: 
        number_of_response_levels min_apriori_probability max_apriori_probability
      1                         2                 0.44767                 0.55233
      
      
      H2OBinomialMetrics: naivebayes
      ** Reported on training data. **
      
      MSE:  0.1736824
      RMSE:  0.4167522
      LogLoss:  0.5468578
      Mean Per-Class Error:  0.2362021
      AUC:  0.837826
      AUCPR:  0.788381
      Gini:  0.6756519
      
      Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
             Class1 Class2    Error      Rate
      Class1    275    162 0.370709  =162/437
      Class2     36    318 0.101695   =36/354
      Totals    311    480 0.250316  =198/791
      
      Maximum Metrics: Maximum metrics at their respective thresholds
                              metric threshold      value idx
      1                       max f1  0.176050   0.762590 285
      2                       max f2  0.134438   0.851833 306
      3                 max f0point5  0.501768   0.731272 178
      4                 max accuracy  0.283282   0.764855 246
      5                max precision  0.999703   1.000000   0
      6                   max recall  0.021083   1.000000 389
      7              max specificity  0.999703   1.000000   0
      8             max absolute_mcc  0.234062   0.540376 262
      9   max min_per_class_accuracy  0.402888   0.759887 210
      10 max mean_per_class_accuracy  0.283282   0.771089 246
      11                     max tns  0.999703 437.000000   0
      12                     max fns  0.999703 350.000000   0
      13                     max fps  0.006373 437.000000 399
      14                     max tps  0.021083 354.000000 389
      15                     max tnr  0.999703   1.000000   0
      16                     max fnr  0.999703   0.988701   0
      17                     max fpr  0.006373   1.000000 399
      18                     max tpr  0.021083   1.000000 389
      
      Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
      
      

---

    Code
      fit_reg
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2ORegressionModel: rulefit
      Model ID:  RuleFit_model_R_1654706510170_374 
      Rulefit Model Summary: 
          family     link           regularization number_of_predictors_total
      1 gaussian identity Lasso (lambda = 0.5066 )                       3335
        number_of_active_predictors number_of_iterations rule_ensemble_size
      1                          19                    1               3325
        number_of_trees number_of_internal_trees min_depth max_depth mean_depth
      1             300                      300         0         5    4.00000
        min_leaves max_leaves mean_leaves
      1          0         20    11.08333
      
      
      H2ORegressionMetrics: rulefit
      ** Reported on training data. **
      
      MSE:  4.922215
      RMSE:  2.218606
      MAE:  1.74856
      RMSLE:  0.1013864
      Mean Residual Deviance :  4.922215
      
      
      
      

---

    Code
      fit_class
    Output
      parsnip model object
      
      Model Details:
      ==============
      
      H2OBinomialModel: rulefit
      Model ID:  RuleFit_model_R_1654706510170_379 
      Rulefit Model Summary: 
          family  link            regularization number_of_predictors_total
      1 binomial logit Lasso (lambda = 0.03079 )                       4680
        number_of_active_predictors number_of_iterations rule_ensemble_size
      1                           3                    4               4678
        number_of_trees number_of_internal_trees min_depth max_depth mean_depth
      1             300                      300         0         5    4.00000
        min_leaves max_leaves mean_leaves
      1          0         30    15.59333
      
      
      H2OBinomialMetrics: rulefit
      ** Reported on training data. **
      
      MSE:  0.1415051
      RMSE:  0.3761716
      LogLoss:  0.4478343
      Mean Per-Class Error:  0.1873812
      AUC:  0.8776229
      AUCPR:  0.8374058
      Gini:  0.7552457
      
      Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
             Class1 Class2    Error      Rate
      Class1    351     86 0.196796   =86/437
      Class2     63    291 0.177966   =63/354
      Totals    414    377 0.188369  =149/791
      
      Maximum Metrics: Maximum metrics at their respective thresholds
                              metric threshold      value idx
      1                       max f1  0.499485   0.796170 204
      2                       max f2  0.227474   0.862248 285
      3                 max f0point5  0.629647   0.804438 150
      4                 max accuracy  0.541013   0.811631 189
      5                max precision  0.980020   1.000000   0
      6                   max recall  0.053126   1.000000 396
      7              max specificity  0.980020   1.000000   0
      8             max absolute_mcc  0.499485   0.622467 204
      9   max min_per_class_accuracy  0.516475   0.807910 199
      10 max mean_per_class_accuracy  0.499485   0.812619 204
      11                     max tns  0.980020 437.000000   0
      12                     max fns  0.980020 353.000000   0
      13                     max fps  0.044458 437.000000 399
      14                     max tps  0.053126 354.000000 396
      15                     max tnr  0.980020   1.000000   0
      16                     max fnr  0.980020   0.997175   0
      17                     max fpr  0.044458   1.000000 399
      18                     max tpr  0.053126   1.000000 396
      
      Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
      
      

