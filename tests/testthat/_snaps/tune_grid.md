# tune model only (with id)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics .notes           .predictions
        <list>            <chr> <list>   <list>           <list>      
      1 <split [632/159]> Fold1 <NULL>   <tibble [1 x 3]> <NULL>      
      2 <split [633/158]> Fold2 <NULL>   <tibble [1 x 3]> <NULL>      
      3 <split [633/158]> Fold3 <NULL>   <tibble [1 x 3]> <NULL>      
      4 <split [633/158]> Fold4 <NULL>   <tibble [1 x 3]> <NULL>      
      5 <split [633/158]> Fold5 <NULL>   <tibble [1 x 3]> <NULL>      
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...
      
      Run `show_notes(.Last.tune.result)` for more information.

# tune model only (without id)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics .notes           .predictions
        <list>            <chr> <list>   <list>           <list>      
      1 <split [632/159]> Fold1 <NULL>   <tibble [1 x 3]> <NULL>      
      2 <split [633/158]> Fold2 <NULL>   <tibble [1 x 3]> <NULL>      
      3 <split [633/158]> Fold3 <NULL>   <tibble [1 x 3]> <NULL>      
      4 <split [633/158]> Fold4 <NULL>   <tibble [1 x 3]> <NULL>      
      5 <split [633/158]> Fold5 <NULL>   <tibble [1 x 3]> <NULL>      
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...
      
      Run `show_notes(.Last.tune.result)` for more information.

# tune model only (with id and recipe)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics .notes           .predictions
        <list>            <chr> <list>   <list>           <list>      
      1 <split [632/159]> Fold1 <NULL>   <tibble [1 x 3]> <NULL>      
      2 <split [633/158]> Fold2 <NULL>   <tibble [1 x 3]> <NULL>      
      3 <split [633/158]> Fold3 <NULL>   <tibble [1 x 3]> <NULL>      
      4 <split [633/158]> Fold4 <NULL>   <tibble [1 x 3]> <NULL>      
      5 <split [633/158]> Fold5 <NULL>   <tibble [1 x 3]> <NULL>      
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...
      
      Run `show_notes(.Last.tune.result)` for more information.

# tune model and recipe

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics .notes           .predictions
        <list>            <chr> <list>   <list>           <list>      
      1 <split [632/159]> Fold1 <NULL>   <tibble [1 x 3]> <NULL>      
      2 <split [633/158]> Fold2 <NULL>   <tibble [1 x 3]> <NULL>      
      3 <split [633/158]> Fold3 <NULL>   <tibble [1 x 3]> <NULL>      
      4 <split [633/158]> Fold4 <NULL>   <tibble [1 x 3]> <NULL>      
      5 <split [633/158]> Fold5 <NULL>   <tibble [1 x 3]> <NULL>      
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...
      
      Run `show_notes(.Last.tune.result)` for more information.

# tune with backend options parallelism

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics .notes           .predictions
        <list>            <chr> <list>   <list>           <list>      
      1 <split [632/159]> Fold1 <NULL>   <tibble [1 x 3]> <NULL>      
      2 <split [633/158]> Fold2 <NULL>   <tibble [1 x 3]> <NULL>      
      3 <split [633/158]> Fold3 <NULL>   <tibble [1 x 3]> <NULL>      
      4 <split [633/158]> Fold4 <NULL>   <tibble [1 x 3]> <NULL>      
      5 <split [633/158]> Fold5 <NULL>   <tibble [1 x 3]> <NULL>      
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...   - Error(s) x1: Error in .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, u...
      
      Run `show_notes(.Last.tune.result)` for more information.

