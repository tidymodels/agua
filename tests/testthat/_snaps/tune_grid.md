# tune model only (with id)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics          .notes           .predictions      
        <list>            <chr> <list>            <list>           <list>            
      1 <split [632/159]> Fold1 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [795 x 7]>
      2 <split [633/158]> Fold2 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      3 <split [633/158]> Fold3 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      4 <split [633/158]> Fold4 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      5 <split [633/158]> Fold5 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>

# tune model only (without id)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics          .notes           .predictions      
        <list>            <chr> <list>            <list>           <list>            
      1 <split [632/159]> Fold1 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [795 x 7]>
      2 <split [633/158]> Fold2 <tibble [0 x 5]>  <tibble [1 x 3]> <tibble [0 x 7]>  
      3 <split [633/158]> Fold3 <tibble [0 x 5]>  <tibble [1 x 3]> <tibble [0 x 7]>  
      4 <split [633/158]> Fold4 <tibble [0 x 5]>  <tibble [1 x 3]> <tibble [0 x 7]>  
      5 <split [633/158]> Fold5 <tibble [0 x 5]>  <tibble [1 x 3]> <tibble [0 x 7]>  
      
      There were issues with some computations:
      
        - Error(s) x1: Error in .h2o.__checkConnectionHealth(): H2O connection has been ...   - Error(s) x3: Error in .h2o.__checkConnectionHealth(): H2O connection has been ...
      
      Use `collect_notes(object)` for more information.

# tune model only (with id and recipe)

    Code
      res
    Output
      # Tuning results
      # 5-fold cross-validation 
      # A tibble: 5 x 5
        splits            id    .metrics          .notes           .predictions      
        <list>            <chr> <list>            <list>           <list>            
      1 <split [632/159]> Fold1 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [795 x 7]>
      2 <split [633/158]> Fold2 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      3 <split [633/158]> Fold3 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      4 <split [633/158]> Fold4 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>
      5 <split [633/158]> Fold5 <tibble [10 x 5]> <tibble [0 x 3]> <tibble [790 x 7]>

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
      
        - Error(s) x1: Error in .h2o.__checkConnectionHealth(): H2O connection has been ...   - Error(s) x4: Error in .h2o.__checkConnectionHealth(): H2O connection has been ...
      
      Use `collect_notes(object)` for more information.

