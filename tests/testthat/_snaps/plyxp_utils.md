# add_bind works for lazy binding

    Code
      print("first")
    Output
      [1] "first"
    Code
      env$Sepal.Width
    Output
      [1] "evaluating for Sepal.Width"
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      
    Code
      print("second")
    Output
      [1] "second"
    Code
      env$Sepal.Width
    Output
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      

# add_bind works for standard binding

    Code
      binding_func("Sepal.Width")
    Output
      [1] "evaluating for Sepal.Width"

---

    Code
      print("first")
    Output
      [1] "first"
    Code
      env$Sepal.Width
    Output
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      
    Code
      print("second")
    Output
      [1] "second"
    Code
      env$Sepal.Width
    Output
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      

# add_bind works for active binding

    Code
      print("first")
    Output
      [1] "first"
    Code
      env$Sepal.Width
    Output
      [1] "evaluating for Sepal.Width"
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      
    Code
      print("second")
    Output
      [1] "second"
    Code
      env$Sepal.Width
    Output
      [1] "evaluating for Sepal.Width"
      [[1]]
      [1] 3.5
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 3.2
      
      [[4]]
      [1] 3.1
      
      [[5]]
      [1] 3.6
      
      [[6]]
      [1] 3.9
      
      [[7]]
      [1] 3.4
      
      [[8]]
      [1] 3.4
      
      [[9]]
      [1] 2.9
      
      [[10]]
      [1] 3.1
      

