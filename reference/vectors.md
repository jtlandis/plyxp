# S7 classes for vctrs and S4 Vectors

A set of S7 classes and Class unions that help establish S7 method
dispatch. These classes were made to re-export several `vctrs` functions
such that internals for `plyxp` were consistent with room for
optimization.

## Usage

``` r
class_vctrs

class_s4_vctrs

class_DF
```

## Format

An object of class `S7_union` of length 1.

An object of class `classRepresentation` of length 1.

An object of class `classRepresentation` of length 1.

## Value

S7 class union or base class

## See also

[`vec_rep()`](https://jtlandis.github.io/plyxp/reference/vctrs-vec_rep.md),[`vec_recycle()`](https://jtlandis.github.io/plyxp/reference/vctrs-vec_recycle.md),[`vec_slice()`](https://jtlandis.github.io/plyxp/reference/vctrs_slice.md)

## Examples

``` r
# used for defining methods on S7 generics

S7::method(vec_slice, class_s4_vctrs)
#> <S7_method> method(vec_slice, Vector)
#> function (x, i, ...) 
#> {
#>     x[i]
#> }
#> <environment: namespace:plyxp>
```
