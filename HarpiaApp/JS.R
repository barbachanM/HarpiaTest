source_python('JSdivergence.py')

JS_R <- function(tpm1, tpm2){
  JS_out = JS_div(tpm1,tpm2)
  output <- list(
    JS_df = JS_out[1][[1]]
  )
  output
}
