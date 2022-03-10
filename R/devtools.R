crear_formula_nombre <- function(bd, prefijo){
  res <- bd %>%
    select(starts_with(prefijo)) %>%
    names() %>%
    sort()
   res<-  c(res, res[1])[-1] %>%
    stringr::str_c(collapse = " + ") %>%
    rlang::parse_expr() %>%
    rlang::new_formula(lhs=NULL, rhs=.)
}
