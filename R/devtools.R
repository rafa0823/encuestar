crear_formula_nombre <- function(bd, prefijo){
  res <- bd %>% select(starts_with(prefijo)) %>%
    names() %>%
    stringr::str_c(collapse = " + ") %>%
    rlang::parse_expr() %>% rlang::new_formula(lhs=NULL, rhs=.)
}
