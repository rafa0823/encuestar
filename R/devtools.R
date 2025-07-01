crear_formula_nombre <- function(bd, prefijo){
  res <- bd %>%
    select(starts_with(prefijo)) %>%
    names() %>%
    sort()
   res<-  res %>%
    stringr::str_c(collapse = " + ") %>%
    rlang::parse_expr() %>%
    rlang::new_formula(lhs=NULL, rhs=.)
}

comparar_disenos <-  function(disenos,
                              variables, valor_variables, vartype){


  variables <- enquos(variables)

  cs <- disenos |>
    imap(~{
      .x |> srvyr::as_survey_design() |>
        summarise(across(!!!variables,
                         ~ srvyr::survey_mean(.x == !!valor_variables, vartype = vartype, na.rm = TRUE),
                         .names = "{.col}")) |>
        mutate(diseño=.y) |>
        pivot_longer(cols = -diseño) |>
        mutate(separar = ifelse(stringr::str_detect(name, glue::glue('_{vartype}$')),
                                vartype, "mean"),
               name=stringr::str_remove(name, glue::glue('_{vartype}'))) |>
        tidyr::pivot_wider(names_from = separar, values_from = value)
    })



  c1 <-  bind_rows(cs) |>
    ggplot(aes(x=name, y=mean, fill=diseño,
               ymin = mean-!!ensym(vartype), ymax = mean+!!ensym(vartype))) +
    ggchicklet::geom_chicklet(width = 0.6,alpha=0.9,
                              position = "dodge") +
    ggfittext::geom_bar_text(aes(label = scales::percent(mean, accuracy=.1)),
                             color="black",
                             position = "dodge", reflow = TRUE, vjust = -0.5) +
    geom_linerange(linetype="solid", color="black", linewidth=.5,
                   position = position_dodge(width = 0.6)) +
    scale_y_continuous(labels=scales::percent)  +
    theme_bw() +
    theme(legend.position = "top")

  return(c1)


}
