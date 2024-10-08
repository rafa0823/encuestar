




#' Analizar black box
#'
#' @param bd
#' @param vars
#' @param stimuli
#'
#' @return
#'
#' @examples
analizar_blackbox_1d <- function(bd, vars, stimuli){
  stmli <- bd %>% select(stimuli =all_of(stimuli))
  basic <- bd %>% select(all_of(vars))
  nas <- basic %>% mutate(across(everything(), ~if_else(is.na(.x), 9999,.x)))

  issue <- basicspace::blackbox(nas, missing=c(9999),verbose=FALSE,dims=1,minscale=ncol(nas)/2+1)

  individuals <- issue$individuals %>% pluck(1) %>% as_tibble %>%
    bind_cols(stmli)

  orden <- individuals %>% group_by(stimuli = all_of(stimuli)) %>%
    summarise(media = mean(c1,na.rm = T)) %>% arrange(desc(media))

  individuals <- individuals %>% mutate(stimuli = factor(x = stimuli,levels =  orden %>% pull(stimuli)), stimuli*-1)
  return(
    list(
      stimuli = issue$stimuli %>% pluck(1),
      fits = issue$fits,
      individuals = individuals,
      slf = orden
    )
  )
}



#' Graficar metodo black box
#'
#' @param lst
#'
#' @return
#'
#' @examples
graficar_blackbox_1d <- function(lst){
  print(lst$stimuli)
  print(lst$slf)

  lst$individuals %>%# mutate(c1 =c1*-1) %>%
    ggplot(aes(x = c1)) +
    geom_density(color = "#871938") +
    facet_wrap(~stimuli) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(subtitle = glue::glue("Explica el {scales::percent(lst$fits$percent/100)} de la varianza total"))+
    theme_minimal()+
    theme(   # legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(color  = "#A0B12F"),
      panel.grid.major.y = element_line(linetype = "dotted"),
      # axis.text = element_blank(),
      text = element_text(family = "Poppins", size=14))
}





encuesta_demo$muestra$diseno$variables<-




