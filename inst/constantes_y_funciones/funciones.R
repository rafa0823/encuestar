asignar_colores = function(tb_respuestas, partidos = T){
  tb_respuestas |>
    mutate(color = dplyr::case_when(grepl(pattern = "por MORENA, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Astiazarán", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "por PAN, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "por Movimiento Ciudadano, ", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "MORENA", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "Morena", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_morena,
                                    grepl(pattern = "PAN", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pan,
                                    grepl(pattern = "PRI", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pri,
                                    grepl(pattern = "PRD", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_prd,
                                    grepl(pattern = "PT", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pt,
                                    grepl(pattern = "PES", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pes,
                                    grepl(pattern = "Partido Verde", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "PVEM", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pvem,
                                    grepl(pattern = "Movimiento Ciudadano|MC", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_mc,
                                    grepl(pattern = "Fuerza por", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_fuerzaxmexico,
                                    grepl(pattern = "Otro", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "Ninguno", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Candidato independiente", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "no registrado", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_otro,
                                    grepl(pattern = "No iré a votar", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "Anular", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_ninguno,
                                    grepl(pattern = "No sabe", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "No contesta", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Ns/Nc", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_nsnc,
                                    grepl(pattern = "Chiapas Unido", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_chisunido,
                                    grepl(pattern = "Encuentro Solidario", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pencsolchis,
                                    grepl(pattern = "Mover a Chiapas", x = !!rlang::sym(names(tb_respuestas)[1])) ~ color_pmchis,
                                    T ~ color_general)) |>
    pull(color) |>
    purrr::set_names(tb_respuestas |>
                       pull())
}
