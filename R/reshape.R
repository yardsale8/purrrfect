reshape_and_transform <- function(.data,
                                  .outcome_col = .outcome,
                                  .trial_col = .trial,
                                  .repl_col = .replication,
                                  .reshape = NULL,
                                  names_sep = '.',
                                  simplify = TRUE,
                                  strict = FALSE,
                                  keep_empty = FALSE,
                                  names_repair = "check_unique",
                                  ptype = NULL,
                                  transform = NULL) {

  if (is.null(.reshape) & is.null(transform)) {
    .data
  } else if (is.null(.reshape)) {
    (.data
     %>% dplyr::mutate("{{.outcome_col}}" := purrr::map({{.outcome_col}}, \(x) purrr::map_vec(x, transform))))
  } else if (!is.null(.reshape) & .reshape == 'split') {
    (.data
     %>% tidyr::unnest_wider({{.outcome_col}},
                             names_sep = names_sep,
                             simplify = simplify,
                             strict = strict,
                             names_repair = names_repair,
                             ptype = ptype,
                             transform = transform)
    )
  } else if (!is.null(.reshape) & .reshape == 'stack') {
    (.data
     %>% tidyr::unnest_longer({{.outcome_col}},
                              keep_empty = keep_empty,
                              names_repair = names_repair,
                              simplify = simplify,
                              ptype = ptype,
                              transform = transform)
     %>% dplyr::group_by({{.trial_col}})
     %>% dplyr::mutate("{{.repl_col}}" := row_number())
     %>% dplyr::relocate({{.repl_col}}, {{.outcome_col}}, .after = {{.trial_col}})
     %>% dplyr::ungroup())

  } else {.data}
}
