# aggregate_votes.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

get_votes_sum = function(votes, position, turno, with_blank_null) {
  if (turno == 'latest') {
    turno = max(votes$NUM_TURNO)
  }

  if (!with_blank_null) {
    votes = votes %>%
      dplyr::filter(!(NUMERO_CANDIDATO %in% c(95, 96)))
  }

  votes %>%
    dplyr::filter(NUM_TURNO == turno) %>%
    dplyr::filter(CODIGO_CARGO == position) %>%
    dplyr::group_by(ANO_ELEICAO, NUM_ZONA, NUM_SECAO) %>%
    dplyr::mutate(total = sum(QTDE_VOTOS)) %>%
    dplyr::group_by(NUMERO_CANDIDATO, NUM_TURNO, .add = T) %>%
    dplyr::summarize(
      abs_votes = sum(QTDE_VOTOS),
      cand = sum(QTDE_VOTOS) / dplyr::first(total)
    ) %>%
    dplyr::ungroup()
}

#' @export
aggregate_majoritarian = function(votes, position, turno, party_number, with_blank_null) {
  votes_sum = get_votes_sum(votes, position, turno, with_blank_null)

  if (party_number == 'winner') {
    party_number = votes_sum %>%
      dplyr::group_by(NUMERO_CANDIDATO) %>%
      dplyr::summarize(abs_votes = sum(abs_votes)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(abs_votes == max(abs_votes)) %>%
      dplyr::pull(NUMERO_CANDIDATO)
  }

  votes_sum %>%
    dplyr::filter(NUMERO_CANDIDATO == party_number)
}

#' @export
aggregate_all_candidates = function(votes, position, turno, with_blank_null) {
  votes_sum = get_votes_sum(votes, position, turno, with_blank_null)

  votes_sum %>%
    tidyr::pivot_wider(names_from = NUMERO_CANDIDATO, values_from = c(cand, abs_votes))
}

#' @export
aggregate_all_candidates_vertical = function(votes, position, turno, with_blank_null) {
  get_votes_sum(votes, position, turno, with_blank_null)
}

#' @export
aggregate_all_parties = function(votes, position, turno, with_blank_null) {
  get_votes_sum(votes, position, turno, with_blank_null) %>%
    dplyr::mutate(NUMERO_CANDIDATO = stringr::str_sub(NUMERO_CANDIDATO, 1, 2)) %>%
    dplyr::group_by(ANO_ELEICAO, NUM_ZONA, NUM_SECAO, NUM_TURNO, NUMERO_CANDIDATO) %>%
    dplyr::summarize(cand = sum(cand), abs_votes = sum(abs_votes)) %>%
    tidyr::pivot_wider(names_from = NUMERO_CANDIDATO, values_from = c(cand, abs_votes))
}

aggregate_minoritarian_ideology = function(votes, positions) {
  votes %>%
    dplyr::filter(CODIGO_CARGO %in% positions) %>%
    dplyr::mutate(party = as.numeric(stringr::str_sub(NUMERO_CANDIDATO, end = 2))) %>%
    dplyr::group_by(NUM_ZONA, NUM_SECAO) %>%
    dplyr::mutate(total = sum(QTDE_VOTOS)) %>%
    dplyr::group_by(party, .add = T) %>%
    dplyr::summarize(frac = sum(QTDE_VOTOS) / dplyr::first(total)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(party_ideologies, by = c('ANO_ELEICAO' = 'ANO_ELEICAO', 'party' = 'NUM_PART')) %>%
    dplyr::group_by(ANO_ELEICAO, NUM_ZONA, NUM_SECAO) %>%
    dplyr::summarize(ideology = sum(frac * IDEO_IMPUTED, na.rm = T)) %>%
    dplyr::ungroup()
}
