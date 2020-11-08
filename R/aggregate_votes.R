# aggregate_votes.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

#' @export
aggregate_majoritarian = function(votes, position, turno, party_number, with_blank_null) {
  if (turno == 'latest') {
    turno = max(votes$NUM_TURNO)
  }

  if (!with_blank_null) {
    votes = votes %>%
      dplyr::filter(NUMERO_CANDIDATO <= 90)
  }

  votes_sum = votes %>%
    dplyr::filter(NUM_TURNO == turno) %>%
    dplyr::filter(CODIGO_CARGO == position) %>%
    dplyr::group_by(ANO_ELEICAO, NUM_ZONA, NUM_SECAO) %>%
    dplyr::mutate(total = sum(QTDE_VOTOS)) %>%
    dplyr::group_by(NUMERO_CANDIDATO, .add = T) %>%
    dplyr::summarize(
      NUM_TURNO = dplyr::first(NUM_TURNO),
      abs_votes = sum(QTDE_VOTOS),
      cand = sum(QTDE_VOTOS) / dplyr::first(total)
    ) %>%
    dplyr::ungroup()

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
