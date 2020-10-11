# aggregate_votes.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
#
# This file is licensed under the GNU General Public License, version 3.

aggregate_majoritarian = function(votes, position, turno, party_number) {
  if (turno == 'latest') {
    turno = max(votes$NR_TURNO)
  }

  votes_sum = votes %>%
    dplyr::filter(NR_TURNO == turno) %>%
    dplyr::filter(CD_CARGO == position) %>%
    dplyr::group_by(ano, NUM_ZONA, NUM_SECAO) %>%
    dplyr::mutate(total = sum(QT_VOTOS)) %>%
    dplyr::group_by(NUM_VOTAVEL, .add = T) %>%
    dplyr::summarize(abs_votes = sum(QT_VOTOS), cand = sum(QT_VOTOS) / dplyr::first(total)) %>%
    dplyr::ungroup()

  if (party_number == 'winner') {
    party_number = votes_sum %>%
      dplyr::group_by(NUM_VOTAVEL) %>%
      dplyr::summarize(abs_votes = sum(abs_votes)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(abs_votes == max(abs_votes)) %>%
      dplyr::pull(NUM_VOTAVEL)
  }

  votes_sum %>%
    dplyr::filter(NUM_VOTAVEL == party_number)
}

aggregate_minoritarian_ideology = function(votes, positions) {
  votes %>%
    dplyr::filter(CD_CARGO %in% positions) %>%
    dplyr::mutate(party = as.numeric(stringr::str_sub(NUM_VOTAVEL, end = 2))) %>%
    dplyr::group_by(NUM_ZONA, NUM_SECAO) %>%
    dplyr::mutate(total = sum(QT_VOTOS)) %>%
    dplyr::group_by(party, .add = T) %>%
    dplyr::summarize(frac = sum(QT_VOTOS) / dplyr::first(total)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(party_ideologies, by = c('ano' = 'ANO_ELEICAO', 'party' = 'NUM_PART')) %>%
    dplyr::group_by(ano, NUM_ZONA, NUM_SECAO) %>%
    dplyr::summarize(ideology = sum(frac * IDEO_IMPUTED, na.rm = T)) %>%
    dplyr::ungroup()
}
