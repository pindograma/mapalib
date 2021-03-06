/*
 * mapwalk.cpp
 * (c) 2020 CincoNoveSeis Jornalismo Ltda.
 *
 * This program is licensed under the GNU General Public License, version 3.
 * See the LICENSE file for details.
 */

#include <Rcpp.h>
#include <algorithm>
#include <random>

using namespace Rcpp;

DataFrame mapwalk_(NumericVector main,
                   List geom,
                   CharacterVector codetract,
                   List g_geom,
                   CharacterVector g_codetract,
                   std::set<int> all,
                   std::vector<int> useless,
                   int epsg,
                   DataFrame tracts)
{
    int orig_len = all.size();
    Rcout << "orig_len = " << orig_len << "\n";

    Environment sf = Environment::namespace_env("sf");

    Function st_intersects = sf["st_intersects"];
    Function st_sfc = sf["st_sfc"];

    List geom_q = List::create();
    for (int i = 0; i < geom.size(); i++) {
        if (!useless[i])
            geom_q.push_back(geom[i]);
    }
    geom_q = st_sfc(geom_q, Named("crs", epsg));

    List q_ = st_intersects(geom_q, tracts);

    List q = List::create();
    int j = 0;
    for (int i = 0; i < geom.size(); i++) {
        if (!useless[i]) {
            q.push_back(q_[j]);
            j += 1;
        }
        else
            q.push_back(IntegerVector());
    }

    std::random_device rd;
    std::mt19937 gen(rd());

    for (int i = 0; i < q.size(); i++) {
        if (useless[i])
            continue;

        NumericVector matches = q[i];
        std::vector<int> candidates;

        for (NumericVector::iterator j = matches.begin(); j != matches.end(); j++) {
            if (*j == i) continue;

            std::set<int>::iterator f = std::find(all.begin(), all.end(), *j);
            if (f == all.end())
                candidates.push_back(*j);
        }

        if (candidates.size() == 0) {
            useless[i] = true;
            continue;
        }

        std::uniform_int_distribution<int> dis(0, candidates.size() - 1);
        int pick = candidates[dis(gen)];
        all.insert(pick);

        main.push_back(main[i]);
        geom.push_back(g_geom[pick - 1]);
        codetract.push_back(g_codetract[pick - 1]);
    }

    geom = st_sfc(geom, Named("crs", epsg));

    if (all.size() == orig_len) {
		    Function st_sf = sf["st_sf"];
        return st_sf(Named("geom", geom),
                     Named("main", main),
                     Named("code_tract", codetract));
    }

    return mapwalk_(main, geom, codetract, g_geom, g_codetract, all, useless, epsg, tracts);
}

// [[Rcpp::export]]
DataFrame mapwalk(DataFrame spt, DataFrame sp, NumericVector all_vec, int epsg) {
    std::vector<int> all_ = as<std::vector<int> >(all_vec);
    std::set<int> all(all_.begin(), all_.end());

    NumericVector main = spt["main"];
    CharacterVector codetract = spt["code_tract"];
    List geom = spt["geom"];
    List g_geom = sp["geom"];
    CharacterVector g_codetract = sp["code_tract"];

    std::vector<int> useless(150000, false);

    return mapwalk_(main, geom, codetract, g_geom, g_codetract, all, useless, epsg, sp);
}
