systematic_review_data <- function() {
    ## Study-level mortality counts primarily from Nakhid et al. 2026
    ## (Scand J Trauma Resusc Emerg Med; doi:10.1186/s13049-026-01549-w).
    ## Includes the 17 studies that passed SIGN quality appraisal and entered
    ## the published mortality meta-analysis (Ariyanayagam 1992 and
    ## Drimousis 2011 were rated unacceptable and are omitted), plus
    ## Lule et al. 2026 (JMIR Hum Factors; doi:10.2196/82591) — a cluster
    ## RCT of RTTDC that was a preprint at the time of the Nakhid search
    ## and is now published.
    ##
    ## `atls.*` / `non.atls.*` columns are trained vs untrained counts
    ## (historical names kept for the metabin export). `programme` is the
    ## trauma life support training programme used as the intervention.
    list(
        Vestrup1988 = list(
            study = "Vestrup et al. 1988",
            design = "Retrospective cohort",
            year = 1988,
            programme = "ATLS",
            outcome = "In-hospital",
            eligibility = "ISS > 14",
            non.atls.n = 50,
            non.atls.died = 13,
            atls.n = 71,
            atls.died = 14
        ),
        Ali1993 = list(
            study = "Ali et al. 1993",
            design = "Retrospective cohort",
            year = 1993,
            programme = "ATLS",
            outcome = "In-hospital",
            eligibility = "ISS > 16",
            non.atls.n = 413,
            non.atls.died = 279,
            atls.n = 400,
            atls.died = 134
        ),
        vanOlden2004 = list(
            study = "van Olden et al. 2004",
            design = "Prospective cohort",
            year = 2004,
            programme = "ATLS",
            outcome = "Total mortality",
            eligibility = "ISS > 16",
            non.atls.n = 31,
            non.atls.died = 15,
            atls.n = 32,
            atls.died = 10
        ),
        Wang2010 = list(
            study = "Wang et al. 2010",
            design = "Retrospective cohort",
            year = 2010,
            programme = "ATLS",
            outcome = "In-hospital",
            eligibility = "ISS > 16",
            non.atls.n = 438,
            non.atls.died = 87,
            atls.n = 382,
            atls.died = 62
        ),
        Noordin2011 = list(
            study = "Noordin et al. 2011",
            design = "Prospective cohort",
            year = 2011,
            programme = "ATLS",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 435,
            non.atls.died = 42,
            atls.n = 574,
            atls.died = 33
        ),
        Hashmi2013 = list(
            study = "Hashmi et al. 2013",
            design = "Retrospective cohort",
            year = 2013,
            programme = "ATLS",
            outcome = "In-hospital",
            eligibility = "Adults (>15 years) with blunt or penetrating trauma",
            non.atls.n = 421,
            non.atls.died = 40,
            atls.n = 806,
            atls.died = 39
        ),
        Hondo2013 = list(
            study = "Hondo et al. 2013",
            design = "Retrospective cohort",
            year = 2013,
            programme = "JATEC",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 6495,
            non.atls.died = 864,
            atls.n = 27787,
            atls.died = 2362
        ),
        Petroze2015 = list(
            study = "Petroze et al. 2015",
            design = "Prospective cohort",
            year = 2015,
            ## Counted with ATLS in Nakhid et al. programme sensitivity analysis
            programme = "ATLS",
            outcome = "30 days",
            eligibility = "Injured patients transferred, dying in ED, or admitted",
            non.atls.n = 798,
            non.atls.died = 96,
            atls.n = 575,
            atls.died = 59
        ),
        Bellanova2016 = list(
            study = "Bellanova et al. 2016",
            design = "Prospective cohort",
            year = 2016,
            programme = "ATLS",
            outcome = "48 hours",
            eligibility = "ISS > 15",
            non.atls.n = 98,
            non.atls.died = 9,
            atls.n = 132,
            atls.died = 5
        ),
        Dennis2016 = list(
            study = "Dennis et al. 2016",
            design = "Retrospective cohort",
            year = 2016,
            programme = "RTTDC",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 61,
            non.atls.died = 1,
            atls.n = 69,
            atls.died = 5
        ),
        Magnone2016 = list(
            study = "Magnone et al. 2016",
            design = "Retrospective cohort",
            year = 2016,
            programme = "ATLS",
            outcome = "24 hours",
            eligibility = "ISS > 15",
            non.atls.n = 198,
            non.atls.died = 28,
            atls.n = 141,
            atls.died = 10
        ),
        CioePena2016 = list(
            study = "Cioè-Peña et al. 2016",
            design = "Prospective cohort",
            year = 2016,
            programme = "PTC",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 48,
            non.atls.died = 5,
            atls.n = 146,
            atls.died = 18
        ),
        Yao2018 = list(
            study = "Yao et al. 2018",
            design = "Retrospective cohort",
            year = 2018,
            programme = "CTCT",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 404,
            non.atls.died = 76,
            atls.n = 436,
            atls.died = 67
        ),
        Bauman2024 = list(
            study = "Bauman et al. 2024",
            design = "Prospective cohort",
            year = 2024,
            programme = "RTTDC",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 240,
            non.atls.died = 10,
            atls.n = 232,
            atls.died = 10
        ),
        Kamau2024 = list(
            study = "Kamau et al. 2024",
            design = "Retrospective matched case-control",
            year = 2024,
            programme = "ATLS",
            outcome = "30 days",
            eligibility = "ISS ≥ 16",
            non.atls.n = 81,
            non.atls.died = 14,
            atls.n = 81,
            atls.died = 5
        ),
        Nguyen2025 = list(
            study = "Nguyen et al. 2025",
            design = "Prospective cohort",
            year = 2025,
            programme = "PTC",
            ## 30-day estimate used in Nakhid et al. main meta-analysis
            outcome = "30 days",
            eligibility = NA,
            non.atls.n = 2031,
            non.atls.died = 97,
            atls.n = 1599,
            atls.died = 39
        ),
        Priestap2025 = list(
            study = "Priestap et al. 2025",
            design = "Retrospective cohort",
            year = 2025,
            programme = "RTTDC",
            outcome = "In-hospital",
            eligibility = NA,
            non.atls.n = 90,
            non.atls.died = 6,
            atls.n = 90,
            atls.died = 7
        ),
        Lule2026 = list(
            study = "Lule et al. 2026",
            design = "Cluster randomised trial",
            year = 2026,
            programme = "RTTDC",
            outcome = "90 days",
            eligibility = "Ages 2–80 with motorcycle-related neurological and/or musculoskeletal injuries",
            non.atls.n = 430,
            non.atls.died = 58,
            atls.n = 457,
            atls.died = 24
        )
    )
}
