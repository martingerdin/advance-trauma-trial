systematic_review_data <- function() {
    list(
        Drimousis2011 = list(
            study = "Drimousis et al. 2011",
            design = "Registry based",
            year = 2011,
            outcome = NA,
            eligibility = "Required admission", # ISS > 16 data available
            non.atls.n = 6734,
            non.atls.died = 128,
            atls.n = 1431,
            atls.died = 57
        ),
        vanOlden2004 = list(
            study = "van Olden et al. 2004",
            design = "Prospective cohort",
            year = 2004,
            outcome = NA,
            eligibility = "ISS > 16",
            non.atls.n = 31,
            non.atls.died = 15,
            atls.n = 31,
            atls.died = 10
        ),
        Vestrup1988 = list(
            study = "Vestrup et al. 1988",
            design = "Retrospective cohort",
            year = 1988,
            outcome = NA,
            eligibility = "ISS > 14",
            non.atls.n = 50,
            non.atls.died = 13,
            atls.n = 71,
            atls.died = 14
        ),
        Ariyanayagam1992 = list(
            study = "Ariyanayagam et al. 1992",
            design = "Retrospective cohort",
            year = 1992,
            outcome = "< 6 hours",
            eligibility = NA,
            non.atls.n = 13739,
            non.atls.died = 637,
            atls.n = 9132,
            atls.died = 430
        ),
        Ali1993 = list(
            study = "Ali et al. 1993",
            design = "Retrospective cohort",
            year = 1992,
            outcome = NA,
            eligibility = "ISS > 16",
            non.atls.n = 413,
            non.atls.died = 279,
            atls.n = 400,
            atls.died = 134
        ),
        Olson2001 = list(
            study = "Olson et al. 2001",
            design = "Retrospective cohort",
            year = 2001,
            outcome = "30 days",
            eligibility = "Patients were included if they were younger than 80 years old and had at least 1 confirmed or suspected injury to the head, chest, lower extremity including femur or open tibia fracture, liver, or spleen",
            non.atls.n = 506,
            non.atls.died = 33,
            atls.n = 512,
            atls.died = 43
        ),
        Wang2010 = list(
            study = "Wang et al. 2010",
            design = "Retrospective cohort",
            year = 2010,
            outcome = NA,
            eligibility = "ISS > 16",
            non.atls.n = 438,
            non.atls.died = 87,
            atls.n = 382,
            atls.died = 62
        ),
        Petroze2014 = list(
            study = "Petroze et al. 2014",
            design = "Prospective cohort",
            year = 2014,
            outcome = "30 days",
            eligibility = "Any injured patient who is transferred from a district hospital for evaluation of their injury, any injured patient who dies in emergency from their injury, or any injured patient who is admitted (defined as inpatient hospitalization or emergency stay of [24 h) is prospectively recorded in the registry.",
            non.atls.n = 798,
            non.atls.died = 70,
            atls.n = 575,
            atls.died = 36
        ),
        Hashmi2013 = list(
            study = "Hashmi et al. 2013",
            design = "Retrospective study",
            year = 2013,
            outcome = NA,
            eligibility = "Adults (>15 years) with blunt or penetrating trauma",
            non.atls.n = 421,
            non.atls.died = 41,
            atls.n = 806,
            atls.died = 40
        ),
        Bellanova2016 = list(
            study = "Bellanova et al. 2016",
            design = "Cohort study",
            year = 2016,
            outcome = "48 hours",
            eligibility = "ISS > 15",
            non.atls.n = 98,
            non.atls.died = 9,
            atls.n = 132,
            atls.died = 5
        ),
        Magnone2016 = list(
            study = "Magnone et al. 2016",
            design = "Retrospective cohort",
            year = 2016,
            outcome = "24 hours",
            eligibility = "ISS > 15",
            non.atls.n = 198,
            non.atls.died = 28,
            atls.n = 141,
            atls.died = 10
        ),
        kamau_impact_2024 = list(
            study = "Kamau et al. 2024",
            design = "Retrospective matched case-control",
            year = 2024,
            outcome = "24 hours",
            eligibility = "ISS ≥ 16",
            non.atls.n = 81,
            non.atls.died = 14,
            atls.n = 81,
            atls.died = 5
        )
    )
}
