#' Default seed for reproducible shell simulated data
#'
#' @return Integer seed (GitHub issue number for this feature).
shell_simulated_data_seed <- function() {
    76L
}

#' Replace empty `(n=)` / `(n=;` placeholders with counts, left to right
#'
#' @param text Character. Label text containing one or more `(n=` tokens that
#'     end at `)` or `;` (as in `(n=)` or `(n=; reasons)`).
#' @param counts Integer or numeric vector of counts, one per `(n=` token.
#' @return Character with counts filled in.
fill_consort_n <- function(text, counts) {
    assertthat::assert_that(is.character(text) && length(text) == 1)
    counts <- as.integer(counts)
    assertthat::assert_that(length(counts) >= 1L)
    n.slots <- length(gregexpr("\\(n=(?=\\)|;)", text, perl = TRUE)[[1]])
    if (identical(n.slots, -1L)) {
        n.slots <- 0L
    }
    assertthat::assert_that(
        length(counts) == n.slots,
        msg = paste0(
            "Expected ", n.slots, " count(s) for (n=) placeholders, got ",
            length(counts)
        )
    )
    result <- text
    for (i in seq_len(n.slots)) {
        result <- sub(
            "\\(n=(?=\\)|;)",
            paste0("(n=", counts[[i]]),
            result,
            perl = TRUE
        )
    }
    result
}

#' Simulate a continuous shell variable from its field name
#'
#' @param field.name Character.
#' @param n Integer.
#' @return Numeric vector of length `n`.
simulate_shell_continuous <- function(field.name, n) {
    name <- tolower(field.name)
    values <- if (grepl("age", name)) {
        stats::rnorm(n, mean = 38, sd = 16)
    } else if (grepl("glasgow|gcs", name)) {
        pmin(15, pmax(3, round(stats::rnorm(n, mean = 13.5, sd = 2.2))))
    } else if (grepl("systolic", name)) {
        stats::rnorm(n, mean = 118, sd = 22)
    } else if (grepl("diastolic", name)) {
        stats::rnorm(n, mean = 74, sd = 14)
    } else if (grepl("heart_rate|heartrate", name)) {
        stats::rnorm(n, mean = 92, sd = 18)
    } else if (grepl("respiratory", name)) {
        stats::rnorm(n, mean = 20, sd = 5)
    } else if (grepl("oxygen|spo2|saturation", name)) {
        pmin(100, pmax(70, stats::rnorm(n, mean = 96, sd = 4)))
    } else if (grepl("temperature", name)) {
        stats::rnorm(n, mean = 98.4, sd = 1.1)
    } else if (grepl("injury_severity|iss", name)) {
        pmax(1, round(stats::rlnorm(n, meanlog = 2.2, sdlog = 0.7)))
    } else if (grepl("hospital_beds|volume|beds", name)) {
        pmax(1, round(stats::rlnorm(n, meanlog = 4.5, sdlog = 0.55)))
    } else if (grepl("transfusion_units|units", name)) {
        pmax(0, round(stats::rpois(n, lambda = 2.5)))
    } else if (grepl("frailty|clinical_frailty", name)) {
        pmin(9, pmax(1, round(stats::rnorm(n, mean = 3.5, sd = 1.6))))
    } else if (grepl("asa|preoperative", name)) {
        pmin(5, pmax(1, round(stats::rnorm(n, mean = 2.4, sd = 0.9))))
    } else if (grepl("eq5d.*vas|vas", name)) {
        pmin(100, pmax(0, round(stats::rnorm(n, mean = 62, sd = 18))))
    } else if (grepl("whodas.*summary|summary_score", name)) {
        pmin(100, pmax(0, round(stats::rnorm(n, mean = 28, sd = 16))))
    } else if (grepl("length_of_stay|los|days", name)) {
        pmax(0, round(stats::rlnorm(n, meanlog = 1.6, sdlog = 0.8)))
    } else {
        stats::rnorm(n, mean = 50, sd = 15)
    }
    as.numeric(values)
}

#' Simulate one shell variable column
#'
#' @param specification List with `field_name`, `type`, and optional `levels`.
#' @param n Integer number of rows.
#' @param missing.rate Numeric in \\[0, 1\\). Rate of missing values when
#'     missings are requested.
#' @param include.missing Logical. If TRUE, introduce `NA`s at `missing.rate`.
#' @return A vector of length `n`.
simulate_shell_variable <- function(specification,
                                    n,
                                    missing.rate = 0.04,
                                    include.missing = TRUE) {
    type <- specification$type
    field.name <- specification$field_name
    levels <- specification$levels

    if (identical(type, "continuous")) {
        values <- simulate_shell_continuous(field.name, n)
    } else if (identical(type, "dichotomous")) {
        values <- factor(
            ifelse(stats::rbinom(n, 1L, 0.32) == 1L, "Yes", "No"),
            levels = c("Yes", "No")
        )
    } else {
        assertthat::assert_that(length(levels) > 0L)
        ## Uneven multinomial so shells do not look artificially uniform
        weights <- rev(seq_along(levels))
        probs <- weights / sum(weights)
        values <- factor(
            sample(levels, size = n, replace = TRUE, prob = probs),
            levels = levels
        )
    }

    if (isTRUE(include.missing) && missing.rate > 0) {
        n.missing <- max(1L, round(n * missing.rate))
        missing.index <- sample.int(n, size = min(n.missing, n))
        is.na(values) <- missing.index
    }

    values
}

#' Build a simulated data frame for gtsummary shell tables
#'
#' @param specifications List of variable specifications.
#' @param groups Character vector of stratum labels.
#' @param n.per.group Integer rows per stratum.
#' @param group.column Character name for the grouping column.
#' @param seed Integer RNG seed.
#' @param missing Logical/character from the shell builder: when `"no"`, do not
#'     inject missing values.
#' @return A data frame ready for `gtsummary::tbl_summary()`.
build_simulated_shell_data <- function(specifications,
                                       groups,
                                       n.per.group = 120L,
                                       group.column = "group",
                                       seed = shell_simulated_data_seed(),
                                       missing = "always") {
    assertthat::assert_that(is.list(specifications) && length(specifications) > 0)
    assertthat::assert_that(is.character(groups) && length(groups) >= 1)
    assertthat::assert_that(is.numeric(n.per.group) && n.per.group >= 1)
    set.seed(as.integer(seed))

    n.rows <- as.integer(n.per.group) * length(groups)
    shell.data <- data.frame(
        group = factor(rep(groups, each = as.integer(n.per.group)), levels = groups),
        stringsAsFactors = FALSE
    )
    names(shell.data)[1] <- group.column

    include.missing <- !identical(missing, "no")
    for (specification in specifications) {
        shell.data[[specification$field_name]] <- simulate_shell_variable(
            specification = specification,
            n = n.rows,
            include.missing = include.missing
        )
    }
    shell.data
}

#' Simulated intervention-effect estimate for analysis-result shells
#'
#' @param measure Character effect-measure code.
#' @param seed Integer.
#' @return A list with `estimate`, `conf.low`, `conf.high`, and `p.value`.
simulate_shell_effect_estimate <- function(measure, seed) {
    set.seed(as.integer(seed))
    if (identical(measure, "ARD") || identical(measure, "mean difference") ||
        identical(measure, "Logit-scale difference")) {
        estimate <- stats::rnorm(1, mean = -0.02, sd = 0.025)
        se <- abs(stats::rnorm(1, mean = 0.015, sd = 0.005)) + 0.005
    } else if (identical(measure, "Rate ratio")) {
        log.estimate <- stats::rnorm(1, mean = -0.08, sd = 0.12)
        estimate <- exp(log.estimate)
        se <- abs(stats::rnorm(1, mean = 0.10, sd = 0.03)) + 0.03
        ## Store CI on the ratio scale via log-normal approximation below
        return(list(
            estimate = estimate,
            conf.low = exp(log.estimate - 1.96 * se),
            conf.high = exp(log.estimate + 1.96 * se),
            p.value = 2 * stats::pnorm(-abs(log.estimate / se))
        ))
    } else {
        ## OR / COR and other ratio measures
        log.estimate <- stats::rnorm(1, mean = -0.15, sd = 0.18)
        estimate <- exp(log.estimate)
        se <- abs(stats::rnorm(1, mean = 0.12, sd = 0.04)) + 0.04
        return(list(
            estimate = estimate,
            conf.low = exp(log.estimate - 1.96 * se),
            conf.high = exp(log.estimate + 1.96 * se),
            p.value = 2 * stats::pnorm(-abs(log.estimate / se))
        ))
    }

    list(
        estimate = estimate,
        conf.low = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        p.value = 2 * stats::pnorm(-abs(estimate / se))
    )
}

#' Simulated correlation / variance-component estimate for shells
#'
#' @param label Character parameter label.
#' @param seed Integer.
#' @return A list with `estimate`, `conf.low`, and `conf.high`.
simulate_shell_correlation_estimate <- function(label, seed) {
    set.seed(as.integer(seed))
    if (grepl("variance", label, ignore.case = TRUE)) {
        estimate <- abs(stats::rnorm(1, mean = 0.35, sd = 0.12)) + 0.05
        se <- abs(stats::rnorm(1, mean = 0.08, sd = 0.02)) + 0.02
    } else if (grepl("decay|rho", label, ignore.case = TRUE)) {
        estimate <- stats::runif(1, 0.55, 0.92)
        se <- abs(stats::rnorm(1, mean = 0.06, sd = 0.02)) + 0.015
    } else {
        ## ICC / within- / between-period correlations
        estimate <- stats::runif(1, 0.01, 0.12)
        se <- abs(stats::rnorm(1, mean = 0.02, sd = 0.008)) + 0.005
    }
    list(
        estimate = estimate,
        conf.low = max(0, estimate - 1.96 * se),
        conf.high = estimate + 1.96 * se
    )
}

#' Simulated counts for the cluster-level CONSORT shell
#'
#' @param sequences Integer number of sequences.
#' @param seed Integer.
#' @return A list of overall and per-sequence counts.
simulate_cluster_consort_counts <- function(sequences = 5L, seed = shell_simulated_data_seed()) {
    set.seed(as.integer(seed))
    n.seq <- as.integer(sequences)
    assessed <- 48L
    excl.not.meeting <- 10L
    excl.declined <- 5L
    excl.other.pre <- 3L
    excluded.pre <- excl.not.meeting + excl.declined + excl.other.pre
    randomised <- assessed - excluded.pre

    per.seq <- as.integer(rep(floor(randomised / n.seq), n.seq))
    remainder <- randomised - sum(per.seq)
    if (remainder > 0L) {
        per.seq[seq_len(remainder)] <- per.seq[seq_len(remainder)] + 1L
    }

    withdrawn <- as.integer(stats::rbinom(n.seq, 1L, 0.25))
    no.outcome <- as.integer(stats::rbinom(n.seq, 1L, 0.15))
    other.post <- as.integer(stats::rbinom(n.seq, 1L, 0.1))
    ## Keep exclusions within the sequence total
    for (k in seq_len(n.seq)) {
        while (withdrawn[k] + no.outcome[k] + other.post[k] > per.seq[k]) {
            if (other.post[k] > 0L) {
                other.post[k] <- other.post[k] - 1L
            } else if (no.outcome[k] > 0L) {
                no.outcome[k] <- no.outcome[k] - 1L
            } else {
                withdrawn[k] <- withdrawn[k] - 1L
            }
        }
    }
    included <- per.seq - withdrawn - no.outcome - other.post

    list(
        assessed = assessed,
        excl.not.meeting = excl.not.meeting,
        excl.declined = excl.declined,
        excl.other.pre = excl.other.pre,
        excluded.pre = excluded.pre,
        randomised = randomised,
        per.seq = per.seq,
        withdrawn = withdrawn,
        no.outcome = no.outcome,
        other.post = other.post,
        excluded.post = withdrawn + no.outcome + other.post,
        included = included,
        total.included = sum(included),
        total.withdrawn = sum(withdrawn),
        total.no.outcome = sum(no.outcome),
        total.other.post = sum(other.post),
        total.excluded.post = sum(withdrawn + no.outcome + other.post)
    )
}

#' Simulated counts for the patient-level CONSORT shell
#'
#' @param sequences Integer.
#' @param seed Integer.
#' @return A list of overall and per-sequence counts.
simulate_patient_consort_counts <- function(sequences = 5L, seed = shell_simulated_data_seed()) {
    set.seed(as.integer(seed) + 1L)
    n.seq <- as.integer(sequences)
    entered <- 4200L
    per.seq <- as.integer(round(entered * (0.18 + 0.01 * seq_len(n.seq))))
    per.seq <- as.integer(round(per.seq * entered / sum(per.seq)))
    per.seq[n.seq] <- entered - sum(per.seq[-n.seq])

    lost <- as.integer(pmax(8L, round(per.seq * stats::runif(n.seq, 0.015, 0.035))))
    withdrew <- as.integer(pmax(3L, round(per.seq * stats::runif(n.seq, 0.005, 0.015))))
    other <- as.integer(pmax(2L, round(per.seq * stats::runif(n.seq, 0.003, 0.01))))
    excluded <- lost + withdrew + other
    included <- per.seq - excluded

    before.share <- stats::runif(1, 0.46, 0.52)
    before.entered <- as.integer(round(entered * before.share))
    after.entered <- entered - before.entered
    before.excl.lost <- as.integer(round(sum(lost) * before.share))
    before.excl.withdrew <- as.integer(round(sum(withdrew) * before.share))
    before.excl.other <- as.integer(round(sum(other) * before.share))
    before.excluded <- before.excl.lost + before.excl.withdrew + before.excl.other
    before.included <- before.entered - before.excluded
    after.excl.lost <- sum(lost) - before.excl.lost
    after.excl.withdrew <- sum(withdrew) - before.excl.withdrew
    after.excl.other <- sum(other) - before.excl.other
    after.excluded <- after.excl.lost + after.excl.withdrew + after.excl.other
    after.included <- after.entered - after.excluded

    list(
        entered = entered,
        per.seq = per.seq,
        lost = lost,
        withdrew = withdrew,
        other = other,
        excluded = excluded,
        included = included,
        before.entered = before.entered,
        before.included = before.included,
        before.excl.lost = before.excl.lost,
        before.excl.withdrew = before.excl.withdrew,
        before.excl.other = before.excl.other,
        before.excluded = before.excluded,
        after.entered = after.entered,
        after.included = after.included,
        after.excl.lost = after.excl.lost,
        after.excl.withdrew = after.excl.withdrew,
        after.excl.other = after.excl.other,
        after.excluded = after.excluded,
        total.included = sum(included),
        total.lost = sum(lost),
        total.withdrew = sum(withdrew),
        total.other = sum(other),
        total.excluded = sum(excluded)
    )
}

#' Simulated counts for the nested-staircase CONSORT shell
#'
#' @param sequences Integer.
#' @param seed Integer.
#' @return A list of overall and per-sequence counts.
simulate_nested_staircase_consort_counts <- function(sequences = 5L,
                                                     seed = shell_simulated_data_seed()) {
    set.seed(as.integer(seed) + 2L)
    n.seq <- as.integer(sequences)
    in.windows <- 1800L
    outside.shifts <- 920L
    other.not.sampled <- 80L
    not.sampled <- outside.shifts + other.not.sampled
    sampled <- in.windows - not.sampled

    per.seq <- as.integer(round(sampled * (0.18 + 0.01 * seq_len(n.seq))))
    per.seq <- as.integer(round(per.seq * sampled / sum(per.seq)))
    per.seq[n.seq] <- sampled - sum(per.seq[-n.seq])

    lost <- as.integer(pmax(4L, round(per.seq * stats::runif(n.seq, 0.03, 0.06))))
    withdrew <- as.integer(pmax(2L, round(per.seq * stats::runif(n.seq, 0.01, 0.03))))
    other <- as.integer(pmax(1L, round(per.seq * stats::runif(n.seq, 0.005, 0.02))))
    excluded <- lost + withdrew + other
    analysed <- per.seq - excluded

    before.share <- stats::runif(1, 0.47, 0.53)
    before.in.windows <- as.integer(round(in.windows * before.share))
    after.in.windows <- in.windows - before.in.windows
    before.sampled <- as.integer(round(sampled * before.share))
    after.sampled <- sampled - before.sampled
    before.lost <- as.integer(round(sum(lost) * before.share))
    before.withdrew <- as.integer(round(sum(withdrew) * before.share))
    before.died <- as.integer(round(sum(other) * 0.4 * before.share))
    before.other <- as.integer(round(sum(other) * before.share)) - before.died
    before.other <- max(0L, before.other)
    before.excluded <- before.lost + before.withdrew + before.died + before.other
    before.analysed <- before.sampled - before.excluded
    after.lost <- sum(lost) - before.lost
    after.withdrew <- sum(withdrew) - before.withdrew
    after.died <- max(0L, as.integer(round(sum(other) * 0.4)) - before.died)
    after.other <- sum(other) - before.other - before.died - after.died
    after.other <- max(0L, after.other)
    after.excluded <- after.lost + after.withdrew + after.died + after.other
    after.analysed <- after.sampled - after.excluded

    list(
        in.windows = in.windows,
        outside.shifts = outside.shifts,
        other.not.sampled = other.not.sampled,
        not.sampled = not.sampled,
        sampled = sampled,
        per.seq = per.seq,
        lost = lost,
        withdrew = withdrew,
        other = other,
        excluded = excluded,
        analysed = analysed,
        before.in.windows = before.in.windows,
        before.sampled = before.sampled,
        before.analysed = before.analysed,
        before.lost = before.lost,
        before.withdrew = before.withdrew,
        before.died = before.died,
        before.other = before.other,
        before.excluded = before.excluded,
        after.in.windows = after.in.windows,
        after.sampled = after.sampled,
        after.analysed = after.analysed,
        after.lost = after.lost,
        after.withdrew = after.withdrew,
        after.died = after.died,
        after.other = after.other,
        after.excluded = after.excluded,
        total.analysed = sum(analysed),
        total.lost = sum(lost),
        total.withdrew = sum(withdrew),
        total.other = sum(other),
        total.excluded = sum(excluded)
    )
}
