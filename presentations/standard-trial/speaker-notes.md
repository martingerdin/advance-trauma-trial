# Speaker notes — standard trial presentation

These are speaking prompts, not a script or an integrated presenter mode. IDs and order follow `src/slides.ts`. Sources below are relative to the repository root `advance-trauma-trial/`; section names refer to `protocol/protocol.qmd` unless another path is given. Describe planned methods, not achieved recruitment, approvals, funding status or results.

## Timing and route choices

- **Brief (`?view=brief`):** 13 slides, including `atls-forest`, about **9–12 minutes** plus questions. Allow roughly 30–45 seconds per substantive slide and 60–90 seconds each for the forest and animated design. Fold the allocation/blinding caveat into `design`, consent into `eligibility`, and interpretation into the close. Summarise the omitted background evidence in the forest discussion rather than demonstrating every filter.
- **Core (omit `view`):** 25 slides, about **20–25 minutes** plus questions. Allow about five minutes for the five added background/evidence slides, 90 seconds for design, and roughly one minute each for outcomes, analysis and consent. Forest interaction can be deferred to questions.
- **Full (`?view=full`):** 29 slides, about **25–30 minutes** plus questions. Add approximately one minute each for the four detail slides and additional methodological discussion. Detail slides can instead be used for questions via the full-route overview/hash links.

Route labels below: **all** = brief/core/full; **core/full** = omitted from brief; **full only** = optional detail. Do not try to open a full-only hash in core. Timing is guidance, not a built-in timer.

## `title` — ADVANCE TRAUMA

**All · 15–20 seconds.** Introduce a hospital-randomised evaluation of ATLS training versus standard care in India. Registration identifiers identify the trial, not approval status. Although the title uses “adult,” explicitly clarify later that eligibility starts at **15 years**. Do not identify a legal sponsor without verification.

**Sources:** `shared-assets/study-identifiers.qmd`; Trial aim; Overall trial design.

## `background` — Background and rationale

**All · 40–50 seconds.** Separate the global burden of injury from the evidence gap: widespread training and favourable observational associations do not establish a causal mortality benefit. Confounding and accompanying system changes motivate a randomised evaluation. Avoid saying that ATLS has already been proved effective or ineffective.

**Sources:** Background and rationale; Updated systematic review; references displayed in `src/slides.ts` on this slide; `shared-assets/bibliography.bib`.

## `atls-purpose` — Purpose and content

**Core/full · 45–60 seconds.** Introduce this as the original ACS manual quotation, restored verbatim from the Sweden slide—not a paraphrased bullet summary. Emphasise rapid assessment **concurrent with treatment**, followed by reevaluation, stabilisation and transfer when needed. This describes the programme's intended approach, not an observed trial effect or proof of survival benefit. Do not narrate removed reach/count bullets as if they remain on this slide.

**Quote source:** American College of Surgeons, *Advanced Trauma Life Support® (ATLS®) Student Course Manual*, 11th ed., 2025. Restored from `presentations/atls-sweden-30-years/src/slides.ts` → `atls-purpose`; current slide `presentations/standard-trial/src/slides.ts` → `atls-purpose`. **Protocol context:** Background and rationale; `shared-assets/intervention-and-control-treatment.qmd`.

## `clinician-evidence` — Impact on clinicians

**Core/full · 50–60 seconds.** Attribute the displayed words to the ACS manual: it claims improvements in knowledge, psychomotor skills, resuscitation performance and confidence, with organisational/procedural skills retained for at least six years. These are **quoted educational claims, not ADVANCE TRAUMA results**. Do not extend the retention claim to unchanged retention of every skill, or infer that improved knowledge/course performance establishes a causal mortality benefit or sustained bedside adherence in this trial.

**Optional off-slide context, not displayed study summaries:** a randomised study of 32 physicians improved OSCE scores, priorities and organisation; a controlled study of 40 medical students improved knowledge and management skills. Follow-up distinguishes retained priorities/organisation from earlier decline in cognitive scores. Use these details only to explain the evidence, not as bullets supposedly on screen.

**Quote source:** American College of Surgeons, *Advanced Trauma Life Support® (ATLS®) Student Course Manual*, 11th ed., 2025. Restored from `presentations/atls-sweden-30-years/src/slides.ts` → **`atls-providers`**; current slide `presentations/standard-trial/src/slides.ts` → `clinician-evidence`. **Off-slide contextual sources:** Sweden source ID `atls-provider-evidence`: Ali et al., *World J Surg.* 1996;20:1121–1125; Ali et al., *J Trauma.* 1995;38:687–691; Ali et al., *J Trauma.* 1996;40:860–866. **Protocol context:** Background and rationale; `shared-assets/bibliography.bib`.

## `patient-evidence` — Impact on patients

**Core/full · 50–60 seconds.** Say explicitly: “These are the ACS manual's claims, not findings from ADVANCE TRAUMA.” The original quotation links training with lower injury mortality, lower area-level death rates and survival with a small trauma team. Preserve that attribution when discussing its causal-sounding “has resulted” wording. Observational before/after and ecological associations do not prove training caused the difference; concurrent system improvements, case mix and setting may explain some associations. The quote's “equivalent patient survival” is not, by itself, evidence from a formal equivalence trial.

**Optional off-slide context, not displayed study summaries:** Ali's before/after cohort and Rutledge's county-level study support the distinction between an association and an individual causal benefit. The Sweden evidence slide also cites Deo's small-team/TRISS study. van Olden reported fewer first-hour deaths without a change in overall mortality; that separate context is not text in the restored quote, and non-significance does not prove no benefit. Transition to the systematic reviews to assess how strong the patient-outcome evidence actually is.

**Quote source:** American College of Surgeons, *Advanced Trauma Life Support® (ATLS®) Student Course Manual*, 11th ed., 2025. Restored from `presentations/atls-sweden-30-years/src/slides.ts` → **`atls-outcomes-claim`**; current slide `presentations/standard-trial/src/slides.ts` → `patient-evidence`. **Off-slide contextual sources:** Sweden source ID `atls-patient-impact-sources`: Ali et al., *J Trauma.* 1993;34:890–898; Rutledge et al., *Ann Surg.* 1994;219:547–563; Deo et al., *Injury.* 1997;28:633–637; van Olden et al., *Am J Emerg Med.* 2004;22:522–525. **Protocol context:** Background and rationale; Updated systematic review; `shared-assets/bibliography.bib`.

## `systematic-reviews` — What have systematic reviews found?

**Core/full · 60 seconds.** Reviews differ in scope and eligibility. Mohammad's 23-study review supports educational gains but not strong mortality evidence; the 2014 Cochrane review found no eligible controlled ATLS mortality/morbidity trials under its criteria. That is a review finding, not a timeless statement about all trauma training research. Putra's seven-study estimate was OR 0.68 (95% CI 0.39–1.20), leaving substantial uncertainty rather than proving no benefit. Published Nakhid pooled **17 observational studies of trauma life support training**, OR **0.60 (95% CI 0.48–0.75)**—not 17 randomised ATLS trials. The next slide adds a study and is not this same published estimate.

**Sources:** Mohammad et al., *World J Surg.* 2014;38:322–329; Jayaraman et al., *Cochrane Database Syst Rev.* 2014;CD004173; Putra et al., *New Ropanasuri J Surg.* 2023;8(2); Nakhid et al., *Scand J Trauma Resusc Emerg Med.* 2026;34:15. Source deck `presentations/atls-sweden-30-years/src/slides.ts` → `atls-outcomes-reviews`; standard-trial `src/slides.ts` → `systematic-reviews`; protocol → Background and rationale, Updated systematic review. The protocol's ATLS-only/quality-rating analysis has a different scope; do not silently substitute it for the published multiple-programme estimate.

## `atls-forest` — Updated systematic review

**All · 60–90 seconds; allow up to two minutes for interaction in full.** The plot opens with **all 18 studies** included: exported random-effects REML **OR 0.58 (95% CI 0.46–0.72), I² 75%**. This is the inherited multiple-programme update of the published Nakhid **17-observational-study** estimate, **OR 0.60 (95% CI 0.48–0.75)**. The additional **Lule et al. (2026)** study is a **cluster randomised trial of RTTDC, not ATLS**. Neither the slide ID nor legacy JSON field names make all studies ATLS studies or turn ORs into risk ratios. High heterogeneity and predominantly observational evidence limit causal conclusions; do not translate OR 0.58 into a 42% reduction in mortality risk.

Filters by programme/design and individual study selection change the included set and **re-pool live**. Multi-study subsets use the browser's DerSimonian–Laird model; all studies restore the exported REML estimate. Filtered estimates are exploratory **not published subset estimates**, and selecting RCT isolates RTTDC evidence, not a randomised ATLS mortality trial. For brief, explain the default estimate and these caveats without a long filter demonstration. Rehearse navigation because advance keys can also drive the forest's staged interaction; use “All studies” to restore the full estimate.

**Sources:** inherited `presentations/standard-trial/src/data/meta-analysis.json` → `studies` (especially `Lule2026`), `pooled`, `measure`; source deck `presentations/atls-sweden-30-years/src/slides.ts` → `atls-outcomes-reviews`, `atls-forest`, and its `src/data/meta-analysis.json`; standard-trial `src/forest-plot.ts` and `src/pool-meta.ts` for interaction/pooling. Canonical protocol context: `protocol/protocol.qmd` → Updated systematic review. That section describes a separate ATLS-only, quality-rating-aware analysis and should not be cited as if it were the same 18-study multiple-programme data set.

## `aim` — Aim

**All · 15–20 seconds.** State the comparison once: introducing ATLS training versus standard care, evaluated through patient outcomes. This is a training/implementation intervention, not a comparison of drugs or a guarantee that every patient is treated by a trained physician.

**Sources:** Trial aim; Overall trial design; `shared-assets/intervention-and-control-treatment.qmd`.

## `design` — Batched stepped-wedge cluster randomised trial

**All · 60–90 seconds.** Walk through standard care, the training transition, and intervention. Hospitals are clusters; multiple participating units in one hospital remain one cluster. Thirty hospitals enter six batches of five, with one hospital per implementation sequence within each batch. Each hospital eventually receives training; randomisation determines when, not whether. Staggering improves feasibility, but calendar-time changes must be accounted for analytically.

For brief, add: randomisation is constrained by patient volume, order is concealed only as long as logistics allow, and **blinding is not possible**. Do not imply blinded assessors or analysts. Rehearse the staged reveal; Space has animation-specific behaviour while it is incomplete.

**Sources:** Overall trial design; Design justification; Randomisation; Blinding; `protocol/functions/global_variables.R`.

## `setting` — Setting and clusters

**All · 40–50 seconds.** These are secondary/tertiary trauma-care hospitals in India, with round-the-clock surgical and orthopaedic emergency services. The intervention focuses on selected rotating units, not the whole hospital workforce. Data collection follows those units’ working days before and after training. The ≤25% threshold means some clinicians may already have training: do not say every control clinician is untrained.

In brief, summarise that hospital and unit volumes and competing interventions also determine eligibility; this slide is not the complete screening checklist.

**Sources:** Cluster selection; `shared-assets/eligibility-criteria-cluster.qmd`; `shared-assets/intervention-and-control-treatment.qmd`.

## `cluster-eligibility` — Cluster eligibility in detail

**Full only · 60 seconds.** Hospital volume is ≥400 trauma admissions/referrals/transfers for admission annually **or** ≥35 monthly for at least the last six months. Selected units need ≥12 monthly for at least six months. Both hospital and unit levels must meet the ≤25% prior-training condition. Competing formal training and major trauma-system changes are exclusions; examples include NELS, a new trauma centre, trauma-team implementation or a quality-improvement programme. Explain that these criteria support a meaningful comparison, not a representative sample of all Indian hospitals.

**Sources:** `shared-assets/eligibility-criteria-cluster.qmd` → Inclusion criteria, Exclusion criteria and footnotes; Cluster selection → Screening.

## `eligibility` — Patient eligibility

**All · 50–60 seconds.** All inclusion conditions apply: **age ≥15**, injury **<48 hours** before arrival, trauma presentation to the ED under ICD chapter XX, the specified admission/death/transfer pathway, and management by a participating unit. Death between arrival and admission is explicitly included; do not restrict the population to admitted survivors. Exclusions are isolated closed extremity fracture and direct ward admission without ED physician assessment.

For brief, note that consent concerns collection of patient data rather than opting out of the hospital’s training; fuller consent arrangements include opt-out, opt-in and waiver provisions.

**Sources:** `shared-assets/eligibility-criteria-patient-participant.qmd`; Patient participant selection; Procedure for obtaining consent.

## `intervention` — Intervention and comparator

**All · 40–50 seconds.** ATLS is an accredited 2.5-day course in India, generally for about ten physicians in one or two units per hospital. It targets those providing initial resuscitation. Control is usual initial care before trial-provided training, not absence of clinical care or a ban on other indicated treatments. The training estimate is not a fixed promise of ten places at every site.

**Sources:** `shared-assets/intervention-and-control-treatment.qmd`; Concomitant use of other medications or treatments.

## `training` — Delivering the intervention

**Full only · 45–60 seconds.** Explain the first-hour focus and unit-based delivery. Unit selection is by convenience, whereas hospital implementation timing is randomised: these are different selection/allocation processes. Training includes primary and secondary surveys, resuscitation, triage/transfer, practical scenarios, lectures and a proficiency evaluation. Other treatment remains discretionary, subject to the exclusions for competing programmes and major interventions. Do not equate course completion with demonstrated fidelity in every patient encounter.

**Sources:** `shared-assets/intervention-and-control-treatment.qmd`; Overall trial design; Concomitant use of other medications or treatments.

## `allocation` — Randomisation and masking

**Core/full · 45–60 seconds.** Hospitals enter batches when eligible and ethically approved; batches need not correspond to regions. Within-batch covariate-constrained randomisation balances monthly eligible-patient volume. Concealment lasts as long as logistics allow; do not promise a fixed notification interval. **The trial is not blinded.** Concealment before disclosure and blinding after allocation are distinct safeguards, and neither should be overstated.

**Sources:** Randomisation; Blinding; Overall trial design; `protocol/functions/global_variables.R`.

## `timeline` — What happens at each hospital?

**Core/full · 40–50 seconds.** Thirteen inclusion months comprise variable standard-care/intervention durations around one transition month; each of the two care phases lasts at least four months. Nested measurements bracket transition by three months on either side. Each patient has follow-up through 90 days from ED arrival, so the last recruited patient’s follow-up extends beyond inclusion. **Transition-phase data are excluded from the main analysis**, not evidence that inclusion or care stops during training.

**Sources:** Overall trial design; Procedures; Statistics → General principles; `protocol/functions/global_variables.R`.

## `primary-outcome` — Primary outcome

**All · 40–50 seconds.** Emphasise every qualifier: death **in hospital**, during the **initial admission**, within **30 days of ED arrival**. This is not all-cause 30-day mortality, and a death at home after discharge is not this primary endpoint. Transferred patients are followed by contacting the patient, representative or receiving hospital; transfer must not simply be treated as survival or loss to follow-up.

**Sources:** `shared-assets/outcomes.qmd` → Primary outcome.

## `secondary-outcomes` — Secondary outcomes

**All · 45–60 seconds.** All-cause mortality includes deaths regardless of setting at 24 hours, 30 days and 90 days. Main-design outcomes also address resource use and return to work. The nested study measures observed care and patient-reported recovery; it does not collect those measures in every trial participant. In brief, mention that recovery assessments also include a discharge-related time point and that nested observations bracket transition.

Do not describe in-hospital mortality at every secondary time point as a separately specified outcome. The outcome asset says “three months”; the procedures and deck operationalise follow-up as 90 days.

**Sources:** `shared-assets/outcomes.qmd` → Secondary outcomes; Procedures; Overall trial design.

## `schedule` — Schedule of patient assessments

**Core/full · 45–60 seconds.** Orient the audience to ED arrival as time zero. Mortality follow-up is at 24 hours, 30 days and 90 days; return to work is at 30/90 days. Nested EQ-5D-5L and WHODAS assessments are in survivors **within seven days of discharge**, as well as 30/90 days after arrival. The seven-day window is relative to discharge, not ED arrival. ATLS adherence is observed for up to one hour after the physician first sees the patient. Telephone follow-up is used after discharge; records supply stay/ICU measures.

**Sources:** Procedures; `shared-assets/outcomes.qmd` → Primary outcome and Secondary outcomes.

## `nested-methods` — Nested study: sampling and measurement

**Full only · 60 seconds.** Sampling spans three months before and after transition, excluding the transition window itself. Coordinator shifts of about eight hours are randomised across morning, afternoon and night, with measures collected in included patients during those shifts. Target ≥6 patients/cluster/month allows for loss to follow-up; it is not six additional patients outside the main cohort. All 14 primary-survey items completed constitutes 100% adherence.

Canonical wording needs care: the protocol-included `sample-size-calculations.qmd` says simple random sampling at shift level, while the separate nested asset says stratified sampling by shift. Use “randomised coordinator shifts” unless the applicable operational specification is confirmed. Nested power assumptions concern data-collection planning, not guaranteed power for every secondary inference.

**Sources:** Overall trial design; `shared-assets/outcomes.qmd` → Collected using the nested staircase design; `shared-assets/sample-size-calculations.qmd` → Nested staircase design and secondary outcomes; `shared-assets/sample-size-calculations-nested.qmd`.

## `sample-size` — Sample size

**All · 60 seconds.** The planned **4,320 analysis observations** are 30 × 12/month × **12 non-transition months**. At the same rate across all 13 inclusion months, there would be about **156 inclusions/hospital**, but **144/hospital outside transition**. Do not compare an all-inclusions total directly with the analysis target. The calculation does not eliminate missing-outcome risk.

Approximately 90% power assumes mortality falls from 20% to 15%: **five percentage points absolute**, **25% relative**. It assumes ICC 0.02, CAC 0.9 and time-decay correlation, with variation in cluster sizes. These are assumptions, not observed event rates or a predicted result. If asked, sensitivity ranges are ICC 0.01–0.05 and CAC 0.8–1.0; a lower baseline mortality scenario of 10%→7.5% requires around 30 observations/cluster/month.

**Sources:** Statistics → Sample size calculations; `shared-assets/sample-size-calculations.qmd` → Main stepped wedge design and primary outcome; `protocol/functions/global_variables.R` → `sample.size` and design parameters.

## `analysis` — Statistical analysis

**Core/full · 60 seconds.** Analyse available observations regardless of training receipt, with exposure assigned by scheduled transition and transition data omitted. This is an intention-to-treat principle with **available data**, not a claim that missing outcomes are solved or imputed. Models account for hospital/hospital-period clustering and month because outcome trends could otherwise be mistaken for training effects.

For binary outcomes, report ORs and risk differences where identity-link models converge, with 95% CIs and two-sided 5% significance. An OR is not a risk ratio. Secondary outcomes have no multiplicity adjustment and should be interpreted together, not by highlighting an isolated significant result. **Non-significance does not prove no benefit or equivalence.**

**Sources:** Statistics → General principles; Analysis models.

## `analysis-detail` — Analysis: robustness and heterogeneity

**Full only · 60–75 seconds.** Explain why alternative time/correlation assumptions, batch trends, prognostic adjustment and time-since-training effects matter. Small-cluster correction is part of the planned analysis, not merely an optional sensitivity exercise; the protocol does not fix the precise correction. Do not name a selected correction or exact covariate set without checking the applicable analysis plan.

Subgroups include region, age, sex, blunt multisystem trauma, penetrating trauma and severe isolated traumatic brain injury. Treat apparent subgroup differences cautiously; a significant finding in one subgroup and not another does not itself demonstrate interaction. The protocol age groups are 15–19, 20–24, 25–59 and ≥60 years.

**Sources:** Statistics → General principles, Analysis models, Additional sensitivity analyses, Estimation and reporting of within cluster correlations.

## `data` — Data collection and quality

**Core/full · 45–60 seconds.** Coordinators use records, observations and follow-up contacts; paper CRF data are transferred to the eCRF. Follow-up phone numbers are held locally. The protocol specifies a secure server, VPN and two-factor authentication, source verification, central/statistical monitoring and selective site visits. These are planned safeguards, not proof of completed audits or zero privacy risk. The operative protocol specifies at least ten years of archiving; do not substitute commented template text or infer compliance with every jurisdiction from this slide.

**Sources:** Patient participant selection → Screening; Procedures; Monitoring; Data protection; Collection, handling, and archiving of data → Source data.

## `consent` — Ethics and consent for data collection

**Core/full · 60–75 seconds.** Patients cannot opt out of physicians using their acquired training; consent provisions concern **data collection**. Routine data and observed adherence use opt-out with written information. Non-routine data require opt-in; for ages **15–18**, obtain assent plus guardian/representative consent as specified. Admitted patients provide written consent; transferred/discharged patients approached later may provide audio-recorded verbal consent.

Waiver applies when the patient cannot consent **and** has no legally authorised representative. On recovery, inform the patient and seek consent for non-routine data. Withdrawal/removal before final analysis does not affect care. Do not claim approvals are already obtained: confirm the applicable ethics decisions, permissions and approved consent procedures before use.

**Sources:** Patient participant selection → Screening and Withdrawal criteria; Procedure for obtaining consent; Ethical review of the trial.

## `safety` — Safety monitoring

**Core/full · 60 seconds.** Track the listed events in both control and intervention phases; an individual event does not automatically establish training-related harm. The protocol does not routinely collect all AEs/SAEs separately because many are expected in trauma and overlap outcomes. Safety surveillance focuses on unexpected serious complications, including missed injuries/investigations when identified.

Report to trial management within **24 hours of occurrence**. **Trial management assesses relatedness within 24 hours of reporting**; probably related events go immediately to the joint SDMC. **Local investigators follow events until fully evaluated**—do not transfer the management relatedness role to them. The listed ventilation/vasopressor thresholds are strictly “more than” seven/two days, with vasopressor restart after at least two days off.

**Sources:** Handling of adverse and safety events → Safety event, Reporting and assessment of adverse and safety events, Follow up of safety events.

## `oversight` — Trial oversight and dissemination

**All · 50–60 seconds.** Distinguish daily operations (Trial Team), clinical/practical management (TMG), and oversight (joint SDMC). SDMC has an independent majority and independent chair; it is not wholly independent membership. One interim analysis after half the batches complete examines feasibility and differential recruitment/ascertainment; no efficacy stopping is planned. Do not suggest that safety concerns could never stop the trial.

Name only the protocol-listed funders: **Swedish Research Council** and **Laerdal Foundation**. These names do not establish the legal sponsor or funder influence. Verify those roles and any additional funding before attributing them. Dissemination plans include reporting to funders within one year of completion, manuscripts using ICMJE authorship criteria, public metadata and anonymised data after completion; do not imply data are already released or promise unrestricted identifiable data.

For brief, close the narrative with uncertainty: a non-significant result is not evidence of equivalence; estimates must be interpreted with confidence intervals, implementation and setting.

**Sources:** Trial organisation → Trial Team, TMG and Joint SDMC; Interim analysis; End of trial; Funding; Notification of trial completion, reporting, and publication; Collection, handling, and archiving of data; `shared-assets/interim-analysis.qmd`; `shared-assets/study-identifiers.qmd`.

## `interpretation` — What will this trial tell us?

**Core/full · 40–50 seconds.** Frame survival, process and recovery together. This estimates the effect of introducing ATLS in these participating units and hospitals, not every possible effect of trauma education worldwide. Interpret confidence intervals, time trends, implementation, missingness and context. **A non-significant finding does not prove no benefit**; it may leave clinically important benefit or harm compatible with the data. Do not present these conditional implications as results.

**Sources:** Trial aim; Design justification; Statistics → General principles, Analysis models and Additional sensitivity analyses. Policy implications are presenter interpretation, not a reported trial finding.

## `closing` — Questions?

**All · 10–15 seconds, then discussion.** Invite questions about the comparison, design and applicability. Use the website for further information, not as evidence of current approvals or completion. ATLS is an American College of Surgeons trademark; do not infer College sponsorship or endorsement. For technical questions, use the appropriate full-route detail slide rather than inventing an allocation, consent or analysis procedure.

**Sources:** `src/slides.ts` → `closing`; for substantive answers use the canonical sections above. Legal sponsor, funder roles and approvals require separately verified documentation.
