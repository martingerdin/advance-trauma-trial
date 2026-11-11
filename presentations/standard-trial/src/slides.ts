export interface Reference { id: string; text: string }
export interface EvidenceItem { id: string; claim: string; source: string; tag?: string }
export interface Stat { value: string; label: string; source?: string; focus?: boolean }
export interface Milestone { year: string; label: string; image?: string; imageAlt?: string; cite?: string }
export interface OutcomeItem { title: string; detail?: string; tag?: string }
export interface TrialArm {
  variant: "control" | "intervention";
  tag: string;
  title: string;
  body: string;
  image: string;
  imageAlt: string;
}
export interface TeamMember { name: string; role: string }
export interface TeamGroup { label: string; location: string; members: TeamMember[] }
export interface Funder { name: string }
export interface ColumnGroup { heading: string; bullets: string[] }
export interface ScheduleRow { activity: string; times: string[] }
export interface Slide {
  id: string;
  layout: "title" | "section" | "stats" | "quote" | "bullets" | "arms"
    | "training-selection" | "batch-timeline" | "design" | "design-animation"
    | "sequences" | "forest" | "implications" | "closing" | "evidence"
    | "milestones" | "aim" | "outcomes" | "outcome-list" | "presenter"
    | "team" | "funding" | "status-map" | "columns" | "provocation" | "schedule";
  title?: string;
  subtitle?: string;
  eyebrow?: string;
  body?: string;
  bullets?: string[];
  statements?: string[];
  affiliations?: string[];
  stats?: Stat[];
  arms?: TrialArm[];
  image?: string;
  imageAlt?: string;
  imagePosition?: "left" | "right" | "background";
  cite?: string;
  source?: string;
  footer?: string;
  references?: Reference[];
  evidence?: EvidenceItem[];
  milestones?: Milestone[];
  outcomes?: OutcomeItem[];
  teamGroups?: TeamGroup[];
  funders?: Funder[];
  columns?: ColumnGroup[];
  trainingSelection?: "concentrated" | "distributed";
  anticipatedStart?: string;
  designVariant?: "main" | "staircase";
  designBatch?: number;
  schedule?: ScheduleRow[];
  /** Extra methods slides appear only in the full route. */
  detail?: boolean;
}

export const allSlides: Slide[] = [
  {
    id: "title", layout: "title", title: "ADVANCE TRAUMA",
    subtitle: "ATLS® training compared with standard care in adult trauma patients",
    footer: "A batched stepped-wedge cluster randomised trial in India<br>NCT06321419 · CTRI/2024/07/071336",
  },
  {
    id: "background", layout: "bullets", title: "Background and rationale",
    image: "./crash-illustration.png", imageAlt: "Road traffic collision requiring emergency trauma care",
    bullets: [
      "Around 5 million people die from injuries each year<sup>1</sup>",
      "ATLS® is the most widely used trauma life support programme, taught in more than 80 countries<sup>2</sup>",
      "Observational studies suggest better survival, but high-quality evidence on mortality is lacking<sup>3</sup>",
    ],
    references: [
      { id: "1", text: "GBD 2023 Causes of Death Collaborators. Lancet. 2025;406:1811–1872." },
      { id: "2", text: "American College of Surgeons. <a href='https://www.facs.org/quality-programs/trauma/education/advanced-trauma-life-support/'>Advanced Trauma Life Support®</a>." },
      { id: "3", text: "Nakhid Z et al. Scand J Trauma Resusc Emerg Med. 2026;34:15." },
    ],
  },
  {
    id: "atls-purpose",
    layout: "quote",
    title: "Purpose and content",
    body: "The ATLS Course emphasizes rapid assessment and concurrent treatment of severely injured patients. Care starts at the time of injury and continues through initial assessment, lifesaving interventions, reevaluation, stabilization, and, when needed, transfer.",
    cite: "American College of Surgeons. Advanced Trauma Life Support® (ATLS®) Student Course Manual. 11th ed. 2025.",
  },
  {
    id: "clinician-evidence",
    layout: "quote",
    title: "Impact on clinicians",
    body: "There is abundant evidence that ATLS training improves knowledge base, psychomotor skills, application of skills in resuscitation, and the confidence and performance of clinicians. The organizational and procedural skills taught in the course are retained by course participants for at least 6 years, which may be the most significant impact.",
    cite: "American College of Surgeons. Advanced Trauma Life Support® (ATLS®) Student Course Manual. 11th ed. 2025.",
  },
  {
    id: "patient-evidence",
    layout: "quote",
    title: "Impact on patients",
    body: "ATLS training in a developing country has resulted in a decrease in injury mortality. Lower-per-capita rates of deaths from injuries are observed in areas where clinicians have ATLS training. In one study, a small trauma care team led by a doctor with ATLS experience had equivalent patient survival when compared with a larger team with more doctors in an urban setting. In addition, there were more unexpected survivors than fatalities.",
    cite: "American College of Surgeons. Advanced Trauma Life Support® (ATLS®) Student Course Manual. 11th ed. 2025.",
  },
  {
    id: "systematic-reviews", layout: "columns", title: "What have systematic reviews found?",
    columns: [
      { heading: "Educational benefit; uncertain survival benefit", bullets: [
        "Mohammad et al. (2014): 23 studies; knowledge and skills improve, but strong mortality evidence is lacking¹",
        "Cochrane (2014): no eligible controlled trials of ATLS effects on mortality or morbidity²",
      ] },
      { heading: "Observational mortality evidence", bullets: [
        "Putra et al. (2023): seven studies; OR 0.68 (95% CI 0.39–1.20)³",
        "Nakhid et al. (2026): 17 observational studies of trauma life support training; OR 0.60 (95% CI 0.48–0.75)⁴",
      ] },
    ],
    footer: "1. World J Surg. 2014;38:322–329. 2. Cochrane Database Syst Rev. 2014;CD004173. 3. New Ropanasuri J Surg. 2023;8(2). 4. Scand J Trauma Resusc Emerg Med. 2026;34:15.",
  },
  {
    id: "atls-forest", layout: "forest", title: "Updated systematic review",
    footer: "Nakhid et al. Scand J Trauma Resusc Emerg Med. 2026;34:15, updated with Lule et al. (2026). Multiple training programmes; Lule evaluated RTTDC, not ATLS®.",
  },
  {
    id: "aim", layout: "aim", title: "Aim",
    body: "To compare the effects of ATLS® training with standard care on outcomes in adult trauma patients.",
  },
  {
    id: "design", layout: "design-animation", title: "Batched stepped-wedge cluster randomised trial",
  },
  {
    id: "setting", layout: "columns", title: "Setting and clusters",
    columns: [
      { heading: "Hospitals in India", bullets: [
        "30 secondary or tertiary hospitals providing trauma care",
        "Round-the-clock surgical and orthopaedic emergency services",
        "No more than 25% of initial-trauma-care physicians formally trained in trauma life support",
      ] },
      { heading: "Participating units", bullets: [
        "Usually one or two units per hospital",
        "At least 12 trauma admissions or transfers for admission per month",
        "Patient data collected on participating units’ working days",
      ] },
    ],
  },
  {
    id: "cluster-eligibility", layout: "columns", title: "Cluster eligibility in detail", detail: true,
    columns: [
      { heading: "Required", bullets: [
        "Hospital: at least 400 trauma admissions or transfers for admission per year, or 35 per month for at least six months",
        "Selected units: at least 12 per month for at least six months",
        "At both hospital and unit level: no more than 25% of initial-care physicians formally trained",
      ] },
      { heading: "Excluded", bullets: [
        "Implementation of another formal trauma life support programme",
        "Planned or ongoing major trauma-care changes during the trial",
        "Examples: new trauma centre, trauma team or trauma quality-improvement programme",
      ] },
    ],
  },
  {
    id: "eligibility", layout: "columns", title: "Patient eligibility",
    columns: [
      { heading: "All inclusion criteria", bullets: [
        "Aged 15 years or older; injury less than 48 hours before arrival",
        "Seen in the ED for trauma (ICD-10 chapter XX)",
        "Admitted, died between arrival and admission, or transferred from the ED for admission",
        "Managed by a participating ED unit",
      ] },
      { heading: "Exclusions", bullets: [
        "Isolated closed extremity fracture",
        "Direct ward admission without assessment by an ED physician",
      ] },
    ],
  },
  {
    id: "intervention", layout: "arms", title: "Intervention and comparator",
    arms: [
      { variant: "control", tag: "Control", title: "Standard care",
        body: "Usual initial trauma resuscitation by casualty medical officers or residents, before trial-provided ATLS training.",
        image: "./patient-review-before-illustration.png", imageAlt: "Clinical team providing initial trauma care" },
      { variant: "intervention", tag: "Intervention", title: "ATLS® training",
        body: "A 2.5-day course at an accredited facility in India. Around ten physicians per hospital, usually from one or two units.",
        image: "./training-illustration.png", imageAlt: "Instructor demonstrating trauma care during an ATLS course" },
    ],
  },
  {
    id: "training", layout: "columns", title: "Delivering the intervention", detail: true,
    columns: [
      { heading: "Who is trained?", bullets: [
        "Physicians providing initial resuscitation in the first hour",
        "Concentrate training within selected units rather than spreading it thinly across the hospital",
        "Select units by convenience; collect data on the same units’ working days before and after training",
      ] },
      { heading: "What is taught?", bullets: [
        "Primary and secondary surveys, resuscitation, triage and transfer",
        "Lectures, practical scenarios and a final proficiency evaluation",
        "Other clinical treatments remain at investigators’ discretion",
      ] },
    ],
  },
  {
    id: "allocation", layout: "columns", title: "Randomisation and masking",
    columns: [
      { heading: "Hospital-level allocation", bullets: [
        "Six batches of five hospitals; five implementation sequences",
        "One hospital per sequence in each batch",
        "Covariate-constrained randomisation balances monthly eligible-patient volume",
      ] },
      { heading: "Concealment and masking", bullets: [
        "Training order concealed as long as logistically possible",
        "Advance notification allows course arrangements",
        "Blinding is not possible in this pragmatic training trial",
      ] },
    ],
  },
  {
    id: "timeline", layout: "columns", title: "What happens at each hospital?",
    columns: [
      { heading: "13 months of inclusion", bullets: [
        "At least four months of standard care",
        "One-month transition phase for ATLS training",
        "At least four months after training; timing depends on randomised sequence",
      ] },
      { heading: "Follow-up and nested study", bullets: [
        "Each patient followed for 90 days from ED arrival",
        "Nested measurements in the three months before and three months after transition",
        "Transition-phase data excluded from the main analysis",
      ] },
    ],
  },
  {
    id: "primary-outcome", layout: "aim", title: "Primary outcome",
    body: "In-hospital mortality within 30 days of emergency department arrival.",
    footer: "Death during the initial hospital admission; distinct from all-cause 30-day mortality.",
  },
  {
    id: "secondary-outcomes", layout: "columns", title: "Secondary outcomes",
    columns: [
      { heading: "Main trial", bullets: [
        "All-cause mortality at 24 hours, 30 days and 90 days",
        "ED, ICU and hospital length of stay; ICU admission",
        "Return to work at 30 and 90 days",
      ] },
      { heading: "Nested staircase study", bullets: [
        "Adherence to ATLS principles during initial care",
        "Health-related quality of life: EQ-5D-5L",
        "Disability: WHODAS 2.0",
      ] },
    ],
  },
  {
    id: "schedule", layout: "schedule", title: "Schedule of patient assessments",
    schedule: [
      { activity: "Eligibility, injury and clinical characteristics", times: ["ED / admission", "—", "—", "—"] },
      { activity: "In-hospital primary outcome", times: ["Records through day 30 of initial admission", "—", "—", "—"] },
      { activity: "All-cause mortality", times: ["24 hours", "—", "Yes", "Yes"] },
      { activity: "Return to work", times: ["—", "—", "Yes", "Yes"] },
      { activity: "ATLS adherence · nested", times: ["First hour of physician care", "—", "—", "—"] },
      { activity: "EQ-5D-5L / WHODAS · nested", times: ["—", "Within 7 days", "Yes", "Yes"] },
    ],
    footer: "Time points are relative to ED arrival unless stated. EQ-5D-5L and WHODAS are assessed in survivors. Lengths of stay and ICU admission come from records.",
  },
  {
    id: "nested-methods", layout: "columns", title: "Nested study: sampling and measurement", detail: true,
    columns: [
      { heading: "Sampling around transition", bullets: [
        "Three months before and three months after transition",
        "Randomised research-coordinator shifts of approximately eight hours",
        "Target at least six patients per hospital per month in these periods",
      ] },
      { heading: "Observed care and recovery", bullets: [
        "14-item ATLS primary-survey checklist",
        "Observe up to one hour after first physician assessment",
        "EQ-5D-5L and WHODAS within seven days of discharge, at 30 days and at 90 days",
      ] },
    ],
  },
  {
    id: "sample-size", layout: "columns", title: "Sample size",
    columns: [
      { heading: "At least 4,320 patients for analysis", bullets: [
        "30 hospitals × 12 patients per month × 12 non-transition months",
        "Approximately 90% power for mortality of 20% versus 15%",
        "Five percentage-point absolute reduction; 25% relative reduction",
      ] },
      { heading: "Allowing for clustering", bullets: [
        "Assumed intracluster correlation: 0.02",
        "Cluster autocorrelation: 0.9; time-decay structure",
        "Variable cluster sizes considered in power calculations",
      ] },
    ],
    footer: "At the same recruitment rate across all 13 months: about 156 inclusions per hospital, of which 144 are outside transition.",
  },
  {
    id: "analysis", layout: "columns", title: "Statistical analysis",
    columns: [
      { heading: "Compare as scheduled", bullets: [
        "Analyse available observations regardless of training receipt",
        "Assign exposure by the scheduled transition date",
        "Exclude the one-month transition phase",
      ] },
      { heading: "Account for design and time", bullets: [
        "Mixed-effects models adjust for study month and hospital / hospital-period clustering",
        "Report odds ratios and, where models converge, risk differences with 95% confidence intervals",
        "Two-sided 5% significance level; secondary outcomes interpreted without multiplicity adjustment",
      ] },
    ],
  },
  {
    id: "analysis-detail", layout: "columns", title: "Analysis: robustness and heterogeneity", detail: true,
    columns: [
      { heading: "Sensitivity analyses", bullets: [
        "Alternative within-hospital time-correlation structures",
        "Batch-specific and flexible calendar-time trends",
        "Small-cluster correction and prognostic covariate adjustment",
      ] },
      { heading: "Effect heterogeneity", bullets: [
        "Hospital-specific intervention effects and time since training",
        "Subgroups by region, age, sex and clinical trauma cohort",
        "Detailed specifications belong in the statistical analysis plan",
      ] },
    ],
  },
  {
    id: "data", layout: "columns", title: "Data collection and quality",
    columns: [
      { heading: "At the hospital", bullets: [
        "Research coordinators collect records and clinical observations",
        "Follow up after discharge or transfer by telephone",
        "Follow-up telephone numbers retained locally",
      ] },
      { heading: "Central safeguards", bullets: [
        "Secure electronic case report form with VPN and two-factor authentication",
        "Central checks, statistical monitoring and selective site visits",
        "Source verification and at least ten years of archiving",
      ] },
    ],
  },
  {
    id: "consent", layout: "columns", title: "Ethics and consent for data collection",
    columns: [
      { heading: "Opt out", bullets: [
        "Routine records and observed adherence to ATLS principles",
        "Written information for patients or representatives",
        "May opt out before final analysis without affecting care",
      ] },
      { heading: "Opt in", bullets: [
        "Consent for non-routinely collected data",
        "Ages 15–18: assent plus guardian or representative consent",
      ] },
      { heading: "Waiver", bullets: [
        "When unable to consent and no legally authorised representative is available",
        "Seek consent when the patient or representative can provide it",
      ] },
    ],
    footer: "Ethical review and site permissions are required before participation. Consent provisions concern data collection, not allocation of physician training.",
  },
  {
    id: "safety", layout: "columns", title: "Safety monitoring",
    columns: [
      { heading: "Events tracked in both phases", bullets: [
        "Mechanical ventilation for more than seven days",
        "New renal replacement therapy",
        "Vasopressors for more than two days, or restarted after at least two days off",
        "Other unexpected serious complications causing life threat, prolonged admission or meaningful harm",
      ] },
      { heading: "Reporting and review", bullets: [
        "Report safety events to trial management within 24 hours",
        "Trial management assesses relatedness; local investigators follow events",
        "Probably trial-related events escalated immediately to the oversight committee",
      ] },
    ],
  },
  {
    id: "oversight", layout: "columns", title: "Trial oversight and dissemination",
    columns: [
      { heading: "Governance", bullets: [
        "Trial Team: daily operations",
        "Trial Management Group: clinical and practical conduct",
        "Joint Steering and Data Monitoring Committee: independent majority and independent chair",
        "Interim review focuses on safety and feasibility; no planned efficacy stopping",
      ] },
      { heading: "Transparency", bullets: [
        "Registered as NCT06321419 and CTRI/2024/07/071336",
        "Protocol-listed funding: Swedish Research Council and Laerdal Foundation",
        "Publish findings and make metadata and anonymised data available as planned in the protocol",
      ] },
    ],
  },
  {
    id: "interpretation", layout: "columns", title: "What will this trial tell us?",
    columns: [
      { heading: "Patient benefit", bullets: [
        "Does introducing ATLS training improve survival in this setting?",
        "Do changes in initial care translate into better recovery?",
      ] },
      { heading: "Policy and practice", bullets: [
        "Inform decisions about investing in trauma life support training",
        "Interpret effects alongside uncertainty, implementation and the participating hospitals’ context",
        "Avoid assuming that a non-significant result proves no benefit",
      ] },
    ],
  },
  {
    id: "closing", layout: "closing", title: "Questions?", subtitle: "advancetrauma.info",
    footer: "ADVANCE TRAUMA · ATLS® training compared with standard care",
    body: "ATLS® is a registered trademark of the American College of Surgeons.",
  },
];

export const briefSlideIds = [
  "title", "background", "atls-forest", "aim", "design", "setting", "eligibility", "intervention",
  "primary-outcome", "secondary-outcomes", "sample-size", "oversight", "closing",
];

export function selectSlides(view: string | null): Slide[] {
  if (view === "full") return allSlides;
  if (view === "brief") return allSlides.filter((slide) => briefSlideIds.includes(slide.id));
  return allSlides.filter((slide) => !slide.detail);
}

export const slides = selectSlides(new URLSearchParams(window.location.search).get("view"));
