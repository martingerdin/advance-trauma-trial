export interface Stat {
  value: string;
  label: string;
  source?: string;
}

export interface Slide {
  id: string;
  layout:
    | "title"
    | "section"
    | "stats"
    | "quote"
    | "bullets"
    | "two-col"
    | "visual"
    | "design"
    | "implications"
    | "closing";
  title?: string;
  subtitle?: string;
  eyebrow?: string;
  body?: string;
  bullets?: string[];
  stats?: Stat[];
  image?: string;
  imageAlt?: string;
  imagePosition?: "left" | "right" | "background";
  cite?: string;
  footer?: string;
}

export const slides: Slide[] = [
  {
    id: "title",
    layout: "title",
    title: "ADVANCE TRAUMA",
    subtitle:
      "Effects of Advanced Trauma Life Support® Training Compared to Standard Care on Adult Trauma Patient Outcomes",
    eyebrow: "Swedish ATLS Chapter — 30 Years · Region 15",
    footer: "NCT06321419 · advancetrauma.info",
  },
  {
    id: "visual-abstract",
    layout: "visual",
    title: "How does ATLS training impact patient outcomes?",
    image: "./advance-trauma-visual-abstract.png",
    imageAlt:
      "Visual abstract of the ADVANCE TRAUMA stepped-wedge cluster randomised trial in India",
    imagePosition: "background",
  },
  {
    id: "section-problem",
    layout: "section",
    title: "Trauma",
    subtitle: "Scope of the problem",
  },
  {
    id: "trauma-stats",
    layout: "stats",
    title: "Scope of the problem",
    image: "./crash-illustration.png",
    imageAlt: "Illustration of a road traffic collision",
    imagePosition: "right",
    stats: [
      { value: ">4M", label: "deaths globally each year", source: "1" },
      { value: "$4.2T", label: "economic cost in the US alone", source: "2" },
      { value: "~2M", label: "quality-related deaths", source: "3" },
      { value: "#1", label: "disease burden, ages 10–49", source: "4" },
    ],
  },
  {
    id: "section-atls",
    layout: "section",
    title: "ATLS®",
    subtitle: "The most widely adopted trauma life support programme",
  },
  {
    id: "atls-purpose",
    layout: "quote",
    title: "Purpose and content",
    body: "Emphasizes the rapid initial assessment and primary treatment of injured patients, starting at the time of injury and continuing through initial assessment, lifesaving intervention, reevaluation, stabilization, and, when needed, transfer to a trauma center.",
    cite: "ATLS® Student Course Manual, 10th ed. 2018",
  },
  {
    id: "atls-spread",
    layout: "two-col",
    title: "Spread and dissemination",
    image: "./training-illustration.png",
    imageAlt: "ATLS training session with instructor and students",
    imagePosition: "right",
    stats: [
      { value: "1978", label: "first course" },
      { value: ">80", label: "countries worldwide" },
      { value: ">1M", label: "physicians trained" },
    ],
  },
  {
    id: "atls-providers",
    layout: "quote",
    title: "Impact on providers' knowledge and skills",
    body: "There is abundant evidence that ATLS training improves the knowledge base, the psychomotor skills and their use in resuscitation, and the confidence and performance of doctors who have taken part in the program. The organization and procedural skills taught in the course are retained by course participants for at least 6 years.",
    cite: "ATLS® Student Course Manual, 10th ed. 2018",
  },
  {
    id: "atls-provider-evidence",
    layout: "bullets",
    title: "Evidence on providers",
    bullets: [
      "Ali et al. 1995 — trauma management skills acquisition demonstrated after ATLS course",
      "Ali et al. 1996 — improvement in OSCE scores, adherence to trauma priorities, and cognitive performance",
      "Ali et al. 1999 — performance after new and old ATLS courses was similar using standard pass criteria",
    ],
  },
  {
    id: "atls-outcomes-claim",
    layout: "quote",
    title: "Impact on patient outcomes",
    body: "ATLS training in a developing country has resulted in a decrease in injury mortality. Lower per capita rates of deaths from injuries are observed in areas where providers have ATLS training. In one study, a small trauma team led by a doctor with ATLS experience had equivalent patient survival when compared with a larger team.",
    cite: "ATLS® Student Course Manual, 10th ed. 2018",
  },
  {
    id: "atls-outcomes-reviews",
    layout: "bullets",
    title: "Evidence on patient outcomes",
    bullets: [
      "Mohammad et al. 2013 — future studies required to evaluate impact on trauma death rates",
      "Jayaraman et al. 2014 — no evidence from controlled trials that ATLS impacts outcomes",
      "Jin et al. 2021 — in-hospital trauma training reduced mortality (RR 0.71, 95% CI 0.62–0.78)",
      "Putra et al. 2023 — ATLS had no significant effect on mortality (OR 0.68, 95% CI 0.39–1.20)",
      "Nakhid et al. 2026 — 10 observational studies; pooled OR 0.51",
    ],
  },
  {
    id: "atls-critique",
    layout: "bullets",
    title: "Critique",
    bullets: [
      "Costly",
      "Perpetuates theories despite evidence of the contrary",
      "Not adapted to modern trauma care",
      "Not adaptable to local circumstances",
      "Fixed didactic nature",
    ],
    cite: "Shilston & Turner 2022; Wiles 2015",
  },
  {
    id: "section-trial",
    layout: "section",
    title: "ADVANCE TRAUMA",
    subtitle: "A stepped-wedge cluster randomised trial",
  },
  {
    id: "aim",
    layout: "bullets",
    title: "Aim",
    bullets: [
      "To compare the effects of ATLS® training with standard care on outcomes in adult trauma patients",
    ],
  },
  {
    id: "previous-work",
    layout: "bullets",
    title: "Previous work",
    bullets: [
      "2013 — Multicentre research (TITCO)",
      "2022–2023 — Community consultations",
      "2022–2023 — Pilot and feasibility study in India",
      "2022–2026 — Systematic review",
    ],
  },
  {
    id: "design",
    layout: "design",
    title: "Study design",
    bullets: [
      "Batched stepped-wedge cluster randomised trial",
      "30 hospitals · 6 batches · 5 sequences · 13 months in trial",
      "Conducted in India — ongoing collaborations >10 years; ATLS not yet standard",
    ],
  },
  {
    id: "intervention",
    layout: "two-col",
    title: "Intervention and control",
    image: "./patient-review-before-illustration.png",
    imageAlt: "Trauma team managing a patient in the emergency department",
    imagePosition: "right",
    bullets: [
      "Control — standard care; trauma patients initially managed by 1st/2nd year residents without formal trauma training",
      "Intervention — 2.5-day ATLS® course at accredited facility; 1–2 units per hospital trained",
    ],
  },
  {
    id: "primary-outcome",
    layout: "bullets",
    title: "Primary outcome",
    bullets: [
      "30-day in-hospital mortality",
      "Collected through medical records for patients admitted or discharged home",
      "Collected through telephonic follow-up for patients transferred to another hospital",
    ],
  },
  {
    id: "secondary-outcomes",
    layout: "bullets",
    title: "Secondary outcomes",
    bullets: [
      "All-cause and in-hospital mortality at 24 h, 30 days, and 90 days",
      "Length of stay in ED, ICU, and hospital",
      "Return to work at 30 and 90 days",
      "Adherence to ATLS principles (nested staircase design)",
      "Quality of life (EQ-5D-5L) and disability (WHODAS 2.0) at 30 and 90 days",
    ],
  },
  {
    id: "sample-size",
    layout: "stats",
    title: "Sample size",
    stats: [
      { value: "20→15%", label: "mortality reduction to detect" },
      { value: "90%", label: "statistical power" },
      { value: "30", label: "hospital clusters" },
      { value: ">4,320", label: "patients required" },
    ],
  },
  {
    id: "current-status",
    layout: "stats",
    title: "Current status",
    image: "./training-illustration.png",
    imageAlt: "ATLS training in progress",
    imagePosition: "right",
    stats: [
      { value: "Feb 2025", label: "first batch ongoing" },
      { value: "~1,200", label: "patients included" },
      { value: "Dec 2025", label: "second batch started" },
      { value: "May 2026", label: "third batch planned" },
    ],
    footer: "Expected completion December 2028, pending funding",
  },
  {
    id: "implications",
    layout: "implications",
    title: "Implications",
  },
  {
    id: "closing",
    layout: "closing",
    title: "Thank you",
    subtitle: "advancetrauma.info",
    footer: "ADVANCE TRAUMA trial (NCT06321419)",
  },
];
