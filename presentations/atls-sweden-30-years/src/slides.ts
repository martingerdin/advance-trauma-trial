export interface Reference {
  id: string;
  text: string;
}

export interface EvidenceItem {
  id: string;
  claim: string;
  source: string;
  tag?: string;
}

export interface Stat {
  value: string;
  label: string;
  source?: string;
}

export interface Milestone {
  year: string;
  label: string;
  image?: string;
  imageAlt?: string;
  cite?: string;
}

export interface OutcomeItem {
  title: string;
  detail?: string;
  tag?: string;
}

export interface TeamMember {
  name: string;
  role: string;
}

export interface TeamGroup {
  label: string;
  location: string;
  members: TeamMember[];
}

export interface Funder {
  name: string;
}

export interface ColumnGroup {
  heading: string;
  bullets: string[];
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
    | "design-animation"
    | "sequences"
    | "forest"
    | "implications"
    | "closing"
    | "references"
    | "evidence"
    | "milestones"
    | "aim"
    | "outcomes"
    | "presenter"
    | "team"
    | "funding"
    | "sites-map"
    | "columns";
  title?: string;
  subtitle?: string;
  eyebrow?: string;
  body?: string;
  bullets?: string[];
  affiliations?: string[];
  stats?: Stat[];
  image?: string;
  imageAlt?: string;
  imagePosition?: "left" | "right" | "background";
  cite?: string;
  footer?: string;
  references?: Reference[];
  evidence?: EvidenceItem[];
  milestones?: Milestone[];
  outcomes?: OutcomeItem[];
  teamGroups?: TeamGroup[];
  funders?: Funder[];
  columns?: ColumnGroup[];
  /** Which exported trial-design JSON to render on design slides. */
  designVariant?: "main" | "staircase";
}

const ATLS_MANUAL_CITE =
  "American College of Surgeons. Advanced Trauma Life Support® (ATLS®) Student Course Manual. 11th ed. 2025.";

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
    id: "presenter",
    layout: "presenter",
    title: "Martin Gerdin Wärnberg",
    subtitle: "MD, PhD",
    eyebrow: "Speaker",
    bullets: [
      "Principal Investigator, ADVANCE TRAUMA",
      "Associate Professor of Clinical Epidemiology, Karolinska Institutet",
      "Specialist Physician in Anaesthesia and Intensive Care, Karolinska University Hospital",
    ],
    affiliations: [
      "Department of Global Public Health, Karolinska Institutet, Stockholm",
      "Perioperative Medicine and Intensive Care, Karolinska University Hospital, Solna",
    ],
    body: "Nothing to declare",
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
    references: [
      {
        id: "1",
        text: "Naghavi M et al. Global burden of 292 causes of death in 204 countries and territories, 1990–2023. Lancet. 2025.",
      },
      {
        id: "2",
        text: "Peterson C et al. Economic Cost of Injury — United States, 2019. MMWR Morb Mortal Wkly Rep. 2021.",
      },
      {
        id: "3",
        text: "National Academies of Sciences, Engineering, and Medicine. Crossing the Global Quality Chasm: Improving Health Care Worldwide. 2018.",
      },
      {
        id: "4",
        text: "GBD 2019 Diseases and Injuries Collaborators. Global burden of 369 diseases and injuries, 1990–2019. Lancet. 2020.",
      },
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
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-spread",
    layout: "stats",
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
    title: "Impact on clinicians' knowledge and skills",
    body: "There is abundant evidence that ATLS training improves knowledge base, psychomotor skills, application of skills in resuscitation, and the confidence and performance of clinicians. The organizational and procedural skills taught in the course are retained by course participants for at least 6 years, which may be the most significant impact.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-provider-evidence",
    layout: "evidence",
    title: "Evidence on providers",
    body: "The 11th edition Impact paragraph cites no sources — here is what the literature shows.",
    evidence: [
      {
        id: "1",
        tag: "Manual claim",
        claim: "Improves knowledge, psychomotor skills, confidence, and performance",
        source:
          "Ali J, Cohen R et al. World J Surg. 1996;20:1121–1125; J Trauma. 1994;36:695–702; J Trauma. 1995;38:687–691.",
      },
      {
        id: "2",
        tag: "Manual claim",
        claim: "Organizational and procedural skills retained ≥6 years",
        source:
          "Ali J, Cohen R et al. Attrition of cognitive and trauma management skills after ATLS. J Trauma. 1996;40:860–866.",
      },
      {
        id: "3",
        tag: "Further studies",
        claim: "Trauma management skills acquisition after ATLS course",
        source: "Ali et al. 1995",
      },
      {
        id: "4",
        tag: "Further studies",
        claim: "Improvement in OSCE scores, adherence to priorities, and cognitive performance",
        source: "Ali et al. 1996",
      },
      {
        id: "5",
        tag: "Further studies",
        claim: "Similar performance after new and old ATLS courses",
        source: "Ali et al. 1999",
      },
    ],
    footer:
      "Note: the 6-year retention paper found cognitive scores decline while adherence to priorities is preserved.",
  },
  {
    id: "atls-outcomes-claim",
    layout: "quote",
    title: "Impact on patient outcomes",
    body: "ATLS training in a developing country has resulted in a decrease in injury mortality. Lower-per-capita rates of deaths from injuries are observed in areas where clinicians have ATLS training. In one study, a small trauma care team led by a doctor with ATLS experience had equivalent patient survival when compared with a larger team with more doctors in an urban setting. In addition, there were more unexpected survivors than fatalities.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-patient-impact-sources",
    layout: "evidence",
    title: "The evidence behind the claims",
    body: "Each claim from the manual's Impact section — and the study behind it.",
    evidence: [
      {
        id: "1",
        claim: "Decreased injury mortality in a developing country",
        source:
          "Ali J et al. Trauma outcome improves following ATLS in a developing country. J Trauma. 1993;34:890–899.",
      },
      {
        id: "2",
        claim: "Lower per-capita injury death rates where clinicians have ATLS training",
        source:
          "Rutledge R et al. Association of medical manpower with county trauma death rates. Ann Surg. 1994;219:547–563.",
      },
      {
        id: "3",
        claim: "Small ATLS-experienced team equivalent to a larger urban team",
        source:
          "Deo SD et al. Evaluation of a small trauma team for major resuscitation. Injury. 1997;28:633–637.",
      },
      {
        id: "4",
        claim: "More unexpected survivors than fatalities",
        source:
          "van Olden GDJ et al. Clinical impact of advanced trauma life support. Am J Emerg Med. 2004;22:522–525.",
      },
    ],
  },
  {
    id: "atls-outcomes-reviews",
    layout: "bullets",
    title: "Evidence on patient outcomes — systematic reviews",
    bullets: [
      "Mohammad et al. 2013 — educational impact established; strong evidence that ATLS reduces mortality still lacking",
      "Jayaraman et al. 2014 (Cochrane) — no controlled-trial evidence that ATLS changes mortality or morbidity",
      "Jin et al. 2021 — certified in-hospital trauma training associated with lower mortality (RR 0.71, 95% CI 0.62–0.78)",
      "Putra et al. 2023 — ATLS not significantly associated with lower mortality (OR 0.68, 95% CI 0.39–1.20)",
      "Nakhid et al. 2026 — trauma life support training associated with lower mortality (OR 0.60, 95% CI 0.48–0.75); all observational",
    ],
  },
  {
    id: "atls-outcomes-scoping",
    layout: "bullets",
    title: "Evidence on patient outcomes — scoping reviews",
    bullets: [
      "Livergant et al. 2021 — only 3 of 45 LMIC course studies assessed patient outcomes; 2 found no improvement",
      "Brown et al. 2022 — among ATLS alternatives, only one study reported a mortality reduction",
      "Nelson et al. 2026 — 3 of 14 ED-physician studies assessed clinical impact; results mixed",
      "Hauta et al. 2024 — team performance often improves; mortality, morbidity and length of stay generally do not",
      "Petroze 2014 / Osebo 2025 — patient-outcome data for focused LMIC trauma courses remain scarce",
    ],
    footer:
      "Across reviews: knowledge and skills improve; randomised evidence that training changes patient outcomes is still missing.",
  },
  {
    id: "atls-forest",
    layout: "forest",
    title: "Updated systematic review",
    subtitle: "Nakhid et al. 2026 — trauma life support training and mortality",
  },
  // {
  //   id: "atls-critique",
  //   layout: "bullets",
  //   title: "Critique",
  //   bullets: [
  //     "Costly",
  //     "Perpetuates theories despite evidence of the contrary",
  //     "Not adapted to modern trauma care",
  //     "Not adaptable to local circumstances",
  //     "Fixed didactic nature",
  //   ],
  //   cite: "Shilston & Turner 2022; Wiles 2015",
  // },
  {
    id: "section-trial",
    layout: "section",
    title: "ADVANCE TRAUMA",
    subtitle: "A stepped-wedge cluster randomised trial",
  },
  {
    id: "aim",
    layout: "aim",
    title: "Aim",
    body: "To compare the effects of ATLS® training with standard care on outcomes in adult trauma patients",
  },
  {
    id: "previous-work",
    layout: "milestones",
    title: "Previous work",
    subtitle: "Key references",
    milestones: [
      {
        year: "2013",
        label: "Multicentre research",
        image: "./milestones/multicentre.png",
        imageAlt: "Map of India with hospital sites",
        cite: "12",
      },
      {
        year: "2022–2023",
        label: "Pilot and feasibility study",
        image: "./milestones/pilot.png",
        imageAlt: "Clinicians discussing care in a hospital room",
        cite: "14",
      },
      {
        year: "2022–2023",
        label: "Community consultations",
        image: "./milestones/consultations.png",
        imageAlt: "Patient bedside discussion about ATLS",
        cite: "13",
      },
      {
        year: "2022–2026",
        label: "Systematic review",
        image: "./milestones/systematic-review.png",
        imageAlt: "Nakhid et al. systematic review article in press",
        cite: "5",
      },
    ],
  },
  {
    id: "design",
    layout: "design",
    title: "Study design",
    designVariant: "main",
    bullets: [
      "Batched stepped-wedge cluster randomised trial",
      "30 hospitals · 6 batches · 5 sequences · 13 months in trial",
      "Conducted in India — ongoing collaborations >10 years; ATLS not yet standard",
    ],
  },
  {
    id: "design-animation",
    layout: "design-animation",
    title: "How the trial unfolds",
    designVariant: "main",
  },
  {
    id: "sequences",
    layout: "sequences",
    title: "Randomisation to sequences",
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
    layout: "outcomes",
    title: "Primary outcome",
    body: "30-day in-hospital mortality",
    outcomes: [
      {
        tag: "Medical records",
        title: "Patients admitted or discharged home",
        detail: "Extracted from hospital records during the initial admission",
      },
      {
        tag: "Telephonic follow-up",
        title: "Patients transferred to another hospital",
        detail: "Collected by calling the patient, representative, or receiving hospital",
      },
    ],
  },
  {
    id: "secondary-outcomes",
    layout: "outcomes",
    title: "Secondary outcomes",
    outcomes: [
      {
        tag: "Mortality",
        title: "All-cause and in-hospital mortality",
        detail: "24 hours · 30 days · 90 days",
      },
      {
        tag: "Length of stay",
        title: "ED, ICU, and hospital stay",
        detail: "From patient hospital records",
      },
      {
        tag: "Recovery",
        title: "Return to work",
        detail: "30 and 90 days after arrival",
      },
      {
        tag: "Process",
        title: "Adherence to ATLS principles",
        detail: "Nested staircase design",
      },
      {
        tag: "Patient-reported",
        title: "Quality of life and disability",
        detail: "EQ-5D-5L · WHODAS 2.0 · 30 and 90 days",
      },
    ],
  },
  {
    id: "design-staircase",
    layout: "design",
    title: "Nested staircase design",
    designVariant: "staircase",
    bullets: [
      "Adherence measured around each hospital’s transition to ATLS®",
      "Pre- and post-transition staircase periods nested in the stepped wedge",
      "Lets us compare process adherence before and after training within clusters",
    ],
  },
  {
    id: "eligibility",
    layout: "columns",
    title: "Eligibility criteria",
    columns: [
      {
        heading: "Cluster",
        bullets: [
          "Hospitals that admit or refer/transfer for admission at least 400 patients with trauma per year",
          "Around-the-clock emergency surgical and orthopaedic services",
        ],
      },
      {
        heading: "Patient",
        bullets: [
          "Adult trauma patients presenting to the emergency department of participating hospitals with a history of trauma",
          "Admitted, dies before admission, or transferred for admission",
          "Less than 48 hours since trauma",
        ],
      },
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
    id: "participating-clusters",
    layout: "sites-map",
    title: "Participating clusters",
    subtitle: "Hospitals currently in the trial",
    footer: "Batches 1–2 confirmed · Batches 3–6 screening · Full list at advancetrauma.info",
  },
  {
    id: "implications",
    layout: "implications",
    title: "Implications",
  },
  {
    id: "team",
    layout: "team",
    title: "The team",
    subtitle: "An international collaboration",
    teamGroups: [
      {
        label: "Karolinska Institutet",
        location: "Stockholm, Sweden",
        members: [
          { name: "Martin Gerdin Wärnberg", role: "Principal Investigator" },
          { name: "Anna Olofsson", role: "Trial Statistician" },
          { name: "Lovisa Strömmer, Li Felländer-Tsai, Johanna Berg", role: "TMG members" },
        ],
      },
      {
        label: "The George Institute for Global Health",
        location: "New Delhi, India",
        members: [
          { name: "Vivekanand Jha, Nobhojit Roy", role: "Co-principal Investigators" },
          { name: "Prashant Kharat, Debojit Basak, Monty Khajanchi, Abhinav Bassi", role: "Trial operations" },
          { name: "Hospital investigators & CRCs", role: "Sites across India" },
        ],
      },
      {
        label: "Methods partners",
        location: "Birmingham · Melbourne",
        members: [
          { name: "Karla Hemming", role: "University of Birmingham" },
          { name: "James Martin", role: "University of Birmingham" },
          { name: "Jessica Kasza", role: "Monash University" },
        ],
      },
    ],
    footer: "Full team and site list at advancetrauma.info",
  },
  {
    id: "funding",
    layout: "funding",
    title: "Funding",
    body: "Current support for ADVANCE TRAUMA",
    funders: [
      { name: "Swedish Research Council" },
      { name: "Laerdal Foundation" },
      { name: "Region Stockholm" },
      { name: "Swedish Society of Medicine" },
    ],
    footer:
      "Not yet fully funded — additional support is needed to complete the trial through 2028–2029.",
  },
  {
    id: "closing",
    layout: "closing",
    title: "Thank you",
    subtitle: "advancetrauma.info",
    footer: "ADVANCE TRAUMA trial (NCT06321419)",
  },
];
