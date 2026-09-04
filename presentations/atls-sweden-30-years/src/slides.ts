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
  image?: string;
  imageAlt?: string;
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
    | "status-map"
    | "columns"
    | "intervention"
    | "provocation";
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
    title: "Global burden",
    image: "./crash-illustration.png",
    imageAlt: "Illustration of a road traffic collision",
    imagePosition: "right",
    stats: [
      { value: "~5M", label: "deaths globally each year", source: "1" },
      { value: "$4.2T", label: "economic cost in the US alone", source: "2" },
      { value: "~2M", label: "quality-related deaths", source: "3" },
      { value: "#1", label: "disease burden, ages 10–49", source: "4" },
    ],
    references: [
      {
        id: "1",
        text: "~5 million injury deaths globally each year — Naghavi M et al. Lancet. 2025.",
      },
      {
        id: "2",
        text: "US injury economic cost ~$4.2 trillion (2019) — Peterson C et al. MMWR. 2021.",
      },
      {
        id: "3",
        text: "~2 million deaths annually from poor-quality care — National Academies. 2018.",
      },
      {
        id: "4",
        text: "Leading disease burden, ages 10–49 — GBD 2019 Collaborators. Lancet. 2020.",
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
    body: "The ATLS Course emphasizes rapid assessment and concurrent treatment of severely injured patients. Care starts at the time of injury and continues through initial assessment, lifesaving interventions, reevaluation, stabilization, and, when needed, transfer.",
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
      { value: ">1M", label: "clinicians trained" },
    ],
    footer: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-providers",
    layout: "quote",
    title: "Impact on clinicians' knowledge and skills",
    body: "There is abundant evidence that ATLS® training improves knowledge base, psychomotor skills, application of skills in resuscitation, and the confidence and performance of clinicians. The organizational and procedural skills taught in the course are retained by course participants for at least 6 years, which may be the most significant impact.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-provider-evidence",
    layout: "evidence",
    title: "Evidence on clinicians' knowledge and skills",
    evidence: [
      {
        id: "1",
        claim: "Improves knowledge, psychomotor skills, confidence, and resuscitation performance",
        source:
          "Ali J, Cohen R et al. World J Surg. 1996; J Trauma. 1994–1995.",
      },
      {
        id: "2",
        claim: "Trauma priorities retained ≥6 years; cognitive test scores decline",
        source:
          "Ali J, Cohen R. J Trauma. 1996;40:860–866.",
      },
    ],
  },
  {
    id: "atls-outcomes-claim",
    layout: "quote",
    title: "Impact on patient outcomes",
    body: "ATLS® training in a developing country has resulted in a decrease in injury mortality. Lower-per-capita rates of deaths from injuries are observed in areas where clinicians have ATLS training. In one study, a small trauma care team led by a doctor with ATLS experience had equivalent patient survival when compared with a larger team with more doctors in an urban setting. In addition, there were more unexpected survivors than fatalities.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-patient-impact-sources",
    layout: "evidence",
    title: "Evidence on patient outcomes",
    evidence: [
      {
        id: "1",
        claim: "Injury mortality fell after ATLS introduction (before–after, Trinidad)",
        source:
          "Ali J et al. J Trauma. 1993;34:890–899.",
      },
      {
        id: "2",
        claim: "Lower county trauma death rates where ATLS-trained physician density is higher (ecological)",
        source:
          "Rutledge R et al. Ann Surg. 1994;219:547–563.",
      },
      {
        id: "3",
        claim: "Small ATLS-experienced team matched survival of a larger urban team",
        source:
          "Deo SD et al. Injury. 1997;28:633–637.",
      },
      {
        id: "4",
        claim: "More unexpected survivors than fatalities (TRISS-based)",
        source:
          "van Olden GDJ et al. Am J Emerg Med. 2004;22:522–525.",
      },
    ],
  },
  {
    id: "atls-outcomes-reviews",
    layout: "bullets",
    title: "Evidence on patient outcomes — systematic reviews",
    bullets: [
      "Educational benefit established; strong mortality evidence still lacking — Mohammad et al. 2013",
      "No RCT evidence that ATLS changes mortality or morbidity — Jayaraman et al. 2014 (Cochrane)",
      "In-hospital trauma training associated with lower mortality (RR 0.71, 95% CI 0.62–0.78) — Jin et al. 2021",
      "ATLS not significantly associated with lower mortality (OR 0.68, 95% CI 0.39–1.20) — Putra et al. 2023",
      "Pooled observational association with lower mortality (OR 0.60, 95% CI 0.48–0.75); no RCTs — Nakhid et al. 2026",
    ],
    footer: "Knowledge and skills improve consistently; causal effect on mortality remains unproven.",
  },
  {
    id: "atls-forest",
    layout: "forest",
    title: "Evidence on patient outcomes — meta-analysis",
    subtitle: "Observational studies only · pooled odds ratio when all studies are included",
  },
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
    milestones: [
      {
        year: "2013",
        label: "Multicentre research",
        image: "./milestones/multicentre.png",
        imageAlt: "Map of India with hospital sites",
        cite: "1",
      },
      {
        year: "2022–2023",
        label: "Pilot and feasibility",
        image: "./milestones/pilot.png",
        imageAlt: "Clinicians discussing care in a hospital room",
        cite: "2",
      },
      {
        year: "2022–2023",
        label: "Community consultations",
        image: "./milestones/consultations.png",
        imageAlt: "Patient bedside discussion about ATLS",
        cite: "3",
      },
      {
        year: "2022–2026",
        label: "Systematic review",
        image: "./milestones/systematic-review.png",
        imageAlt: "Nakhid et al. systematic review article in press",
        cite: "4",
      },
    ],
    references: [
      {
        id: "1",
        text: "Multicentre trauma research platform in India — TITCO Consortium. titco.org.",
      },
      {
        id: "2",
        text: "Cluster RCT of ATLS feasible; high consent and low loss to follow-up — Gerdin Wärnberg M et al. BMJ Open. 2025.",
      },
      {
        id: "3",
        text: "Patient-reported outcome measures for Indian trauma (preprint) — David S et al. medRxiv. 2024.",
      },
      {
        id: "4",
        text: "Trauma life support training associated with lower mortality in observational meta-analysis — Nakhid Z et al. Scand J Trauma Resusc Emerg Med. 2026.",
      },
    ],
  },
  {
    id: "design",
    layout: "design",
    title: "Study design",
    designVariant: "main",
    body: "Batched stepped-wedge cluster randomised trial in India",
    stats: [
      { value: "30", label: "hospitals" },
      { value: "6", label: "batches" },
      { value: "5", label: "sequences" },
      { value: "13", label: "months in trial" },
    ],
    footer: "India: long-standing collaboration; ATLS® is not yet standard care",
  },
  {
    id: "design-animation",
    layout: "design-animation",
    title: "How the trial unfolds",
    designVariant: "main",
  },
  {
    id: "intervention",
    layout: "intervention",
    title: "Intervention and control",
    columns: [
      {
        heading: "Standard care",
        image: "./patient-review-after-illustration.png",
        imageAlt: "Trauma team managing a patient under standard care",
        bullets: [
          "Trauma patients initially managed by 1st/2nd year residents without formal trauma training",
        ],
      },
      {
        heading: "ATLS® training",
        image: "./training-illustration.png",
        imageAlt: "ATLS training session with instructor and students",
        bullets: [
          "2.5-day ATLS® course at an accredited facility; 1–2 units per hospital trained",
        ],
      },
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
          "Admitted, die before admission, or transferred for admission",
          "Less than 48 hours since trauma",
        ],
      },
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
    layout: "status-map",
    title: "Current status",
    subtitle: "10 of 30 hospitals enrolled",
    stats: [
      { value: "Batch 1", label: "complete · Feb 2025–Mar 2026" },
      { value: ">2,000", label: "injury records to date" },
      { value: "Batch 2", label: "in progress · from Dec 2025" },
      { value: "Batch 3", label: "starting" },
    ],
    footer: "Expected completion December 2028, pending funding",
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
          { name: "Vivekanand Jha, Nobhojit Roy", role: "Co-Principal Investigators" },
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
  },
  {
    id: "funding",
    layout: "funding",
    title: "Funding",
    funders: [
      { name: "Swedish Research Council" },
      { name: "Laerdal Foundation" },
      { name: "Region Stockholm" },
      { name: "Swedish Society of Medicine" },
    ],
    footer: "Additional support is needed to complete the trial through 2028–2029.",
  },
  {
    id: "closing",
    layout: "closing",
    title: "Thank you",
    subtitle: "advancetrauma.info",
    footer: "ADVANCE TRAUMA trial (NCT06321419)",
  },
];
