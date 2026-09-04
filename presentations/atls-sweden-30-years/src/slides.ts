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

export interface TrialArm {
  variant: "control" | "intervention";
  tag: string;
  title: string;
  body: string;
  image: string;
  imageAlt: string;
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
    | "arms"
    | "design"
    | "design-animation"
    | "sequences"
    | "forest"
    | "implications"
    | "closing"
    | "evidence"
    | "milestones"
    | "aim"
    | "outcomes"
    | "outcome-list"
    | "presenter"
    | "team"
    | "funding"
    | "status-map"
    | "columns"
    | "provocation";
  title?: string;
  subtitle?: string;
  eyebrow?: string;
  body?: string;
  bullets?: string[];
  affiliations?: string[];
  stats?: Stat[];
  arms?: TrialArm[];
  image?: string;
  imageAlt?: string;
  imagePosition?: "left" | "right" | "background";
  cite?: string;
  /** Attribution shown directly under the heading, above the content. */
  source?: string;
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
    body: "Research support from the Swedish Research Council, the Laerdal Foundation, Region Stockholm, and the Swedish Society of Medicine. The trial pays for accredited ATLS® courses. No personal financial interests.",
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
      { value: "4.3M", label: "deaths globally each year", source: "1" },
      { value: "$4.2T", label: "economic cost in the US alone", source: "2" },
      { value: "~2M", label: "deaths from poor-quality care", source: "3" },
      { value: "#1", label: "cause of lost healthy life, ages 10–49", source: "4" },
    ],
    references: [
      { id: "1", text: "Naghavi M et al. Lancet. 2025." },
      { id: "2", text: "Peterson C et al. MMWR Morb Mortal Wkly Rep. 2021;70:1655–1659." },
      {
        id: "3",
        text: "National Academies of Sciences, Engineering, and Medicine. Crossing the Global Quality Chasm. 2018.",
      },
      { id: "4", text: "GBD 2019 Diseases and Injuries Collaborators. Lancet. 2020;396:1204–1222." },
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
      { value: ">1M", label: "physicians trained" },
    ],
    footer: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-providers",
    layout: "quote",
    title: "Impact on clinicians",
    body: "There is abundant evidence that ATLS® training improves knowledge base, psychomotor skills, application of skills in resuscitation, and the confidence and performance of clinicians. The organizational and procedural skills taught in the course are retained by course participants for at least 6 years, which may be the most significant impact.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-provider-evidence",
    layout: "evidence",
    title: "Evidence on clinicians",
    evidence: [
      {
        id: "1",
        tag: "Three randomised studies",
        claim: "Knowledge and trauma management skills improve after the course",
        source:
          "Ali J et al. J Trauma. 1995;38:687–691; World J Surg. 1996;20:1121–1126; J Trauma. 1999;46:80–86.",
      },
      {
        id: "2",
        tag: "60 physicians, six-year follow-up",
        claim:
          "Priorities and an organised approach hold at six years, but cognitive test scores fall within six months",
        source:
          "Ali J et al. Attrition of cognitive and trauma management skills after the ATLS course. J Trauma. 1996;40:860–866.",
      },
    ],
  },
  {
    id: "atls-outcomes-claim",
    layout: "quote",
    title: "Impact on patients",
    body: "ATLS® training in a developing country has resulted in a decrease in injury mortality. Lower-per-capita rates of deaths from injuries are observed in areas where clinicians have ATLS training. In one study, a small trauma care team led by a doctor with ATLS experience had equivalent patient survival when compared with a larger team with more doctors in an urban setting. In addition, there were more unexpected survivors than fatalities.",
    cite: ATLS_MANUAL_CITE,
  },
  {
    id: "atls-patient-impact-sources",
    layout: "evidence",
    title: "Evidence on patients",
    evidence: [
      {
        id: "1",
        tag: "Before-and-after cohort",
        claim: "Injury mortality fell after ATLS® training was introduced",
        source:
          "Ali J et al. Trauma outcome improves following ATLS in a developing country. J Trauma. 1993;34:890–899.",
      },
      {
        id: "2",
        tag: "Ecological, US counties",
        claim: "Counties with more ATLS-trained physicians had fewer injury deaths per capita",
        source: "Rutledge R et al. Ann Surg. 1994;219:547–563.",
      },
      {
        id: "3",
        tag: "77 patients, TRISS",
        claim: "A small trauma team matched a larger one, with more unexpected survivors than deaths",
        source: "Deo SD, Knottenbelt JD, Peden MM. Injury. 1997;28:633–637.",
      },
      {
        id: "4",
        tag: "63 patients",
        claim: "Deaths in the first hour fell after ATLS®; overall mortality unchanged",
        source: "van Olden GDJ et al. Am J Emerg Med. 2004;22:522–525.",
      },
    ],
  },
  {
    id: "atls-outcomes-reviews",
    layout: "evidence",
    title: "Systematic reviews",
    evidence: [
      {
        id: "1",
        tag: "23 studies",
        claim: "Knowledge and skills clearly improve; strong evidence on mortality still lacking",
        source: "Mohammad A et al. World J Surg. 2014;38:322–329.",
      },
      {
        id: "2",
        tag: "No eligible trials",
        claim: "No controlled trial has tested whether ATLS® changes mortality or morbidity",
        source: "Jayaraman S et al. Cochrane Database Syst Rev. 2014;(8):CD004173.",
      },
      {
        id: "3",
        tag: "Quality improvement in LMICs",
        claim:
          "Lower mortality with certified in-hospital trauma training, RR 0.71 (95% CI 0.62–0.78)",
        source: "Jin J et al. World J Surg. 2021;45:1982–1998.",
      },
      {
        id: "4",
        tag: "7 studies",
        claim: "No significant association with lower mortality, OR 0.68 (95% CI 0.39–1.20)",
        source: "Putra AB et al. New Ropanasuri J Surg. 2023;8:2.",
      },
      {
        id: "5",
        tag: "17 studies, all observational",
        claim:
          "Lower mortality with trauma life support training, OR 0.60 (95% CI 0.48–0.75)",
        source: "Nakhid Z et al. Scand J Trauma Resusc Emerg Med. 2026.",
      },
    ],
    footer:
      "Jin et al. report risk ratios; the others report odds ratios. No review has found randomised evidence on ATLS® itself.",
  },
  {
    id: "atls-forest",
    layout: "forest",
    title: "Updated systematic review",
    source:
      "Nakhid et al. 2026 (17 observational studies), plus Lule et al. 2026 — a cluster randomised trial of RTTDC, not ATLS®.",
  },
  {
    id: "hook",
    layout: "provocation",
    body: "Thirty years of ATLS® in Sweden. No randomised trial has tested whether it saves lives.",
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
        label: "A multicentre trauma cohort across India",
        image: "./milestones/multicentre.png",
        imageAlt: "Map of India with hospital sites",
        cite: "1",
      },
      {
        year: "2022–2023",
        label: "A full-scale trial is feasible",
        image: "./milestones/pilot.png",
        imageAlt: "Clinicians discussing care in a hospital room",
        cite: "2",
      },
      {
        year: "2022–2023",
        label: "Patients told us which outcomes matter",
        image: "./milestones/consultations.png",
        imageAlt: "Patient bedside discussion about ATLS",
        cite: "3",
      },
    ],
    references: [
      { id: "1", text: "TITCO Consortium. Towards Improved Trauma Care Outcomes in India. www.titco.org" },
      { id: "2", text: "Gerdin Wärnberg M et al. BMJ Open. 2025;15:e099020." },
      { id: "3", text: "David S, Gerdin Wärnberg M, TERN Collaborators. medRxiv. 2024." },
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
      { value: "13", label: "months per hospital" },
    ],
    footer:
      "ATLS® is not yet standard care in these hospitals, which is why a randomised comparison there is still ethical. We have collaborated with them for more than ten years.",
  },
  {
    id: "design-animation",
    layout: "design-animation",
    title: "How the trial unfolds",
    designVariant: "main",
  },
  {
    id: "eligibility",
    layout: "columns",
    title: "Eligibility criteria",
    columns: [
      {
        heading: "Cluster",
        bullets: [
          "Admits or refers for admission at least 400 patients with trauma per year",
          "Around-the-clock emergency surgical and orthopaedic services",
        ],
      },
      {
        heading: "Patient",
        bullets: [
          "Adult patients presenting to the emergency department with a history of trauma",
          "Admitted, dies before admission, or transferred for admission",
          "Less than 48 hours since trauma",
        ],
      },
    ],
  },
  {
    id: "intervention",
    layout: "arms",
    title: "Intervention and control",
    arms: [
      {
        variant: "control",
        tag: "Control",
        title: "Standard care",
        body: "Trauma patients are initially managed by first- and second-year residents without formal trauma training.",
        image: "./patient-review-before-illustration.png",
        imageAlt: "Trauma team managing a patient without formal trauma training",
      },
      {
        variant: "intervention",
        tag: "Intervention",
        title: "ATLS® training",
        body: "A 2.5-day ATLS® course at an accredited facility, with one to two units trained per hospital.",
        image: "./training-illustration.png",
        imageAlt: "ATLS course with an instructor teaching residents",
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
    layout: "outcome-list",
    title: "Secondary outcomes",
    outcomes: [
      {
        tag: "Mortality",
        title: "All-cause and in-hospital mortality",
        detail: "24 hours · 30 days · 90 days",
      },
      {
        tag: "Length of stay",
        title: "Emergency department, intensive care, and hospital stay",
        detail: "From patient hospital records",
      },
      {
        tag: "Recovery",
        title: "Return to work",
        detail: "30 and 90 days after arrival",
      },
      {
        tag: "Process",
        title: "Adherence to ATLS® principles",
        detail: "Measured around each transition in a nested staircase design",
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
      { value: "~90%", label: "statistical power" },
      { value: "≥4,320", label: "patients required" },
    ],
  },
  {
    id: "current-status",
    layout: "status-map",
    title: "Current status",
    stats: [
      { value: "10 of 30", label: "hospitals randomised" },
      { value: "~2,000", label: "patients included" },
      { value: "Batch 1", label: "complete (Feb 2025–Mar 2026)" },
      { value: "Batch 3", label: "starting" },
    ],
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
    footer: "Additional funding is needed to complete the trial through 2028.",
  },
  {
    id: "closing",
    layout: "closing",
    title: "Thank you",
    subtitle: "advancetrauma.info",
    footer: "ADVANCE TRAUMA · NCT06321419",
    body: "ATLS® is a registered trademark of the American College of Surgeons. This trial is independent and not endorsed by the College.",
  },
];
