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
  /** Full-width featured figure on a stats slide. */
  focus?: boolean;
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
    | "training-selection"
    | "batch-timeline"
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
  /** Provocation lines, revealed one at a time as the presenter advances. */
  statements?: string[];
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
  trainingSelection?: "concentrated" | "distributed";
  anticipatedStart?: string;
  /** Which exported trial-design JSON to render on design slides. */
  designVariant?: "main" | "staircase";
  /** Show one batch relative to its own start rather than the full trial. */
  designBatch?: number;
}

export const slides: Slide[] = [
  {
    id: "title",
    layout: "title",
    title: "ADVANCE TRAUMA",
    subtitle:
      "Batch 3 site initiation: ATLS® training compared with standard care",
    footer: "NCT06321419 · advancetrauma.info",
  },
  {
    id: "background",
    layout: "bullets",
    title: "Background",
    image: "./crash-illustration.png",
    imageAlt: "Road traffic collision requiring emergency trauma care",
    bullets: [
      "Around 5 million people die from injuries each year<sup>1</sup>",
      "ATLS® is the most widely used trauma life support programme, taught in more than 80 countries<sup>2</sup>",
      "Observational studies suggest better survival, but high-quality evidence on mortality is lacking<sup>3</sup>",
    ],
    references: [
      { id: "1", text: "GBD 2023 Causes of Death Collaborators. Lancet. 2025;406:1811–1872." },
      { id: "2", text: "American College of Surgeons. <a href='https://www.facs.org/quality-programs/trauma/education/advanced-trauma-life-support/'>Advanced Trauma Life Support®</a>." },
      { id: "3", text: "Nakhid Z et al. Systematic review and meta-analysis. Scand J Trauma Resusc Emerg Med. 2026;34:15." },
    ],
  },
  {
    id: "aim",
    layout: "aim",
    title: "Aim",
    body: "To compare the effects of ATLS® training with standard care on outcomes in adult trauma patients.",
  },
  {
    id: "design",
    layout: "design-animation",
    title: "Batched stepped-wedge cluster randomised trial",
  },
  {
    id: "batch-timeline",
    layout: "batch-timeline",
    title: "Batch 3 timeline",
    anticipatedStart: "2026-11-16",
    body: "Anticipated start: 16 November 2026 · 13 months of patient inclusion and 3 additional months of follow up",
    footer: "Training timing is randomised. Cluster numbers are for illustration. Sites are informed at least one month before training.",
  },
  {
    id: "intervention",
    layout: "arms",
    title: "What changes at your hospital",
    arms: [
      {
        variant: "control",
        tag: "Control",
        title: "Standard care",
        body: "Initial resuscitation by casualty medical officers or residents, without formal trauma life support training.",
        image: "./patient-review-before-illustration.png",
        imageAlt: "Clinical team providing initial trauma care",
      },
      {
        variant: "intervention",
        tag: "Intervention",
        title: "ATLS® training",
        body: "A 2.5-day course at an accredited facility in India. About ten physicians per hospital, usually from one or two units.",
        image: "./training-illustration.png",
        imageAlt: "Instructor demonstrating trauma care during an ATLS course",
      },
    ],
  },
  {
    id: "training-selection",
    layout: "training-selection",
    title: "Train physicians within one unit",
    body: "Select physicians providing initial trauma resuscitation.",
    trainingSelection: "concentrated",
  },
  {
    id: "training-distributed",
    layout: "training-selection",
    title: "Spreading trainees across units",
    trainingSelection: "distributed",
  },
  {
    id: "training-selection-emphasis",
    layout: "training-selection",
    title: "Train physicians within one unit",
    body: "We can train an average of 10 physicians per hospital.",
    trainingSelection: "concentrated",
  },
  {
    id: "eligibility",
    layout: "columns",
    title: "Which patients are eligible?",
    columns: [
      {
        heading: "Included",
        bullets: [
          "Aged 15 years or older",
          "Seen in the ED for trauma (ICD-10 chapter XX)",
          "Trauma less than 48 hours before arrival",
          "Admitted, died between arrival and admission, or transferred from the ED for admission",
          "Managed by a participating unit on its working days, before and after training",
        ],
      },
      {
        heading: "Excluded",
        bullets: [
          "Isolated closed extremity fracture",
          "Directly admitted to a ward without being seen by an emergency department physician",
        ],
      },
    ],
  },
  {
    id: "sample-size",
    layout: "columns",
    title: "Sample size and site recruitment",
    columns: [
      {
        heading: "Across the trial",
        bullets: [
          "At least 4,320 patients across 30 hospitals",
          "Around 90% power to detect a reduction in in-hospital mortality within 30 days from 20% to 15%",
        ],
      },
      {
        heading: "At each site",
        bullets: [
          "Aim to include at least 12 eligible patients per month",
          "Include 156 patients over all 13 months",
        ],
      },
    ],
  },
  {
    id: "data-collection",
    layout: "columns",
    title: "What do we measure?",
    body: "Our primary outcome is in-hospital mortality within 30 days of ED arrival",
    columns: [
      {
        heading: "All included patients",
        bullets: [
          "Mortality at 24 hours, 30 and 90 days",
          "ED, ICU and hospital stay; ICU admission",
          "Return to work at 30 and 90 days",
          "Records during admission; phone follow-up after discharge or transfer",
        ],
      },
      {
        heading: "Nested study",
        bullets: [
          "Observation of adherence to ATLS principles",
          "Quality of life (EQ-5D-5L)",
          "Disability (WHODAS 2.0)",
          "Sampled in the three months before and three months after the transition phase",
        ],
      },
    ],
  },
  {
    id: "consent",
    layout: "columns",
    title: "Consent for data collection",
    columns: [
      {
        heading: "Opt out",
        bullets: [
          "Routinely recorded data and observation of ATLS adherence",
          "Give patients or their representatives written study information",
          "They may opt out before final analysis",
        ],
      },
      {
        heading: "Opt in",
        bullets: [
          "Seek consent for non-routinely recorded data",
          "For participants aged 15–18: assent plus guardian or representative consent",
        ],
      },
      {
        heading: "Waiver",
        bullets: [
          "If the patient cannot consent and has no legally authorised representative",
          "Consent by patient or representative when possible"
        ],
      },
    ],
  },
  {
    id: "safety",
    layout: "columns",
    title: "Recognise and report safety events",
    columns: [
      {
        heading: "Events to look for",
        bullets: [
          "Mechanical ventilation for more than 7 days",
          "Initiation of renal replacement therapy",
          "Vasopressors for more than 2 days, or restarted after at least 2 days without",
          "Other events causing prolonged hospitalisation or meaningful harm, or threatening life",
        ],
      },
      {
        heading: "At the site",
        bullets: [
          "Track events during standard care and after training",
          "Clinical research coordinator records events in the case report form",
          "Report to trial management within 24 hours of occurrence",
          "Local investigator evaluates event as expected or trial related",
        ],
      },
    ],
  },

  {
    id: "closing",
    layout: "closing",
    title: "Questions?",
    subtitle: "advancetrauma.info",
  },
];
