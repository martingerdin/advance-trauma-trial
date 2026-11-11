import metaAnalysisJson from "./data/meta-analysis.json";
import trialDesignJson from "./data/trial-design.json";
import trialDesignStaircaseJson from "./data/trial-design-staircase.json";

export interface MetaAnalysisStudy {
  citationKey: string;
  study: string;
  year: number;
  design: string;
  programme: string;
  eligibility: string | null;
  outcome: string | null;
  sampleSize: number;
  atlsN: number;
  atlsDied: number;
  atlsRate: number;
  nonAtlsN: number;
  nonAtlsDied: number;
  nonAtlsRate: number;
  arr: number;
  logRr: number;
  seLogRr: number;
  rr: number;
  ciLower: number;
  ciUpper: number;
  weight: number;
  weightPercent: number;
  favorsAtls: boolean;
  color: string;
}

export interface MetaAnalysisData {
  measure: string;
  method: string;
  methodRandom: string;
  methodTau: string;
  studies: MetaAnalysisStudy[];
  pooled: {
    label: string;
    logRr: number;
    seLogRr: number;
    rr: number;
    ciLower: number;
    ciUpper: number;
    rrFormatted: string;
    ciFormatted: string;
    pValue: number;
    i2: number;
    i2Rounded: number;
    tau2: number;
    numberOfStudies: number;
    color: string;
  };
  labels: {
    left: string;
    right: string;
    effect: string;
    sampleSize: string;
    study: string;
  };
  logScaleXlim: [number, number];
  pooledStudiesCitation: string;
}

export type TrialDesignLayer = "main" | "background" | "overlay";

export interface TrialDesignSegment {
  cluster: number;
  sequence: number;
  batch: number;
  phase: string;
  start: number;
  end: number;
  layer: TrialDesignLayer;
}

export interface TrialDesignData {
  parameters: {
    clusters: number;
    sequences: number;
    batches: number;
    minStandardCareMonths: number;
    minInterventionMonths: number;
    batchesOverlapMonths: number;
    transitionMonths: number;
    transitionOverlapMonths: number;
    startMonth: number;
    totalMonths: number;
    staircaseMonths: number;
    currentMonth: number | null;
    clustersPerBatch: number;
  };
  segments: TrialDesignSegment[];
  colors: Record<string, string>;
  legend: string[];
  labels: {
    x: string;
    y: string;
    fill: string;
    batch: string;
  };
  geometry: {
    xPadding: number;
    barHalfHeight: number;
    overlayHalfHeight: number;
    xBreakStep: number;
    yMin: number;
    yMax: number;
  };
  xMax: number;
}

export const metaAnalysis = metaAnalysisJson as MetaAnalysisData;
export const trialDesign = trialDesignJson as TrialDesignData;
export const trialDesignStaircase = trialDesignStaircaseJson as TrialDesignData;

/** Rebase the selected batch so month zero is its start, not the trial's start. */
export function selectDesignBatch(data: TrialDesignData, batch: number): TrialDesignData {
  const segments = data.segments.filter((segment) => segment.batch === batch);
  if (segments.length === 0) throw new Error(`No trial design data for batch ${batch}`);
  const start = Math.min(...segments.map((segment) => segment.start));
  const firstCluster = Math.min(...segments.map((segment) => segment.cluster));
  return {
    ...data,
    parameters: {
      ...data.parameters,
      clusters: data.parameters.clustersPerBatch,
      batches: 1,
      startMonth: 0,
      currentMonth: null,
    },
    segments: segments.map((segment) => ({
      ...segment,
      cluster: segment.cluster - firstCluster + 1,
      batch: 1,
      start: segment.start - start,
      end: segment.end - start,
    })),
    labels: { ...data.labels, x: "Months since batch start", y: "Hospital" },
    geometry: { ...data.geometry, yMin: 1, yMax: data.parameters.clustersPerBatch },
    xMax: data.parameters.totalMonths,
  };
}
