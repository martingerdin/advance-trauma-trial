/**
 * Participating clusters — adapted from advancetrauma.info `src/data/sites.ts`.
 * Keep in sync when sites are confirmed or batch windows change.
 */

export type SiteBatch = "1" | "2" | "3" | "4" | "5" | "6";

export type BatchStatus = "upcoming" | "ongoing" | "completed" | "starting" | "screening";

export type ParticipatingSite = {
  name: string;
  city: string;
  state: string;
  batch: SiteBatch;
  website: string;
  location: { lat: number; lng: number };
  pi: string;
  coordinators?: string;
};

export type SiteBatchInfo = {
  id: SiteBatch;
  title: string;
  /** Inclusive start month as YYYY-MM */
  start?: string;
  /** Inclusive end month as YYYY-MM */
  end?: string;
  /** Explicit status; when set, overrides date-derived status */
  status?: BatchStatus;
  sites: ParticipatingSite[];
};

/** Batch marker/label colors — resolve from design tokens at call time. */
export const batchColorTokens: Record<SiteBatch, string> = {
  "1": "--brand",
  "2": "--accent",
  "3": "--brand-deep",
  "4": "--accent-hover",
  "5": "--brand-darker",
  "6": "--ink",
};

export const participatingSites: ParticipatingSite[] = [
  {
    name: "HBT Medical College And Dr. R N Cooper Municipal General Hospital",
    location: { lat: 19.10790971021016, lng: 72.83623768267398 },
    city: "Mumbai",
    state: "Maharashtra",
    batch: "1",
    website: "https://hbtmc.edu.in/",
    pi: "Geeta Ghag, Vipul Nandu",
    coordinators: "Snehal Jadhav",
  },
  {
    name: "IPGME&R and SSKM Hospital",
    location: { lat: 22.540269944753586, lng: 88.34186296554837 },
    city: "Kolkata",
    state: "West Bengal",
    batch: "1",
    website: "http://www.ipgmer.gov.in/",
    pi: "Shamita Chatterjee, Maitreyee Mukherjee",
    coordinators: "Sayeli Das",
  },
  {
    name: "Christian Medical College & Hospital",
    location: { lat: 30.911165478936997, lng: 75.86348436577856 },
    city: "Ludhiana",
    state: "Punjab",
    batch: "1",
    website: "https://www.cmcludhiana.in/",
    pi: "Parvez Haque, Thejus Varghese",
    coordinators: "Amandeep Kaur",
  },
  {
    name: "Government Medical College & Hospital",
    location: { lat: 30.692216564214863, lng: 76.7551942229613 },
    city: "Chandigarh",
    state: "Chandigarh",
    batch: "1",
    website: "https://www.gmch.gov.in/",
    pi: "Rajeev Sharma",
    coordinators: "Divya Sharma",
  },
  {
    name: "Himalayan Institute of Medical Sciences",
    location: { lat: 30.193327822632433, lng: 78.16497428998137 },
    city: "Dehradun",
    state: "Uttarakhand",
    batch: "1",
    website: "https://srhu.edu.in/medical-sciences/",
    pi: "Hemant Nautiyal",
    coordinators: "Anju Paliyal",
  },
  {
    name: "Seth G.S. Medical College and King Edward Memorial Hospital",
    location: { lat: 19.002633531934915, lng: 72.84142762699638 },
    city: "Mumbai",
    state: "Maharashtra",
    batch: "2",
    website: "https://www.kem.edu/",
    pi: "Monty Khajanchi",
    coordinators: "Snehal Jadhav",
  },
  {
    name: "Lokmanya Tilak Municipal Medical College and General Hospital",
    location: { lat: 19.036157387791466, lng: 72.85942328082004 },
    city: "Mumbai",
    state: "Maharashtra",
    batch: "2",
    website: "https://ltmgh.com/",
    pi: "Vineet Kumar",
    coordinators: "Snehal Jadhav",
  },
  {
    name: "Holy Family Hospital",
    location: { lat: 28.56218828970658, lng: 77.27511129639481 },
    city: "New Delhi",
    state: "Delhi",
    batch: "2",
    website: "https://www.hfhdelhi.org/",
    pi: "Aisvarya Kapoor",
    coordinators: "Himanshu Kumar",
  },
  {
    name: "Assam Medical College & Hospital",
    location: { lat: 27.483625508772267, lng: 94.94401488101919 },
    city: "Dibrugarh",
    state: "Assam",
    batch: "2",
    website: "https://amch-dibrugarh.assam.gov.in/",
    pi: "Jishan Ahmed",
    coordinators: "Noireeta Chetiapatra",
  },
  {
    name: "Dayanand Medical College & Hospital",
    location: { lat: 30.916655731118574, lng: 75.83081233862086 },
    city: "Ludhiana",
    state: "Punjab",
    batch: "2",
    website: "https://www.dmch.edu/",
    pi: "Jaspal Singh",
    coordinators: "Muskaan Katyal",
  },
];

/**
 * Batch windows follow the protocol: each batch runs 13 months with an
 * anticipated 6-month overlap, starting from the trial start (Feb 2025).
 */
export const siteBatches: SiteBatchInfo[] = [
  {
    id: "1",
    title: "Batch 1",
    start: "2025-02",
    end: "2026-03",
    sites: participatingSites.filter((site) => site.batch === "1"),
  },
  {
    id: "2",
    title: "Batch 2",
    start: "2025-12",
    end: "2027-01",
    sites: participatingSites.filter((site) => site.batch === "2"),
  },
  {
    id: "3",
    title: "Batch 3",
    status: "starting",
    sites: [],
  },
  {
    id: "4",
    title: "Batch 4",
    status: "screening",
    sites: [],
  },
  {
    id: "5",
    title: "Batch 5",
    status: "screening",
    sites: [],
  },
  {
    id: "6",
    title: "Batch 6",
    status: "screening",
    sites: [],
  },
];

function parseYearMonth(value: string): { year: number; month: number } {
  const [year, month] = value.split("-").map(Number);
  return { year, month };
}

export function getBatchStatus(
  batch: Pick<SiteBatchInfo, "start" | "end" | "status">,
  now = new Date(),
): BatchStatus {
  if (batch.status) return batch.status;
  if (!batch.start || !batch.end) return "upcoming";

  const start = parseYearMonth(batch.start);
  const end = parseYearMonth(batch.end);
  const startDate = new Date(start.year, start.month - 1, 1);
  const endDate = new Date(end.year, end.month, 0, 23, 59, 59, 999);

  if (now < startDate) return "upcoming";
  if (now > endDate) return "completed";
  return "ongoing";
}

export const batchStatusLabels: Record<BatchStatus, string> = {
  upcoming: "Upcoming",
  ongoing: "Ongoing",
  completed: "Completed",
  starting: "Starting",
  screening: "Screening clusters",
};

export const batchStatusPillClass: Record<BatchStatus, string> = {
  upcoming: "status-pill status-pill--upcoming",
  ongoing: "status-pill status-pill--live",
  completed: "status-pill status-pill--completed",
  starting: "status-pill status-pill--starting",
  screening: "status-pill status-pill--screening",
};

/** Batches that currently have confirmed participating sites (for legend). */
export function batchesWithSites(): SiteBatchInfo[] {
  return siteBatches.filter((batch) => batch.sites.length > 0);
}
