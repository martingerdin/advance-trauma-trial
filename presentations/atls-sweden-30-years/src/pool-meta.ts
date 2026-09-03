import type { MetaAnalysisStudy } from "./figure-data";

export interface PooledEstimate {
  logRr: number;
  seLogRr: number;
  rr: number;
  ciLower: number;
  ciUpper: number;
  rrFormatted: string;
  ciFormatted: string;
  i2: number;
  i2Rounded: number;
  tau2: number;
  numberOfStudies: number;
  q: number;
}

function formatRr(value: number): string {
  return value.toFixed(2);
}

/**
 * Inverse-variance random-effects pool (DerSimonian–Laird τ²).
 * Study-level logRr / seLogRr come from the R metabin export.
 */
export function poolRandomEffects(
  studies: Pick<MetaAnalysisStudy, "logRr" | "seLogRr">[]
): PooledEstimate | null {
  if (studies.length === 0) return null;
  if (studies.length === 1) {
    const s = studies[0];
    const rr = Math.exp(s.logRr);
    const ciLower = Math.exp(s.logRr - 1.96 * s.seLogRr);
    const ciUpper = Math.exp(s.logRr + 1.96 * s.seLogRr);
    return {
      logRr: s.logRr,
      seLogRr: s.seLogRr,
      rr,
      ciLower,
      ciUpper,
      rrFormatted: formatRr(rr),
      ciFormatted: `${formatRr(ciLower)}; ${formatRr(ciUpper)}`,
      i2: 0,
      i2Rounded: 0,
      tau2: 0,
      numberOfStudies: 1,
      q: 0,
    };
  }

  const yi = studies.map((s) => s.logRr);
  const vi = studies.map((s) => s.seLogRr ** 2);
  const wi = vi.map((v) => 1 / v);
  const sumW = wi.reduce((a, b) => a + b, 0);
  const teFixed = wi.reduce((acc, w, i) => acc + w * yi[i], 0) / sumW;
  const q = wi.reduce((acc, w, i) => acc + w * (yi[i] - teFixed) ** 2, 0);
  const k = studies.length;
  const df = k - 1;
  const c = sumW - wi.reduce((acc, w) => acc + w * w, 0) / sumW;
  const tau2 = Math.max(0, (q - df) / c);
  const wr = vi.map((v) => 1 / (v + tau2));
  const sumWr = wr.reduce((a, b) => a + b, 0);
  const logRr = wr.reduce((acc, w, i) => acc + w * yi[i], 0) / sumWr;
  const seLogRr = Math.sqrt(1 / sumWr);
  const rr = Math.exp(logRr);
  const ciLower = Math.exp(logRr - 1.96 * seLogRr);
  const ciUpper = Math.exp(logRr + 1.96 * seLogRr);
  const i2 = q > 0 ? Math.max(0, (q - df) / q) : 0;

  return {
    logRr,
    seLogRr,
    rr,
    ciLower,
    ciUpper,
    rrFormatted: formatRr(rr),
    ciFormatted: `${formatRr(ciLower)}; ${formatRr(ciUpper)}`,
    i2,
    i2Rounded: Math.round(i2 * 100) / 100,
    tau2,
    numberOfStudies: k,
    q,
  };
}

export function sameStudySet(
  included: ReadonlySet<string>,
  allKeys: readonly string[]
): boolean {
  return allKeys.length === included.size && allKeys.every((key) => included.has(key));
}
