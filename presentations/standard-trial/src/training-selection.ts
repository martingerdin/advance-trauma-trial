const units = [
  { name: "Unit 1", days: "Mon & Thu" },
  { name: "Unit 2", days: "Tue & Fri" },
  { name: "Unit 3", days: "Wed & Sat" },
];

// Selections from onboarding slides 11–13, indexed by resident then unit.
const selections = {
  concentrated: [[true, false, false], [true, false, false], [true, false, false], [true, false, false]],
  distributed: [[true, false, true], [false, true, false], [false, false, false], [false, false, false]],
};

export type TrainingSelection = keyof typeof selections;

export function renderTrainingSelection(option: TrainingSelection): string {
  const preferred = option === "concentrated";
  return `<div class="training-diagram training-diagram--${option}">
    <div class="training-comparison-heading">
      <svg class="training-choice" viewBox="0 0 64 64" role="img" aria-label="${preferred ? "Recommended" : "Not recommended"}"><path d="${preferred ? "M10 33 L25 48 L55 16" : "M14 14 L50 50 M50 14 L14 50"}" fill="none" stroke="currentColor" stroke-width="7"/></svg>
      <p>${preferred ? "Four trained residents working together" : "One trained resident per unit"}</p>
    </div>
    <div class="training-units">
      ${units.map((unit, column) => `<section class="training-unit">
        <h3>${unit.name}</h3>
        <p class="training-unit__days">${unit.days}</p>
        <div class="training-residents">${selections[option].map((row, resident) => `<svg class="training-resident ${row[column] ? "is-selected" : ""}" viewBox="0 0 64 80" role="img" aria-label="Resident ${resident + 1}: ${row[column] ? "selected for ATLS training" : "not selected"}">
          <circle cx="32" cy="14" r="10"/>
          <path d="M15 70 V42 Q15 29 32 29 Q49 29 49 42 V70 Z"/>
          <text x="32" y="55" text-anchor="middle" dominant-baseline="middle">${resident + 1}</text>
        </svg>`).join("")}</div>
      </section>`).join("")}
    </div>
    <p class="training-key">Coloured residents are selected for ATLS® training.</p>
  </div>`;
}
