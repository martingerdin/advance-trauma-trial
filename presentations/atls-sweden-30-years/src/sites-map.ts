/**
 * Leaflet map of participating clusters — adapted from
 * advancetrauma.info `SitesMap` for this vanilla Vite presentation.
 */

import type { Map as LeafletMap, Marker as LeafletMarker } from "leaflet";
import {
  batchColorTokens,
  batchStatusLabels,
  batchStatusPillClass,
  batchesWithSites,
  getBatchStatus,
  participatingSites,
  siteBatches,
  type ParticipatingSite,
} from "./data/sites";

/**
 * Esri World Light Gray — free light basemap suitable for a presentation deck.
 * CARTO Positron (used on advancetrauma.info) now watermarks tiles without an
 * API key; Esri keeps the same clean look without a key for this use case.
 * Note Esri’s {z}/{y}/{x} path order (not {z}/{x}/{y}).
 */
const TILE_URL =
  "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}";
const TILE_ATTRIBUTION =
  'Tiles &copy; <a href="https://www.esri.com/">Esri</a> &mdash; Esri, DeLorme, NAVTEQ';

function cssVar(name: string): string {
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
}

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, "&amp;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;");
}

function safeHttpUrl(value: string): string | null {
  try {
    const url = new URL(value);
    if (url.protocol !== "http:" && url.protocol !== "https:") return null;
    return url.href;
  } catch {
    return null;
  }
}

function buildSitePopupHtml(site: ParticipatingSite): string {
  const batch = siteBatches.find((item) => item.id === site.batch)!;
  const status = getBatchStatus(batch);
  const websiteHref = safeHttpUrl(site.website);
  const websiteLink = websiteHref
    ? `<a class="sites-map-popup__link" href="${escapeHtml(websiteHref)}" target="_blank" rel="noopener noreferrer">Visit website</a>`
    : "";
  const coordinators = site.coordinators
    ? `<p class="sites-map-popup__row">
        <span class="sites-map-popup__label">Clinical research coordinator</span>
        ${escapeHtml(site.coordinators)}
      </p>`
    : "";

  return `
    <div class="sites-map-popup__body">
      <h3 class="sites-map-popup__title">${escapeHtml(site.name)}</h3>
      <div class="sites-map-popup__pills">
        <span class="sites-map-popup__pill sites-map-popup__pill--batch" style="background: var(${batchColorTokens[site.batch]});">Batch ${escapeHtml(site.batch)}</span>
        <span class="${batchStatusPillClass[status]}">${escapeHtml(batchStatusLabels[status])}</span>
      </div>
      <p class="sites-map-popup__row">
        <span class="sites-map-popup__label">Investigator</span>
        ${escapeHtml(site.pi)}
      </p>
      ${coordinators}
      <p class="sites-map-popup__row">
        <span class="sites-map-popup__label">Location</span>
        ${escapeHtml(site.city)}, ${escapeHtml(site.state)}
      </p>
      ${websiteLink}
    </div>
  `;
}

function createSitePopupElement(bodyHtml: string, onClose: () => void): HTMLElement {
  const popup = document.createElement("div");
  popup.className = "sites-map__popup";
  popup.setAttribute("role", "dialog");
  popup.innerHTML = `
    <button type="button" class="sites-map__popup-close" aria-label="Close">×</button>
    ${bodyHtml}
  `;

  const closeBtn = popup.querySelector(".sites-map__popup-close");
  closeBtn?.addEventListener("click", (event) => {
    event.stopPropagation();
    onClose();
  });
  popup.addEventListener("click", (event) => {
    event.stopPropagation();
  });

  return popup;
}

export function buildSitesLegendHtml(): string {
  return batchesWithSites()
    .map((batch) => {
      const status = getBatchStatus(batch);
      return `
        <span class="sites-legend__item">
          <span class="sites-legend__swatch" style="background: var(${batchColorTokens[batch.id]});" aria-hidden="true"></span>
          ${escapeHtml(batch.title)}
          <span class="${batchStatusPillClass[status]}">${escapeHtml(batchStatusLabels[status])}</span>
          <span class="sites-legend__count">${batch.sites.length}</span>
        </span>`;
    })
    .join("");
}

export type SitesMapController = {
  refresh: () => void;
  destroy: () => void;
};

/**
 * Mount a Leaflet map into `container`. Safe to call while the slide is hidden;
 * call `refresh()` when the slide becomes active so Leaflet remeasures size.
 */
export async function mountSitesMap(container: HTMLElement): Promise<SitesMapController> {
  const [{ default: L }] = await Promise.all([
    import("leaflet"),
    import("leaflet/dist/leaflet.css"),
  ]);

  const textInverse = cssVar("--text-inverse") || "#ffffff";
  const markerSize = Number.parseFloat(cssVar("--map-marker-size-px")) || 28;
  const popupWidth = Number.parseFloat(cssVar("--map-popup-width-px")) || 280;
  const mapPad = Number.parseFloat(cssVar("--map-fit-padding-px")) || 24;

  const map: LeafletMap = L.map(container, {
    scrollWheelZoom: false,
    attributionControl: true,
  });

  L.tileLayer(TILE_URL, {
    attribution: TILE_ATTRIBUTION,
    maxZoom: 16,
  }).addTo(map);

  const bounds = L.latLngBounds([]);
  const markers: LeafletMarker[] = [];

  for (const site of participatingSites) {
    const markerColor = cssVar(batchColorTokens[site.batch]) || "#1a9dbb";
    const icon = L.divIcon({
      className: "sites-map__leaflet-marker",
      html: `
        <svg width="${markerSize}" height="${markerSize}" viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
          <circle cx="16" cy="16" r="12" fill="${markerColor}" stroke="${textInverse}" stroke-width="2"/>
          <circle cx="16" cy="16" r="6" fill="${textInverse}"/>
        </svg>
      `,
      iconSize: [markerSize, markerSize],
      iconAnchor: [markerSize / 2, markerSize / 2],
      popupAnchor: [0, -(markerSize / 2 - 2)],
    });

    const popupHtml = buildSitePopupHtml(site);
    const marker = L.marker([site.location.lat, site.location.lng], {
      title: site.name,
      icon,
    })
      .bindPopup(
        () => createSitePopupElement(popupHtml, () => marker.closePopup()),
        {
          maxWidth: popupWidth,
          className: "sites-map__leaflet-popup",
          closeButton: false,
        },
      )
      .addTo(map);

    bounds.extend([site.location.lat, site.location.lng]);
    markers.push(marker);
  }

  if (bounds.isValid()) {
    map.fitBounds(bounds, { padding: [mapPad, mapPad] });
  } else {
    map.setView([20.5937, 78.9629], 5);
  }

  const refresh = () => {
    requestAnimationFrame(() => {
      map.invalidateSize();
      if (bounds.isValid()) {
        map.fitBounds(bounds, { padding: [mapPad, mapPad] });
      }
    });
  };

  refresh();

  return {
    refresh,
    destroy: () => {
      map.remove();
      markers.length = 0;
    },
  };
}
