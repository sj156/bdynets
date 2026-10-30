'use strict';
(async function () {
  const packed = atob(document.getElementById('records').textContent.replace(/\s/g, ''));
  const bytes = Uint8Array.from(packed, c => c.charCodeAt(0));
  const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream('deflate'));
  const data = JSON.parse(await new Response(stream).text());
  const state = { index: data.initial, region: '', category: '', timer: null };
  const map = L.map('map', { preferCanvas: true, renderer: L.canvas({ tolerance: 4 }), zoomSnap: .1, zoomDelta: .5 });
  const roadLayer = L.layerGroup().addTo(map);
  const eventLayer = L.layerGroup().addTo(map);
  const contextLayer = L.layerGroup().addTo(map);
  const labels = L.layerGroup().addTo(contextLayer);
  const roadShapes = [], roadHalos = [], eventShapes = new Map(), trendCache = new Map();
  const byId = id => document.getElementById(id);
  const escape = v => String(v ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const format = v => v == null || !Number.isFinite(v) ? 'NA' : v !== 0 && Math.abs(v) < .001 ? v.toPrecision(4) : v.toLocaleString('en-US', { maximumFractionDigits: 4 });
  const ll = p => L.latLng(p[1], p[0]);
  const quantile = (a, p) => { const v = [...a].sort((x, y) => x - y), i = (v.length - 1) * p, j = Math.floor(i); return v[j] + (v[Math.min(j + 1, v.length - 1)] - v[j]) * (i - j); };
  const allRoads = data.roads.map((_, j) => j), regionRoads = new Map();
  for (const j of allRoads) {
    const key = data.roads[j].region;
    if (!regionRoads.has(key)) regionRoads.set(key, []);
    regionRoads.get(key).push(j);
  }
  const sourceCoordinates = data.roads.map(r => r.geometry.map(ll));
  const projectedRoads = sourceCoordinates.map(g => g.map(p => map.project(p, 0)));
  const roadBounds = projectedRoads.map(g => {
    const b = [Infinity, Infinity, -Infinity, -Infinity];
    for (const p of g) { b[0] = Math.min(b[0], p.x); b[1] = Math.min(b[1], p.y); b[2] = Math.max(b[2], p.x); b[3] = Math.max(b[3], p.y); }
    return b;
  });
  function makeRoadIndex(indices) {
    if (!indices.length) return null;
    const bounds = [Infinity, Infinity, -Infinity, -Infinity];
    for (const j of indices) { const b = roadBounds[j]; bounds[0] = Math.min(bounds[0], b[0]); bounds[1] = Math.min(bounds[1], b[1]); bounds[2] = Math.max(bounds[2], b[2]); bounds[3] = Math.max(bounds[3], b[3]); }
    if (indices.length <= 24) return { bounds, indices };
    const axis = bounds[2] - bounds[0] > bounds[3] - bounds[1] ? 0 : 1;
    indices.sort((a, b) => roadBounds[a][axis] + roadBounds[a][axis + 2] - roadBounds[b][axis] - roadBounds[b][axis + 2]);
    const mid = Math.floor(indices.length / 2);
    return { bounds, left: makeRoadIndex(indices.slice(0, mid)), right: makeRoadIndex(indices.slice(mid)) };
  }
  const roadIndex = makeRoadIndex([...allRoads]);
  const intersects = (a, b) => a[0] <= b[2] && a[2] >= b[0] && a[1] <= b[3] && a[3] >= b[1];
  function viewportRoads() {
    const scale = Math.pow(2, map.getZoom()), origin = map.getPixelBounds().min, size = map.getSize(), pad = data.layout.tile;
    const bounds = [(origin.x - pad) / scale, (origin.y - pad) / scale, (origin.x + size.x + pad) / scale, (origin.y + size.y + pad) / scale];
    const found = [];
    function query(node) {
      if (!node || !intersects(node.bounds, bounds)) return;
      if (node.indices) { for (const j of node.indices) if ((!state.region || data.roads[j].region === state.region) && intersects(roadBounds[j], bounds)) found.push(j); }
      else { query(node.left); query(node.right); }
    }
    query(roadIndex);
    return found.sort((a, b) => a - b);
  }
  const eventsByFrame = data.times.map(() => []);
  for (const e of data.events) eventsByFrame[e.frame - 1].push(e);
  const roadIndices = () => state.region ? regionRoads.get(state.region) || [] : allRoads;
  const regionEvent = e => !state.region || e.region === state.region;
  const categoryEvent = e => !state.category || e.category === state.category;
  const selectedEvents = (i = state.index) => eventsByFrame[i].filter(e => regionEvent(e) && categoryEvent(e));

  function color(value) {
    if (value == null) return '#94a3b8';
    let group = 0;
    while (group < data.scale.breaks.length && Math.log1p(value) >= Math.log1p(data.scale.breaks[group])) group++;
    return data.scale.palette[group];
  }
  function locations(complete) {
    const indices = roadIndices();
    const roads = complete ? indices.flatMap(j => data.roads[j].geometry.map(ll)) : indices.map(j => {
      const g = data.roads[j].geometry;
      return L.latLng(quantile(g.map(p => p[1]), .5), quantile(g.map(p => p[0]), .5));
    });
    const events = data.events.filter(e => regionEvent(e) && categoryEvent(e)).map(e => L.latLng(e.lat, e.lon));
    return [...roads, ...events];
  }
  function reset(complete = false) {
    const points = locations(complete);
    if (!points.length) return;
    if (complete) {
      map.fitBounds(L.latLngBounds(points), { padding: [22, 22], maxZoom: 14, animate: false });
      return;
    }
    const projected = points.map(p => map.options.crs.project(p)), low = points.length >= 5 ? .15 : 0, high = points.length >= 5 ? .85 : 1;
    let left = quantile(projected.map(p => p.x), low), right = quantile(projected.map(p => p.x), high);
    let bottom = quantile(projected.map(p => p.y), low), top = quantile(projected.map(p => p.y), high);
    const pad = Math.max(right - left, top - bottom, 6378137 * .00005) * .08;
    const bounds = L.latLngBounds(map.options.crs.unproject(L.point(left - pad, bottom - pad)), map.options.crs.unproject(L.point(right + pad, top + pad)));
    map.fitBounds(bounds, { padding: [20, 20], maxZoom: 14, animate: false });
  }
  function updateLabels() {
    labels.clearLayers();
    const places = (data.basemap?.features || []).filter(f => f.geometry.type === 'Point' && f.properties.label);
    const placed = [];
    for (const f of places) {
      const point = ll(f.geometry.coordinates);
      if (!map.getBounds().contains(point)) continue;
      const screen = map.latLngToContainerPoint(point), name = String(f.properties.label), width = Math.max(60, Math.min(170, name.length * 6));
      if (placed.some(p => Math.abs(p.x - screen.x) < (p.width + width) / 2 && Math.abs(p.y - screen.y) < 23)) continue;
      L.marker(point, { interactive: false, pane: 'context', icon: L.divIcon({ className: 'place-label', html: escape(name), iconSize: [width, 18], iconAnchor: [width / 2, 9] }) }).addTo(labels);
      placed.push({ ...screen, width });
      if (placed.length >= 24) break;
    }
  }
  function basemap() {
    map.createPane('context'); map.getPane('context').style.zIndex = 250; map.getPane('context').style.pointerEvents = 'none';
    if (!data.basemap) return;
    L.geoJSON(data.basemap, {
      filter: f => f.geometry.type !== 'Point', interactive: false, pane: 'context', renderer: L.canvas({ pane: 'context' }),
      style: f => f.geometry.type === 'Polygon' ? { stroke: false, fillColor: '#e1e5e2', fillOpacity: .95, fillRule: 'evenodd' } :
        { color: '#ccd1d3', weight: .65, opacity: .8 }
    }).addTo(contextLayer);
    map.attributionControl.addAttribution(escape(data.basemap.attribution));
    map.on('moveend zoomend', updateLabels); updateLabels();
  }
  let roadLayout;
  function calculateLayout(indices) {
    const rules = data.layout, tile = rules.tile, size = map.getSize(), origin = map.getPixelBounds().min;
    const base = Math.max(rules.min_width, Math.min(rules.max_width, rules.base_width + rules.zoom_slope * (map.getZoom() - rules.reference_zoom)));
    const samples = [], bins = new Map();
    for (const j of indices) {
      const scale = Math.pow(2, map.getZoom());
      const points = projectedRoads[j].map(p => L.point(p.x * scale - origin.x, p.y * scale - origin.y));
      for (let k = 0; k < points.length - 1; k++) {
        const a = points[k], dx = points[k + 1].x - a.x, dy = points[k + 1].y - a.y;
        let low = 0, high = 1, valid = true;
        for (const [p, q] of [[-dx, a.x + tile], [dx, size.x + tile - a.x], [-dy, a.y + tile], [dy, size.y + tile - a.y]]) {
          if (p === 0 && q < 0) { valid = false; break; }
          if (p < 0) low = Math.max(low, q / p);
          if (p > 0) high = Math.min(high, q / p);
        }
        if (!valid || high <= low) continue;
        const length = Math.hypot(dx, dy) * (high - low);
        if (length < 1e-10) continue;
        const count = Math.ceil(length / rules.sample), weight = length / count;
        for (let h = 0; h < count; h++) {
          const t = low + (h + .5) / count * (high - low);
          const x = Math.floor((a.x + t * dx) / tile), y = Math.floor((a.y + t * dy) / tile), key = x + ':' + y;
          bins.set(key, (bins.get(key) || 0) + weight); samples.push({ road: j, x, y, weight });
        }
      }
    }
    const weighted = new Float64Array(data.roads.length), lengths = new Float64Array(data.roads.length);
    for (const s of samples) {
      let local = 0;
      for (let dx = -1; dx <= 1; dx++) for (let dy = -1; dy <= 1; dy++) local += (bins.get((s.x + dx) + ':' + (s.y + dy)) || 0) / (9 * tile * tile);
      weighted[s.road] += local * s.weight; lengths[s.road] += s.weight;
    }
    roadLayout = [];
    for (const j of indices) {
      const density = lengths[j] ? weighted[j] / lengths[j] : 0;
      const width = Math.max(rules.min_width, base / Math.pow(1 + Math.pow(base * density / rules.budget, 4), .25));
      roadLayout[j] = { width, density,
        gap: Math.max(rules.gap_min, Math.min(rules.gap_max, rules.gap_base + rules.gap_slope * width)),
        halo: Math.max(rules.halo_min, Math.min(rules.halo_max, rules.halo_slope * width)) };
    }
  }
  let activeRoads = [], lastViewKey = '', openRoad = null;
  const coordinateCache = [], coordinateKeys = [];
  const haloLayer = L.layerGroup().addTo(map);
  map.createPane('roadHalo'); map.getPane('roadHalo').style.zIndex = 390; map.getPane('roadHalo').style.pointerEvents = 'none';
  const haloRenderer = L.canvas({ pane: 'roadHalo' });
  function refreshRoads() {
    const origin = map.getPixelBounds().min, size = map.getSize();
    const key = JSON.stringify([state.region, map.getZoom(), origin.x, origin.y, size.x, size.y]);
    if (key === lastViewKey) return;
    const indices = viewportRoads();
    if (openRoad != null && (!state.region || data.roads[openRoad].region === state.region) && !indices.includes(openRoad)) { indices.push(openRoad); indices.sort((a, b) => a - b); }
    calculateLayout(indices);
    const included = new Set(indices);
    for (const j of activeRoads) if (!included.has(j)) { roadLayer.removeLayer(roadShapes[j]); haloLayer.removeLayer(roadHalos[j]); }
    for (const j of indices) {
      const coords = roadCoordinates(j), style = roadStyle(j);
      if (!roadShapes[j]) {
        roadHalos[j] = L.polyline(coords, { renderer: haloRenderer, pane: 'roadHalo', color: '#ffffff', weight: style.weight + 2 * roadLayout[j].halo, opacity: .85, interactive: false });
        roadShapes[j] = L.polyline(coords, style).bindPopup(() => roadPopup(j), { maxWidth: 390 });
        roadShapes[j].on('popupopen', () => { openRoad = j; });
        roadShapes[j].on('popupclose', () => { if (openRoad === j) openRoad = null; });
      } else {
        if (roadShapes[j].getLatLngs() !== coords && roadShapes[j]._trafficCoordinates !== coords) {
          roadShapes[j].setLatLngs(coords); roadHalos[j].setLatLngs(coords);
        }
        if (roadShapes[j].options.weight !== style.weight || roadShapes[j].options.color !== style.color) roadShapes[j].setStyle(style);
        const haloWidth = style.weight + 2 * roadLayout[j].halo;
        if (roadHalos[j].options.weight !== haloWidth) roadHalos[j].setStyle({ weight: haloWidth });
      }
      roadShapes[j]._trafficCoordinates = coords;
      if (!haloLayer.hasLayer(roadHalos[j])) roadHalos[j].addTo(haloLayer);
      if (!roadLayer.hasLayer(roadShapes[j])) roadShapes[j].addTo(roadLayer);
      roadShapes[j].bringToFront();
    }
    activeRoads = indices; lastViewKey = key;
  }
  const visibility = { roads: true, events: true, context: true, casing: true };
  const capabilities = { roads: data.roads.length > 0, events: data.events.length > 0,
    context: !!data.basemap?.features.length, casing: data.roads.length > 0 };
  function applyLayerVisibility() {
    for (const [name, layer] of Object.entries({ roads: roadLayer, events: eventLayer, context: contextLayer, casing: haloLayer })) {
      const shown = capabilities[name] && visibility[name] && (name !== 'casing' || visibility.roads);
      if (shown && !map.hasLayer(layer)) layer.addTo(map);
      if (!shown && map.hasLayer(layer)) map.removeLayer(layer);
    }
  }
  function setupLayerControls() {
    for (const name of Object.keys(visibility)) {
      const control = byId('layer' + name[0].toUpperCase() + name.slice(1));
      control.disabled = !capabilities[name]; control.checked = capabilities[name]; visibility[name] = capabilities[name];
      control.addEventListener('change', () => { visibility[name] = control.checked; applyLayerVisibility(); });
    }
    applyLayerVisibility();
  }
  function roadStyle(j) {
    const width = roadLayout[j].width;
    return { color: color(data.values[state.index][j]), weight: width, opacity: .94, lineCap: 'round', lineJoin: 'round' };
  }
  function roadCoordinates(j) {
    const r = data.roads[j], coords = sourceCoordinates[j], pairs = r.opposing;
    if (!pairs?.some(p => p[0] != null)) return coords;
    const width = roadStyle(j).weight, zoom = map.getZoom(), scale = Math.pow(2, zoom);
    const key = zoom + ':' + width + ':' + roadLayout[j].gap;
    if (coordinateKeys[j] === key) return coordinateCache[j];
    const points = projectedRoads[j].map(p => L.point(p.x * scale, p.y * scale));
    const units = map.options.crs.scale(map.getZoom()) / (2 * Math.PI * 6378137);
    const shifts = pairs.map((pair, k) => {
      const dx = points[k + 1].x - points[k].x, dy = points[k + 1].y - points[k].y, length = Math.hypot(dx, dy);
      const amount = pair[0] == null || length < 1e-10 ? 0 : Math.max(0, (width + roadLayout[j].gap - pair[0] * units) / 2) * pair[1];
      return length ? L.point(-dy / length * amount, dx / length * amount) : L.point(0, 0);
    });
    const shifted = points.map((point, k) => {
      const shift = k === 0 ? shifts[0] : k === points.length - 1 ? shifts[shifts.length - 1] : shifts[k - 1].add(shifts[k]).divideBy(2);
      return map.unproject(point.add(shift));
    });
    coordinateKeys[j] = key; coordinateCache[j] = shifted; return shifted;
  }
  function roadPopup(j) {
    const r = data.roads[j], rows = [['Time', data.labels[state.index]], ['Flow', format(data.values[state.index][j]) + ' ' + data.unit], ...Object.entries(r.attributes)];
    return '<div class="popup"><b>' + escape(r.id) + '</b><table>' + rows.filter(([, v]) => v != null && typeof v !== 'object').map(([k, v]) => '<tr><td>' + escape(k) + '</td><td>' + escape(v) + '</td></tr>').join('') + '</table></div>';
  }
  function buildRoads() { lastViewKey = ''; refreshRoads(); }
  function eventPopup(e) {
    return '<div class="popup"><b>' + escape(e.category) + '</b><table><tr><td>ID</td><td>' + escape(e.id) + '</td></tr><tr><td>Reported time</td><td>' + escape(e.time_label || format(e.time)) + '</td></tr><tr><td>Interval</td><td>' + escape(data.labels[e.frame - 1]) + '</td></tr><tr><td>Position</td><td>' + escape(e.lon) + ', ' + escape(e.lat) + '</td></tr></table><p>' + escape(e.description) + '</p></div>';
  }
  function drawEvents() {
    eventLayer.clearLayers(); eventShapes.clear();
    for (const e of selectedEvents()) {
      const icon = L.divIcon({ className: 'event-container', html: '<div class="event-symbol"><span>!</span></div>', iconSize: [18, 18], iconAnchor: [9, 9] });
      const marker = L.marker([e.lat, e.lon], { icon }).bindPopup(() => eventPopup(e), { maxWidth: 380 }).addTo(eventLayer);
      eventShapes.set(e.id, marker);
    }
    const current = selectedEvents();
    byId('eventList').innerHTML = current.length ? '<h3>Events in interval · ' + current.length + '</h3>' + current.slice(0, 20).map(e => '<button class="event-row" data-id="' + escape(e.id) + '"><b>' + escape(e.category) + '</b>' + escape(e.description) + '</button>').join('') : '';
    for (const button of byId('eventList').querySelectorAll('button')) button.addEventListener('click', () => {
      visibility.events = true; byId('layerEvents').checked = true; applyLayerVisibility();
      const marker = eventShapes.get(button.dataset.id); map.setView(marker.getLatLng(), Math.max(map.getZoom(), 12), { animate: false }); marker.openPopup();
    });
  }
  function trend() {
    if (!trendCache.has(state.region)) {
      const indices = roadIndices();
      trendCache.set(state.region, data.values.map(row => { const v = indices.map(j => row[j]).filter(v => v != null); return v.length ? v.reduce((a, b) => a + b, 0) / v.length : null; }));
    }
    return trendCache.get(state.region);
  }
  function chart(id, values) {
    const canvas = byId(id), size = canvas.getBoundingClientRect(), ratio = devicePixelRatio || 1;
    canvas.width = Math.max(1, size.width * ratio); canvas.height = Math.max(1, size.height * ratio);
    const ctx = canvas.getContext('2d'); ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    const valid = values.filter(v => v != null), top = Math.max(1, ...valid), w = size.width - 18, h = size.height - 16;
    ctx.clearRect(0, 0, size.width, size.height); ctx.strokeStyle = '#365c70'; ctx.lineWidth = 1.8; ctx.beginPath(); let started = false;
    values.forEach((v, j) => { if (v == null) { started = false; return; } const px = 8 + j * w / Math.max(1, values.length - 1), py = 6 + h * (1 - v / top); if (started) ctx.lineTo(px, py); else ctx.moveTo(px, py); started = true; }); ctx.stroke();
    const cursor = 8 + state.index * w / Math.max(1, values.length - 1); ctx.strokeStyle = '#303a40'; ctx.beginPath(); ctx.moveTo(cursor, 3); ctx.lineTo(cursor, h + 10); ctx.stroke();
  }
  function legend() {
    let html = '';
    if (data.roads.length) {
      html = '<b>Flow · ' + escape(data.unit) + '</b><div>Log scale · rounded limits</div>';
      if (data.scale.kind === 'missing') html += '<p>No finite flow observations</p>';
      else if (data.scale.kind === 'zero') html += '<div class="swatch"><i style="background:' + data.scale.palette[0] + '"></i>0</div>';
      else html += data.scale.palette.map((c, j) => '<div class="swatch"><i style="background:' + c + '"></i>' + escape(data.scale.labels[j]) + '</div>').join('');
      html += '<div class="swatch"><i style="background:#94a3b8"></i>Missing</div>';
    }
    if (data.events.length) html += '<div class="event-key"><div class="event-symbol"><span>!</span></div><span>Reported event</span></div>';
    byId('legend').innerHTML = html;
  }
  function update() {
    const indices = roadIndices(), row = data.values[state.index], finite = indices.map(j => row[j]).filter(v => v != null), events = selectedEvents();
    byId('time').value = state.index; byId('timeLabel').textContent = data.labels[state.index]; byId('frameLabel').textContent = 'Frame ' + (state.index + 1) + ' / ' + data.times.length + ' · ' + format(data.intervals[state.index] / 60) + ' min';
    for (const j of activeRoads) { roadShapes[j].setStyle(roadStyle(j)); if (roadShapes[j].isPopupOpen()) roadShapes[j].setPopupContent(roadPopup(j)); }
    const cards = data.roads.length ? [['Available roads', finite.length], ['Missing roads', indices.length - finite.length], ['Mean · ' + data.unit, finite.length ? finite.reduce((a, b) => a + b, 0) / finite.length : null], ['Maximum', finite.length ? finite.reduce((maximum, value) => Math.max(maximum, value), -Infinity) : null], ...(data.events.length ? [['Events in interval', events.length]] : [])] : [['Events in interval', events.length], ['Events in data', data.events.filter(regionEvent).length], ['Categories in interval', new Set(events.map(e => e.category)).size], ['Interval · min', data.intervals[state.index] / 60]];
    byId('cards').innerHTML = cards.map(([label, value]) => '<div class="card"><b>' + escape(format(value)) + '</b><span>' + escape(label) + '</span></div>').join('');
    if (data.roads.length) chart('flowTrend', trend());
    if (data.events.length) chart('eventTrend', data.times.map((_, i) => selectedEvents(i).length));
    drawEvents();
  }
  function stop() { if (state.timer) clearInterval(state.timer); state.timer = null; byId('play').textContent = 'Play'; }
  byId('title').textContent = data.title;
  byId('clock').textContent = data.clock === 'absolute' ? 'Absolute timestamps · UTC display' : 'Source clock · UTC offset unspecified';
  byId('regionField').style.display = data.regions.length ? '' : 'none';
  byId('region').innerHTML = data.regions.map(r => '<option value="' + escape(r) + '">' + escape(r) + '</option>').join('') + '<option value="">All regions</option>'; byId('region').value = state.region;
  byId('categoryField').style.display = data.events.length ? '' : 'none';
  byId('category').innerHTML = '<option value="">All</option>' + [...new Set(data.events.map(e => e.category))].sort().map(c => '<option value="' + escape(c) + '">' + escape(c) + '</option>').join('');
  byId('flowTrendBox').style.display = data.roads.length ? '' : 'none'; byId('eventTrendBox').style.display = data.events.length ? '' : 'none'; byId('time').max = data.times.length - 1;
  reset(); basemap(); buildRoads(); legend(); update(); setupLayerControls();
  byId('region').addEventListener('change', () => { stop(); state.region = byId('region').value; reset(); buildRoads(); update(); });
  byId('category').addEventListener('change', () => { stop(); state.category = byId('category').value; update(); });
  byId('time').addEventListener('input', () => { stop(); state.index = Number(byId('time').value); update(); });
  byId('play').addEventListener('click', () => { if (state.timer) return stop(); byId('play').textContent = 'Pause'; state.timer = setInterval(() => { state.index = (state.index + 1) % data.times.length; update(); }, 700); });
  byId('reset').addEventListener('click', () => reset()); byId('extent').addEventListener('click', () => reset(true));
  map.on('moveend resize', () => refreshRoads());
  window.addEventListener('resize', update);
  window.trafficMap = { layers: { roads: roadLayer, events: eventLayer, context: contextLayer, casing: haloLayer }, layerState: () => ({ ...visibility }), data, state, map, color, roadShapes, eventShapes, roadCoordinates, layout: () => roadLayout, refreshRoads, renderedRoadIndices: () => [...activeRoads], roadIndices, selectedEvents, trend, reset, update };
})().catch(error => { document.getElementById('mapError').textContent = error.message; console.error(error); });
