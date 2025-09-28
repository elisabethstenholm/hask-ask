function patchRowFromPayload(tr, data) {
  const byName = (n) => tr.querySelector(`td[name="${n}"]`);

  const descEl = byName('description');
  const endsEl = byName('endTime');
  const highEl = byName('highestBid');
  const stateEl = byName('state');

  if (descEl) descEl.textContent = data.description ?? '';
  if (endsEl) endsEl.textContent = data.endTime ?? '';
  if (highEl) {
    const bid = data.highestBid;
    highEl.textContent =
      bid ? `${bid.name} – ${bid.amount}` : '—';
  }
  if (stateEl) stateEl.textContent = data.state ?? '';
}

async function pollRow(tr) {
  const subscriptionId = tr.id;
  if (!subscriptionId) return;

  for (; ;) {
    try {
      const res = await fetch('/itemSubscription/poll', {
        method: 'POST',
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ subscriptionId })
      });

      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const data = await res.json();

      if (!document.body.contains(tr)) return;

      patchRowFromPayload(tr, data);
    } catch (err) {
      break;
    }
  }
}

export function startItemPolling() {
  const rows = document.querySelectorAll('table tbody tr[id]');
  rows.forEach(tr => { void pollRow(tr); });
}