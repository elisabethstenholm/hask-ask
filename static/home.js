import { startItemPolling } from "./common.js";

const form = document.getElementById("item-form");
const msgEl = document.getElementById("item-msg");

form.addEventListener("submit", async ev => {
  ev.preventDefault();
  msgEl.textContent = "";

  const obj = Object.fromEntries(new FormData(form).entries());
  obj.askingPrice = Number.parseInt(obj.askingPrice);

  try {
    const result = await fetch("/postItem", {
      method: "POST",
      headers: {
        "Accept": "*/*",
        "Content-Type": "application/json"
      },
      body: JSON.stringify(obj)
    });

    if (result.status === 204) {
      msgEl.textContent = "Success: item up for sale.";
      form.reset();
    } else {
      msgEl.textContent = "Error: something went wrong.";
    }
  } catch (e) {
    console.error("submit error", e);
    msgEl.textContent = "Error: network error.";
  }
});

function addRow(html) {
  const tpl = document.createElement('template');
  tpl.innerHTML = html.trim();
  const newRow = tpl.content.firstElementChild;
  if (!newRow) return;

  const tbody = document.querySelector('table tbody');
  if (tbody) {
    tbody.appendChild(newRow);
  }
}

async function pollItemList(table) {
  const subscriptionId = table.id;
  if (!subscriptionId) return;

  for (; ;) {
    try {
      const res = await fetch('/itemListSubscription/poll', {
        method: 'POST',
        headers: {
          'Accept': 'text/html',
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ subscriptionId })
      });

      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      const html = await res.text();

      addRow(html);
    } catch (err) {
      console.error("Error: ", err);
      break;
    }
  }
}

function startItemListPolling() {
  const table = document.querySelector('table[id]');
  pollItemList(table);
}

startItemPolling();
startItemListPolling();