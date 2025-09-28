import { startItemPolling } from "./common.js";

const form = document.getElementById("bid-form");
const msgEl = document.getElementById("bid-msg");
const currentUrl = new URL(window.location.href);

form.addEventListener("submit", async ev => {
  ev.preventDefault();
  msgEl.textContent = "";

  const obj = Object.fromEntries(new FormData(form).entries());
  obj.amount = Number.parseInt(obj.amount);

  const bidUrl = currentUrl.pathname.replace(/\/?$/, '/placeBid');

  try {
    const result = await fetch(bidUrl, {
      method: "POST",
      headers: {
        "Accept": "*/*",
        "Content-Type": "application/json"
      },
      body: JSON.stringify(obj)
    });

    if (result.status === 204) {
      msgEl.textContent = "Success: bid registered.";
      form.reset();
    } else {
      msgEl.textContent = "Error: something went wrong.";
    }
  } catch (e) {
    console.error("submit error", e);
    msgEl.textContent = "Error: network error.";
  }
});

startItemPolling();