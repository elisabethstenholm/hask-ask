import { startItemPolling, startCountdowns } from "./common.js";

const form = document.getElementById("bid-form");
const msgEl = document.getElementById("bid-msg");
const currentUrl = new URL(window.location.href);

if (form) {
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
      } else if (result.status == 409) {
        const errorText = await result.text();
        msgEl.textContent = "Bid rejected: " + errorText;
      } else {
        msgEl.textContent = "Error: something went wrong.";
      }
    } catch (err) {
      console.error("Error: ", err);
      msgEl.textContent = "Error: network error.";
    }
  });
}

startItemPolling();
startCountdowns();