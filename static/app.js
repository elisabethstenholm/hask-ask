const bidForm = document.getElementById("bid-form");
const msgEl = document.getElementById("msg");
const currentUrl = new URL(window.location.href);

bidForm.addEventListener("submit", async ev => {
    ev.preventDefault();
    msgEl.textContent = "";

    const obj = Object.fromEntries(new FormData(bidForm).entries());
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
            bidForm.reset();
        } else {
            msgEl.textContent = "Error: something went wrong.";
        }
    } catch (e) {
        console.error("submit error", e);
        msgEl.textContent = "Error: Network error.";
    }
});