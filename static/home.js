const form = document.getElementById("item-form");
const msgEl = document.getElementById("item-msg");

form.addEventListener("submit", async ev => {
    ev.preventDefault();
    msgEl.textContent = "";

    const obj = Object.fromEntries(new FormData(form).entries());

    try {
        const result = await fetch ("/postItem", {
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