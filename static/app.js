const form = document.getElementById("bid-form");
const msgEl = document.getElementById("msg");

form.addEventListener("submit", async ev => {
    ev.preventDefault();
    msgEl.textContent = "";

    const body = new URLSearchParams(new FormData(form));

    try {
        const result = await fetch("/placeBid", {
            method: "POST",
            headers: {
                "Accept": "*/*",
                "Content-Type": "application/x-www-form-urlencoded"
            },
            body: body.toString()
        });

        if (result.status === 204) {
            msgEl.textContent = "Success: bid registered.";
            form.reset();
        } else {
            msgEl.textContent = "Error: something went wrong.";
        }
    } catch (e) {
        console.error("submit error", e);
        msgEl.textContent = "Error: Network error.";
    }
});