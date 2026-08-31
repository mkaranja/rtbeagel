// app/js/connectivity.js — imported by app/js/index.js

function pingConnectivity() {
  fetch(window.location.origin + "/__reconnect", { method: "HEAD", cache: "no-store" })
    .then(() => Shiny.setInputValue("rtb_online", true, { priority: "event" }))
    .catch(() => Shiny.setInputValue("rtb_online", false, { priority: "event" }));
}

window.addEventListener("online", pingConnectivity);
window.addEventListener("offline", () =>
  Shiny.setInputValue("rtb_online", false, { priority: "event" })
);

// Catches the websocket dying even when the network interface looks fine.
$(document).on("shiny:disconnected", function () {
  Shiny.setInputValue("rtb_online", false, { priority: "event" });
});

setInterval(pingConnectivity, 15000);