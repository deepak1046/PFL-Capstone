const map = L.map("map").setView([20.5937, 78.9629], 5);

L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution: "OpenStreetMap"
}).addTo(map);

// Coordinates for cities in your dataset
const cityCoordinates = {
    "Ahmedabad": [23.0225, 72.5714],
    "Bangalore": [12.9716, 77.5946],
    "Chennai": [13.0827, 80.2707],
    "Delhi": [28.6139, 77.2090],
    "Hyderabad": [17.3850, 78.4867],
    "Jaipur": [26.9124, 75.7873],
    "Kolkata": [22.5726, 88.3639],
    "Lucknow": [26.8467, 80.9462],
    "Mumbai": [19.0760, 72.8777],
    "Pune": [18.5204, 73.8567]
};

async function loadMapData() {

    try {

        // After deploying to Render, update this URL to match your real service URL.
        const RENDER_URL = "https://noise-backend.onrender.com";
        const baseUrl = (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1')
          ? 'http://localhost:3000'
          : RENDER_URL;
        const response = await fetch(`${baseUrl}/noise/average`);
        const data = await response.json();

        console.log("Map data:", data);

        Object.keys(data).forEach(city => {

            const noise = data[city];
            const coords = cityCoordinates[city];

            if (!coords) return;

            let color =
                noise > 85 ? "red" :
                    noise > 65 ? "orange" :
                        "green";

            const marker = L.circleMarker(coords, {
                radius: 10,
                color: color,
                fillColor: color,
                fillOpacity: 0.8
            }).addTo(map);

            marker.bindPopup(
                `<b>${city}</b><br>Noise Level: ${noise.toFixed(1)} dB`
            );

        });

    } catch (err) {
        console.error("Map backend error:", err);
    }

}

loadMapData();