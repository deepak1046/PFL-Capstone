const RENDER_URL = "https://noise-backend.onrender.com";
const baseUrl = (window.location.hostname === "localhost" || window.location.hostname === "127.0.0.1") ? "http://localhost:3000" : RENDER_URL;

async function loadData() {

  const response = await fetch(`${baseUrl}/noise/average`);

  const data = await response.json();

  createNoiseChart(data);

}

loadData();