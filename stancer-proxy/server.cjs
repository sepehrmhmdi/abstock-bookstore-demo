const express = require("express");
const { createProxyMiddleware } = require("http-proxy-middleware");
const dotenv = require("dotenv");

dotenv.config();

const app = express();

const PORT = process.env.PORT || 3031;
const HOST = process.env.HOST || "127.0.0.1";
const NODE_ENV = process.env.NODE_ENV || "development";

const API_BASE = process.env.STANCER_API_BASE || "https://api.stancer.com/v1";
const RETURN_URL = process.env.STANCER_RETURN_URL || "";
const SECRET = process.env.STANCER_SECRET || "";
const TIMEOUT = Number(process.env.STANCER_TIMEOUT_MS || 10000);

app.get("/whoami", (_req, res) => {
  res.json({
    ok: true,
    nodeEnv: NODE_ENV,
    host: HOST,
    port: PORT,
    upstream: API_BASE,
    hasSecret: Boolean(SECRET && SECRET.length > 5),
    returnUrl: RETURN_URL || "(none)",
    timeoutMs: TIMEOUT
  });
});

app.use(
  "/v1",
  createProxyMiddleware({
    target: API_BASE,
    changeOrigin: true,
    pathRewrite: { "^/v1": "" },
    timeout: TIMEOUT,
    proxyTimeout: TIMEOUT,
    onProxyReq: (proxyReq, req) => {
      proxyReq.setHeader("Authorization", `Bearer ${SECRET}`);
      const override = req.headers["x-return-url"];
      if (override) proxyReq.setHeader("x-return-url", override);
    },
    onError: (err, _req, res) => {
      console.error("Proxy error:", err.message);
      res.status(502).json({ ok: false, error: "proxy_error", detail: err.message });
    },
  })
);

app.listen(PORT, HOST, () => {
  console.log(`stancer-proxy listening on http://${HOST}:${PORT}`);
});
