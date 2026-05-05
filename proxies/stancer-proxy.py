#!/usr/bin/env python3
import os, base64
from urllib.parse import urljoin
from flask import Flask, request, Response, jsonify
import requests

PORT = int(os.environ.get("PORT", "3031"))
STANCER_BASE = os.environ.get("STANCER_BASE", "https://api.stancer.com/")
PUB = os.environ.get("STANCER_PUBLIC_KEY", "")
SEC = os.environ.get("STANCER_SECRET_KEY", "")
AUTH_MODE = os.environ.get("STANCER_AUTH", "bearer").lower()  # bearer | basic_secret | basic

if not SEC:
    raise SystemExit("Set STANCER_SECRET_KEY")
if not PUB and AUTH_MODE == "basic":
    raise SystemExit("Set STANCER_PUBLIC_KEY for STANCER_AUTH=basic")

if not STANCER_BASE.endswith("/"):
    STANCER_BASE += "/"

def make_auth_header():
    if AUTH_MODE == "bearer":
        return f"Bearer {SEC}"
    elif AUTH_MODE == "basic_secret":
        return "Basic " + base64.b64encode(f"{SEC}:".encode()).decode()
    elif AUTH_MODE == "basic":
        return "Basic " + base64.b64encode(f"{PUB}:{SEC}".encode()).decode()
    else:
        raise SystemExit("Invalid STANCER_AUTH (use: bearer | basic_secret | basic)")

app = Flask(__name__)

@app.route("/_health")
def health():
    return jsonify(ok=True, proxy="stancer", base=STANCER_BASE, auth=AUTH_MODE)

# /stancer/<...> -> https://api.stancer.com/<...>
@app.route("/stancer/<path:p>", methods=["GET","POST","PUT","PATCH","DELETE","HEAD","OPTIONS"])
def proxy(p):
    upstream = urljoin(STANCER_BASE, p)

    # Copy inbound headers except Host and Authorization (we will set our own)
    headers = {k: v for k, v in request.headers.items()
               if k.lower() not in ("host", "authorization")}
    headers["Authorization"] = make_auth_header()
    headers.setdefault("Accept", "application/json")

    try:
        r = requests.request(
            method=request.method,
            url=upstream,
            params=request.args,
            data=request.get_data(),
            headers=headers,
            timeout=30,
            allow_redirects=True,
        )
    except requests.RequestException as e:
        return jsonify(ok=False, error=str(e)), 502

    # Tiny debug line in the nohup log (first 16 chars of auth only)
    try:
        print(f"[stancer-proxy] {request.method} {upstream} -> {r.status_code} (auth={AUTH_MODE}, hdr={headers['Authorization'][:16]}...)")
    except Exception:
        pass

    resp = Response(r.content, status=r.status_code)
    if "Content-Type" in r.headers:
        resp.headers["Content-Type"] = r.headers["Content-Type"]
    if "Location" in r.headers:
        resp.headers["Location"] = r.headers["Location"]
    return resp

if __name__ == "__main__":
    app.run(host="127.0.0.1", port=PORT)
