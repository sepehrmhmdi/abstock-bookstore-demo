#!/usr/bin/env bash
set -euo pipefail

BASE=${BASE:-http://127.0.0.1:8989}
COOKIE=${COOKIE:-/tmp/ab.ck}
REF="dev-$(date +%s)"

pick_id() {
  # try selection first
  local id
  id=$(curl -s "$BASE/api/v1/selection.json" \
    | node -e 'let d="";process.stdin.on("data",c=>d+=c).on("end",()=>{try{let j=JSON.parse(d); if(Array.isArray(j)&&j[0]&&j[0].id) process.stdout.write(String(j[0].id));}catch(e){}})')
  # fallback: search
  if [ -z "$id" ]; then
    id=$(curl -s "$BASE/api/v1/search.json?query=a&all=1" \
      | node -e 'let d="";process.stdin.on("data",c=>d+=c).on("end",()=>{try{let j=JSON.parse(d); if(Array.isArray(j)&&j[0]&&j[0].id) process.stdout.write(String(j[0].id));}catch(e){}})')
  fi
  echo "$id"
}

ID=$(pick_id)
[ -z "$ID" ] && { echo "No product id found. Add some products first."; exit 1; }
echo "Using product id: $ID"

# add to cart (keep cookies)
curl -c "$COOKIE" -b "$COOKIE" -s -X POST "$BASE/api/cart/add" -d "id=$ID" >/dev/null

# hit payment route; capture 302 Location (the checkout URL)
loc=$(curl -s -D - -b "$COOKIE" "$BASE/paiement/carte?ref=$REF" -o /dev/null \
      | awk -v IGNORECASE=1 '/^Location:/ {sub(/\r$/,""); print substr($0,11)}')

[ -z "$loc" ] && { echo "No redirect found; check app logs."; exit 2; }

echo "Checkout URL:"
echo "$loc"
