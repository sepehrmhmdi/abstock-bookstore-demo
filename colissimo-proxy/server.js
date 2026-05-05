// server.js
import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import fetch from 'node-fetch';

const app = express();
app.get('/_health', (req,res)=>res.json({ok:true}));
app.use((req, res, next) => {
  res.setHeader('X-Frame-Options', 'ALLOWALL');
  res.setHeader('Content-Security-Policy', "frame-ancestors *");
  next();
});
app.use(cors({ origin: true }));
app.use(express.static('public'));

const APIKEY = process.env.COLI_APIKEY;

if (!APIKEY) {
  throw new Error("Missing COLI_APIKEY environment variable");
}
const PARTNER = process.env.COLI_PARTNER || "000000";       // code tiers partenaire
const PORT    = process.env.PORT || 3032;

// 1) Générer un token (widget v2)
app.get('/api/widget-token', async (req, res) => {
  try {
    const body = { apikey: APIKEY };
    if (PARTNER) body.partnerClientCode = PARTNER;

    const r = await fetch('https://ws.colissimo.fr/widget-colissimo/rest/authenticate.rest', {
      method: 'POST',
      headers: { 'Content-Type':'application/json' },
      body: JSON.stringify(body),
    });
    if (!r.ok) {
      const text = await r.text();
      return res.status(502).json({ error:'auth-failed', status:r.status, text });
    }
    const json = await r.json();
    return res.json(json); // { token: "..." }
  } catch (e) {
    return res.status(500).json({ error:'token-exception', detail:String(e) });
  }
});

// 2) Page /map qui ouvre le widget avec le token
app.get('/map', (req, res) => {
  const address      = (req.query.address     || '').toString();
  const zipCode      = (req.query.zipCode     || '').toString();
  const city         = (req.query.city        || '').toString().toUpperCase();
  const countryCode  = (req.query.countryCode || 'FR').toString().toUpperCase();
  const filterRelay  = (req.query.filterRelay || '1').toString();

  // HTML minimal qui charge le widget officiel (doc 2025)
  res.type('html').send(`<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <!-- jQuery + Mapbox + Widget Colissimo (doc 2025) -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.0/jquery.js"></script>
  <script src="https://api.mapbox.com/mapbox-gl-js/v2.6.1/mapbox-gl.js"></script>
  <link  href="https://api.mapbox.com/mapbox-gl-js/v2.6.1/mapbox-gl.css" rel="stylesheet" />
  <script src="https://ws.colissimo.fr/widget-colissimo/js/jquery.plugin.colissimo.min.js"></script>
  <style>html,body{margin:0;padding:0;height:100%}#widget{height:100vh}</style>
</head>
<body>
  <div id="widget"></div>
  <script>
    // callback quand l'utilisateur valide un point
    function onPickupSelected(point){
      // on renvoie la sélection au parent
      const parentOrigin = document.referrer ? new URL(document.referrer).origin : '*';
      parent.postMessage(
        { type:'colissimo:pudo:selected',
          id: point.identifiant,
          label: point.nom + ', ' + (point.adresse1||'') + ', ' + (point.codePostal||'') + ' ' + (point.localite||'')
        },
        '*' // parent is http://localhost:8989
      );
      // fermeture propre
      jQuery('#widget').frameColissimoClose();
    }

    // récupérer le token côté back pour ouvrir le widget
    fetch('/api/widget-token').then(r=>r.json()).then(({token})=>{
      if(!token){ document.body.innerHTML='<p>Impossible d\\'obtenir un token.</p>'; return; }
      const url_serveur = 'https://ws.colissimo.fr';
      jQuery('#widget').frameColissimoOpen({
        URLColissimo : url_serveur,
        callBackFrame: 'onPickupSelected',
        ceCountry    : ${JSON.stringify(countryCode)},
        ceAddress    : ${JSON.stringify(address)},
        ceZipCode    : ${JSON.stringify(zipCode)},
        ceTown       : ${JSON.stringify(city)},
        origin       : 'WIDGET',
        filterRelay  : ${JSON.stringify(filterRelay)},
        token
      });
    }).catch(e=>{
      document.body.innerHTML='<pre>Erreur token: '+String(e)+'</pre>';
    });
  </script>
</body>
</html>`);
});

app.listen(PORT, () => console.log('Colissimo proxy on :' + PORT));
