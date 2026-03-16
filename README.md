# Clexical
Event-Driven Backend with HTTP REST API and Permanent Automations

## Overview

Clexical is a lightweight Erlang/OTP backend framework built around a
**letter/predicate dispatch model**. It provides:

- A pluggable **Herald** (input/output layer) — ships with an HTTP/REST herald
  backed by Cowboy
- A pluggable **Scribe** (storage layer) — ships with a Mnesia scribe
- A pluggable **Vassal** (work layer) — ships with a permanent-automation vassal

The three **dispatch verbs** map cleanly to HTTP:

| Clexical verb | HTTP equivalent | Behaviour |
|---|---|---|
| `recite`  | `POST /api/v1/recite`  | Execute verbs in the letter |
| `attend`  | `POST /api/v1/attend`  | Full round-trip: recall + execute |
| `proclaim`| (internal / SSE push)  | Broadcast results to subscribers |

---

## Quick Start

### Docker (recommended)

```bash
# Copy and customise the env file
cp .env.example .env

# Build and start
docker compose up -d

# Confirm healthy
curl http://localhost:8080/health
```

### Local development (rebar3 + OTP 26)

```bash
rebar3 deps
rebar3 shell
```

---

## Configuration

All configuration can be supplied via `config/sys.config` or environment variables:

| Env var | Default | Description |
|---|---|---|
| `CLEXICAL_HTTP_PORT` | `8080` | HTTP listen port |
| `CLEXICAL_API_KEY`   | `changeme` | API key for Bearer / X-Api-Key auth |
| `CLEXICAL_MNESIA_DIR`| `/tmp/clexical_mnesia` | Mnesia data directory |
| `CLEXICAL_LOG_LEVEL` | `info` | Log level |

---

## REST API

All `/api/*` routes require authentication:

```
X-Api-Key: <key>
# or
Authorization: Bearer <key>
# or
?api_key=<key>
```

### Health

```
GET /health
```
No authentication required.

---

### Letters

#### Submit a decree (store + execute)

```
POST /api/v1/letters
Content-Type: application/json

{
  "subject": "inventory",
  "author":  "api-client",
  "type":    "decree",
  "predicates": [
    {
      "action_type": "preposition",
      "action":      "onOffer",
      "id":          "sku-42",
      "adjectives":  { "price": "9.99" }
    },
    {
      "action_type": "verb",
      "action":      "syncPrices",
      "id":          "job-1"
    }
  ]
}
```

Returns `202 Accepted`.

#### Query stored predicates

```
GET /api/v1/letters?subject=inventory&action=onOffer&id=sku-42
```

---

### Attend (full round-trip)

```
POST /api/v1/attend
Content-Type: application/json
{ ... letter JSON ... }
```

Triggers `hear` (recall from Scribe) then `proclaim` (notify subscribers).

---

### Recite (execute-only)

```
POST /api/v1/recite
Content-Type: application/json
{ ... letter JSON ... }
```

Runs verb predicates without the recall phase.

---

### Permanent Automations

Automations are persisted in Mnesia and survive restarts.

#### List automations

```
GET /api/v1/automations
```

#### Register an automation

```
POST /api/v1/automations
Content-Type: application/json

{
  "id":   "price-sync-hourly",
  "name": "Hourly price sync",
  "schedule": { "type": "interval", "ms": 3600000 },
  "letter": {
    "subject": "inventory",
    "author":  "scheduler",
    "type":    "decree",
    "predicates": [
      { "action_type": "verb", "action": "syncPrices" }
    ]
  }
}
```

Schedule types:
- `{ "type": "interval", "ms": N }` — fire every N milliseconds
- `{ "type": "once", "at": EpochMs }` — fire once at an epoch timestamp (ms)

#### Remove an automation

```
DELETE /api/v1/automations/price-sync-hourly
```

---

## Architecture

```
HTTP Client
    │
    ▼
http_herald (cowboy)
    │  letter_from_binary / to_binary
    ▼
clexical (gen_server)
    │
    ├── pronounce → refrain → mnesia_scribe (prepositions)
    │
    └── pronounce → say → automation_vassal (verbs)
                              │
                              └── dispatch #letter{} back into clexical
```

---

## Extending

Implement any of the three behaviours to plug in your own logic:

```erlang
-behaviour(herald).   % protocol / transport layer
-behaviour(scribe).   % storage layer
-behaviour(vassal).   % work / execution layer
```
