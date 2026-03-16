%%
%% clexical_webhooks.hrl — Records for inbound and outbound webhook support.
%%
%% INBOUND WEBHOOKS
%%   A #webhook_reg{} registers a named inbound endpoint.
%%   External systems POST or GET /api/v1/webhooks/in/:name and the request
%%   is translated into a #letter{} and dispatched into the clexical engine.
%%
%% OUTBOUND WEBHOOKS (do:webhook action predicate)
%%   Adjectives expected on a do:webhook predicate:
%%
%%     url      (binary)  — target URL (required)
%%     method   (binary)  — "GET" | "POST"  (default: "POST")
%%     headers  (map)     — extra request headers
%%     body     (binary)  — request body template (Mustache, POST only)
%%     timeout  (integer) — ms, default 5000
%%     retries  (integer) — max attempts, default 1
%%     store_as (binary)  — adjective key to store response body in ctx
%%
%% HMAC verification (inbound)
%%   If secret is set on a #webhook_reg{}, the request body must be signed
%%   with HMAC-SHA256 and the signature supplied in X-Webhook-Signature
%%   as "sha256=<hex>".
%%

%% Registered inbound endpoint.
-record(webhook_reg, {
    id          :: binary(),            %% unique name / slug
    description = <<>> :: binary(),
    secret      = undefined :: binary() | undefined,  %% HMAC secret
    letter_template :: map(),           %% letter JSON template (may use {{field}} placeholders)
    created_at  :: pos_integer(),
    enabled     = true :: boolean()
}).
