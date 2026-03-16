# ---- Build Stage ----
FROM erlang:26-alpine AS builder

RUN apk add --no-cache git build-base

WORKDIR /app

# Install rebar3 (version compatible with OTP 26)
RUN curl -fsSL https://github.com/erlang/rebar3/releases/download/3.24.0/rebar3 \
         -o /usr/local/bin/rebar3 \
    && chmod +x /usr/local/bin/rebar3

# Fetch deps first (layer-cached when only src changes)
COPY rebar.config ./
RUN rebar3 deps

# Compile sources
COPY . .
RUN rebar3 as prod release

# ---- Runtime Stage ----
FROM erlang:26-alpine AS runtime

RUN apk add --no-cache ncurses-libs

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/clexical ./

ENV CLEXICAL_HTTP_PORT=8080 \
    CLEXICAL_API_KEY=changeme \
    CLEXICAL_LOG_LEVEL=info \
    CLEXICAL_MNESIA_DIR=/data/mnesia

EXPOSE 8080

VOLUME ["/data/mnesia"]

ENTRYPOINT ["/app/bin/clexical"]
CMD ["foreground"]
