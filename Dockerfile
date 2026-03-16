# ---- Build Stage ----
FROM erlang:26-alpine AS builder

RUN apk add --no-cache git build-base

WORKDIR /app

# Install rebar3
RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 \
    && chmod +x /usr/local/bin/rebar3

# Copy dependency specs first for layer caching
COPY rebar.config rebar.lock* ./
RUN rebar3 deps

# Copy source
COPY . .

# Build release
RUN rebar3 as prod release

# ---- Runtime Stage ----
FROM erlang:26-alpine AS runtime

RUN apk add --no-cache libstdc++ ncurses-libs

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/clexical ./

# Default configuration can be overridden via environment variables
ENV CLEXICAL_HTTP_PORT=8080 \
    CLEXICAL_API_KEY=changeme \
    CLEXICAL_LOG_LEVEL=info

EXPOSE 8080

ENTRYPOINT ["/app/bin/clexical"]
CMD ["foreground"]
