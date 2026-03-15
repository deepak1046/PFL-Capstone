# ─────────────────────────────────────────────────────────────────────────────
# Stage 1: Build
# PIN to exactly 9.6.6 — lts-22.43 requires ghc-9.6.6 precisely.
# Using the floating `haskell:9.6` tag can resolve to 9.6.3/9.6.4 and causes
# the [S-6362] "No compiler found" error at build time.
# ─────────────────────────────────────────────────────────────────────────────
FROM haskell:9.6.6 AS builder

# Install system libs needed to compile Haskell packages
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgmp-dev zlib1g-dev pkg-config ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

# ── Dependency caching trick ──────────────────────────────────────────────────
# Copy ONLY the project description files first so Docker can cache this layer.
# IMPORTANT: Do NOT copy noise-backend.cabal here.
# If both package.yaml and noise-backend.cabal are present, hpack inside the
# image may be an older version than the one that generated the .cabal file,
# causing stack to read the stale .cabal instead of package.yaml → [S-6362].
# We let stack/hpack regenerate the .cabal from package.yaml at build time.
COPY stack.yaml stack.yaml.lock package.yaml ./

# Create stub source files so stack can resolve all transitive deps
# without needing the real source code in this cache layer.
RUN mkdir -p app src && \
    echo 'module Main where\nmain :: IO ()\nmain = return ()' > app/Main.hs && \
    printf 'module Lib where\nsomeFunc :: IO ()\nsomeFunc = return ()' > src/Lib.hs

# Download + compile ONLY dependencies (cached as a separate Docker layer).
# --system-ghc tells stack to use the GHC already in the image (9.6.6).
RUN stack build --only-dependencies --system-ghc 2>&1

# ── Full build ────────────────────────────────────────────────────────────────
# Now copy the real source and dataset.
# Explicitly exclude the pre-generated .cabal to avoid hpack version conflicts.
COPY stack.yaml stack.yaml.lock package.yaml ./
COPY app ./app
COPY src ./src
COPY dataset ./dataset

RUN stack build --copy-bins --local-bin-path /app/bin --system-ghc 2>&1

# ─────────────────────────────────────────────────────────────────────────────
# Stage 2: Minimal runtime image
# ─────────────────────────────────────────────────────────────────────────────
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libgmp10 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy compiled binary from builder stage
COPY --from=builder /app/bin/noise-backend-exe ./noise-backend

# Copy dataset so the app can read it at runtime
COPY --from=builder /build/dataset ./dataset

# Render injects PORT automatically at runtime.
# We set a local fallback of 3000 here; Render will override it.
# The app reads $PORT in API.hs via lookupEnv — this is already correct.
ENV PORT=3000
EXPOSE 3000

CMD ["./noise-backend"]