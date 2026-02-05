FROM haskell:9.6 AS builder

WORKDIR /build

# Copy cabal files first for dependency caching
COPY cabal.project ./
COPY splendor-core/splendor-core.cabal splendor-core/
COPY splendor-ai/splendor-ai.cabal splendor-ai/
COPY splendor-server/splendor-server.cabal splendor-server/

RUN cabal update && cabal build --only-dependencies splendor-server

# Copy source code
COPY splendor-core/ splendor-core/
COPY splendor-ai/ splendor-ai/
COPY splendor-server/ splendor-server/

RUN cabal build splendor-server:exe:splendor-server \
 && cp $(cabal list-bin splendor-server) /build/server

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y libgmp10 libsqlite3-0 ca-certificates \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/server /usr/local/bin/splendor-server

EXPOSE 8080

CMD ["splendor-server"]
