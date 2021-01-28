
################################################################################
# Dependency
################################################################################

FROM haskell:8.10.2-buster as dependency

ENV LANG C.UTF-8

# apt-get
#########

RUN apt-get update && apt-get install -y \
    postgresql-11 \
    postgresql-server-dev-11

# Haskell
#########

# Create bin dir
RUN mkdir -p /opt/name-update-2434/bin
ENV PATH "$PATH:/opt/stack/bin"

# Create src dir
RUN mkdir -p /opt/name-update-2434/src
WORKDIR /opt/name-update-2434/src

# Install dependencies
COPY ./name-update.cabal /opt/name-update-2434/src/name-update.cabal
# COPY ./cabal.project.freeze /opt/name-update-2434/src/cabal.project.freeze
# RUN cabal v2-update
RUN cabal update && cabal v2-build --only-dependencies

################################################################################
# Build
################################################################################

FROM dependency as builder

# Build & install application.
COPY LICENSE /opt/name-update-2434/src/
COPY src/hs /opt/name-update-2434/src/src/hs
RUN cabal v2-build \
 && cabal v2-install --installdir=/opt/name-update-2434/bin --install-method=copy

# Dhall
#######

COPY src/dhall /opt/name-update-2434/src/src/dhall

################################################################################
# Run
################################################################################

FROM debian:buster-slim as runner
COPY --from=builder /opt/name-update-2434/ /opt/name-update-2434/
COPY --from=builder /usr/lib/x86_64-linux-gnu/* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /lib/x86_64-linux-gnu/* /lib/x86_64-linux-gnu/
COPY --from=builder /usr/bin/curl /usr/bin/curl
RUN apt-get update && apt-get install curl -y


# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/name-update-2434
USER apiuser

CMD /opt/name-update-2434/bin/name-update-api
