FROM haskell:8.10.2-buster as builder

ENV LANG C.UTF-8

################################################################################
# apt-get
################################################################################

# Install required packages.
# RUN apt-get update && apt-get install -y \
#       xz-utils gcc libgmp-dev zlib1g-dev \
#       libpq-dev \
#       tree iputils-ping vim-nox \
#       libtinfo-dev \
#       fonts-ipaexfont-gothic libcurl4-openssl-dev

# Remove apt caches to reduce the size of our container.
# RUN rm -rf /var/lib/apt/lists/*

################################################################################
# Haskell
################################################################################

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

# Build & install application.
COPY LICENSE /opt/name-update-2434/src/
COPY src/hs /opt/name-update-2434/src/src/hs
RUN cabal v2-build \
 && cabal v2-install --installdir=/opt/name-update-2434/bin --install-method=copy

################################################################################
# Dhall
################################################################################

COPY src/dhall /opt/name-update-2434/src/src/dhall

################################################################################
# Run
################################################################################

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/name-update-2434
USER apiuser

# Set the working directory as /opt/name-update-2434/.
# WORKDIR /opt/name-update-2434

CMD /opt/name-update-2434/bin/name-update-api
